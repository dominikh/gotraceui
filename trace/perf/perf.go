package main

// XXX this package shouldn't be in the trace folder. perf isn't tracing.
// XXX eliminate all panics
// XXX bounds check everything

import (
	"bytes"
	"debug/elf"
	"debug/gosym"
	"fmt"
	"log"
	"math/bits"
	"os"
	"strings"
	"time"
	"unsafe"

	"honnef.co/go/gotraceui/container"
	myunsafe "honnef.co/go/gotraceui/unsafe"
)

type hdrType uint8

//go:generate stringer -trimprefix "hdr" -type hdrType -linecomment
const (
	hdrTracingData    hdrType = 1  // tracing data
	hdrBuildID        hdrType = 2  // build id
	hdrHostname       hdrType = 3  // hostname
	hdrOSRelease      hdrType = 4  // OS release
	hdrVersion        hdrType = 5  // version
	hdrArch           hdrType = 6  // arch
	hdrNrCPUs         hdrType = 7  // num CPUs
	hdrCPUDesc        hdrType = 8  // CPU desc
	hdrCPUID          hdrType = 9  // CPUID
	hdrTotalMem       hdrType = 10 // total mem
	hdrCmdline        hdrType = 11 // cmdline
	hdrEventDesc      hdrType = 12 // event desc
	hdrCPUTopology    hdrType = 13 // CPU topology
	hdrNUMATopology   hdrType = 14 // NUMA topology
	hdrBranchStack    hdrType = 15 // branch stack
	hdrPMUMappings    hdrType = 16 // PMU mappings
	hdrGroupDesc      hdrType = 17 // group desc
	hdrAuxTrace       hdrType = 18 // aux
	hdrStat           hdrType = 19 // stat
	hdrCache          hdrType = 20 // cache
	hdrSampleTime     hdrType = 21 // sample time
	hdrSampleTopology hdrType = 22 // sample topology
	hdrClockID        hdrType = 23 // clock ID
	hdrDirFormat      hdrType = 24 // dir format
	hdrBPFProgInfo    hdrType = 25 // BPF prog info
	hdrBPFBTF         hdrType = 26 // BPF BTF
	hdrCompressed     hdrType = 27 // compressed
	hdrCPUPMUCaps     hdrType = 28 // CPU PMU caps
	hdrClockData      hdrType = 29 // clock data
	hdrHybridTopology hdrType = 30 // hybrid topology
	hdrPMUCaps        hdrType = 31 // PMU caps

	hdrMax hdrType = 32
)

type flags [4]uint64

func (fl flags) bit(x hdrType) bool {
	if x < 0 {
		return false
	} else if x <= 63 {
		return fl[0]&(1<<x) != 0
	} else if x <= 127 {
		return fl[1]&(1<<(x-64)) != 0
	} else if x <= 191 {
		return fl[1]&(1<<(x-128)) != 0
	} else if x <= 255 {
		return fl[1]&(1<<(x-192)) != 0
	} else {
		return false
	}
}

func (fl flags) String() string {
	// XXX include unknown bits in the output
	ss := make([]string, 0, hdrMax)
	for i := hdrType(0); i < hdrMax; i++ {
		if fl.bit(i) {
			ss = append(ss, i.String())
		}
	}
	return strings.Join(ss, " | ")
}

type header struct {
	magic    [8]byte
	size     uint64
	attrSize uint64
	// documentation claims that this points to an array of eventAttr, each sized attrSize, but it's really an
	// array of fileAttr
	attrs fileSection
	data  fileSection
	// eventTypes has been unused since 2013.
	eventTypes fileSection
	flags      flags
}

type fileSection struct {
	offset uint64
	size   uint64
}

func (s fileSection) String() string {
	return fmt.Sprintf("section{@%d; +%d}", s.offset, s.size)
}

type attrFlags uint64

type fileEventAttr struct {
	eventAttr
	ids fileSection
}

func (f *File) EventAttrIDs(attr *fileEventAttr) []uint64 {
	if attr.ids.size == 0 {
		return nil
	}
	return myunsafe.SliceCast[[]uint64](f.data[attr.ids.offset : attr.ids.offset+attr.ids.size])
}

type eventAttr struct {
	typ                     eventType
	size                    uint32
	config                  uint64
	samplePeriodOrFreq      uint64
	sampleType              SampleType
	readFormat              uint64
	flags                   attrFlags
	wakeupEventsOrWatermark uint32
	bpType                  uint32
	bp                      bp
	branchSampleType        uint64
	sampleRegsUser          uint64
	sampleStackUser         uint32
	clockID                 int32
	sampleRegsIntr          uint64
	auxWatermark            uint32
	sampleMaxStack          uint16
	_                       uint16
	auxSampleSize           uint32
	_                       uint32
	sigData                 uint64
	config3                 uint64
}

type eventType uint32

const (
	typeHardware   eventType = 0
	typeSoftware   eventType = 1
	typeTracepoint eventType = 2
	typeHWCache    eventType = 3
	typeRaw        eventType = 4
	typeBreakpoint eventType = 5
	typeMax        eventType = 6
)

func (typ eventType) String() string {
	switch typ {
	case typeHardware:
		return "hardware"
	case typeSoftware:
		return "software"
	case typeTracepoint:
		return "tracepoint"
	case typeHWCache:
		return "hw cache"
	case typeRaw:
		return "raw"
	case typeBreakpoint:
		return "breakpoint"
	default:
		return fmt.Sprintf("%d", typ)
	}
}

type bp struct {
	// bp_addr or kprobe_func or uprobe_path or config1
	a uint64
	// bp_len or kprobe_addr or kprobe_offset or config2
	b uint64
}

type File struct {
	// mapping from header type to offset in data where fileSection starts. Zero indicates absence of a
	// section.
	header   *header
	sections [hdrMax]uint64
	data     []byte

	// Map from event IDs to event descriptions
	eventAttrs map[uint64]*fileEventAttr
	// Map from record ID to file offset of its event header
	recordOffsets []uint64

	sharedType   container.Option[SampleType]
	sharedOffset container.Option[uint64]
}

func (f *File) EventAttrs() []fileEventAttr {
	return myunsafe.SliceCast[[]fileEventAttr](f.data[f.header.attrs.offset:][:f.header.attrs.size])
}

func (f *File) Hostname() string  { return f.strSection(hdrHostname) }
func (f *File) OSRelease() string { return f.strSection(hdrOSRelease) }
func (f *File) Version() string   { return f.strSection(hdrVersion) }
func (f *File) Arch() string      { return f.strSection(hdrArch) }
func (f *File) CPUDesc() string   { return f.strSection(hdrCPUDesc) }
func (f *File) CPUID() string     { return f.strSection(hdrCPUID) }

func (f *File) Clock() *Clock {
	if f.sections[hdrClockID] == 0 {
		return nil
	}
	s := f.section(hdrClockData)
	return myunsafe.Map[Clock](f.data[s.offset:])
}

type Clock struct {
	Version uint32
	ID      uint32
	Wall    uint64
	Clock   uint64
}

func (c *Clock) Time() time.Time {
	return time.Unix(0, int64(c.Wall))
}

func (f *File) section(section hdrType) *fileSection {
	return myunsafe.Map[fileSection](f.data[f.sections[section]:])
}

func (f *File) strSection(section hdrType) string {
	if f.sections[section] == 0 {
		return ""
	}
	s := f.section(section)
	return f.str(s.offset)
}

func (f *File) str(offset uint64) string {
	n := *myunsafe.Map[uint32](f.data[offset:])
	if n == 0 {
		return ""
	}
	str := f.data[offset+4 : offset+4+uint64(n)]
	// XXX verify there's a zero
	off := bytes.IndexByte(str, 0)
	return unsafe.String(&str[0], off)
}

func NewFile(data []byte) *File {
	f := File{
		header:     myunsafe.Map[header](data),
		data:       data,
		eventAttrs: make(map[uint64]*fileEventAttr),
	}

	hdr := f.header
	if hdr.attrSize != uint64(unsafe.Sizeof(fileEventAttr{})) {
		panic("unsupported")
	}

	// Index all of the optional sections
	off := hdr.data.offset + hdr.data.size
	for i := hdrType(0); i < hdrMax; i++ {
		if hdr.flags.bit(i) {
			f.sections[i] = off
			off += uint64(unsafe.Sizeof(fileSection{}))
		}
	}

	// Figure out how to reliably determine the types of samples. When there are multiple event sources in a
	// single file, then different sources may have different sample types, i.e. have different sets of
	// fields. If they do have different types, then we must look them up before parsing. For this, we need
	// the samples' event IDs. If all sample_types include SampleTypeIdentifier, then the IDs are the first
	// field in the sample. If all sample_types include SampleTypeID, and the ID is always at the same offset,
	// then that's the offset we'll use. In all other cases we cannot reliably parse the file.
	attrs := f.EventAttrs()
	if len(attrs) > 0 {
		allHaveIdentifier := attrs[0].sampleType&SampleTypeIdentifier != 0
		allHaveID := attrs[0].sampleType&SampleTypeID != 0
		allShareType := true
		for i := range attrs {
			attr := &attrs[i]
			typ := attr.sampleType
			if typ != attrs[0].sampleType {
				allShareType = false
			}
			if typ&SampleTypeIdentifier == 0 {
				allHaveIdentifier = false
			}
			if typ&SampleTypeID == 0 {
				allHaveID = false
			}

			for _, id := range f.EventAttrIDs(attr) {
				f.eventAttrs[id] = attr
			}
		}

		idOffset := func(typ SampleType) uint64 {
			set := bits.OnesCount64(uint64(typ & (SampleTypeIdentifier | SampleTypeIP | SampleTypeTID | SampleTypeTime | SampleTypeAddr)))
			return uint64(set) * 8
		}

		if allShareType {
			f.sharedType = container.Some(attrs[0].sampleType)
		} else {
			if allHaveIdentifier {
				f.sharedOffset = container.Some[uint64](0)
			} else if allHaveID {
				sharedOffset := idOffset(attrs[0].sampleType)
				for i := range attrs {
					if off := idOffset(attrs[i].sampleType); off != sharedOffset {
						panic("multiple events but no reliable identifier to identify them")
					}
				}
				f.sharedOffset = container.Some(sharedOffset)
			} else {
				panic("multiple events but no reliable identifier to identify them")
			}
		}
	}

	for off := hdr.data.offset; off < hdr.data.offset+hdr.data.size; {
		f.recordOffsets = append(f.recordOffsets, off)
		h := myunsafe.Map[EventHeader](data[off:])
		off += uint64(h.size)
	}

	return &f
}

func (f *File) NumRecords() int {
	return len(f.recordOffsets)
}

func (f *File) Record(idx int) Record {
	b := f.data[f.recordOffsets[idx]:]
	h := myunsafe.Map[EventHeader](b)
	sz := h.size
	b = b[:sz]
	var typ SampleType

	if h.typ == EventTypeSample {
		if t, ok := f.sharedType.Get(); ok {
			typ = t
		} else {
			off := f.sharedOffset.MustGet()
			off += uint64(unsafe.Sizeof(EventHeader{}))
			id := *myunsafe.Map[uint64](f.data[off:])
			typ = f.eventAttrs[id].sampleType
		}
	}
	return Record{
		sampleType: typ,
		data:       b,
	}
}

type Record struct {
	sampleType SampleType
	data       []byte
}

type Comm struct {
	PID  uint32
	TID  uint32
	Name string
}

func (c Comm) String() string {
	return fmt.Sprintf("PERF_RECORD_COMM: %s:%d/%d", c.Name, c.PID, c.TID)
}

func (r Record) Comm() Comm {
	name := unsafe.String(&r.data[16], len(r.data)-16)
	name = name[:strings.IndexByte(name, 0)]
	return Comm{
		PID:  *myunsafe.Map[uint32](r.data[8:12]),
		TID:  *myunsafe.Map[uint32](r.data[12:16]),
		Name: name,
	}
}

type Exit struct {
	PID  uint32
	PPID uint32
	TID  uint32
	PTID uint32
	Time uint64
}

func (r Record) Exit() Exit {
	return *myunsafe.Map[Exit](r.data[8:])
}

type Mmap2 struct {
	PID             uint32
	TID             uint32
	Addr            uint64
	Len             uint64
	PageOffset      uint64
	Major           uint32
	Minor           uint32
	Inode           uint64
	InodeGeneration uint64
	Protection      uint32
	Flags           uint32
	Filename        string
}

const (
	PROT_EXEC      = 0x4
	PROT_GROWSDOWN = 0x1000000
	PROT_GROWSUP   = 0x2000000
	PROT_NONE      = 0x0
	PROT_READ      = 0x1
	PROT_WRITE     = 0x2

	MAP_PRIVATE = 0x2
	MAP_SHARED  = 0x1

	PERF_SAMPLE_REGS_ABI_NONE = 0x0
	PERF_SAMPLE_REGS_ABI_32   = 0x1
	PERF_SAMPLE_REGS_ABI_64   = 0x2
)

func (m *Mmap2) String() string {
	prot := m.Protection
	flags := m.Flags
	prots := [4]byte{'-', '-', '-', '-'}
	if prot&PROT_READ != 0 {
		prots[0] = 'r'
	}
	if prot&PROT_WRITE != 0 {
		prots[1] = 'w'
	}
	if prot&PROT_EXEC != 0 {
		prots[2] = 'x'
	}
	if flags == MAP_SHARED {
		prots[3] = 's'
	} else if flags == MAP_PRIVATE {
		prots[3] = 'p'
	} else {
		prots[3] = '?'
	}
	return fmt.Sprintf("PERF_RECORD_MMAP2 %d/%d: [0x%x(0x%x) @ %d %02x:%02x %d %d]: %s %s",
		m.PID, m.TID, m.Addr, m.Len, m.PageOffset, m.Major, m.Minor, m.Inode, m.InodeGeneration, prots, m.Filename)
}

func (r Record) Mmap2(m *Mmap2) {
	*m = *myunsafe.Map[Mmap2](r.data[8:])
	name := r.data[8+64:]
	name = name[:bytes.IndexByte(name, 0)]
	m.Filename = unsafe.String(&name[0], len(name))
}

type ThreadMap struct {
	PID  uint64
	Name [16]byte
}

type ThreadMaps []ThreadMap

func (tm ThreadMaps) String() string {
	var ss []string
	for _, m := range tm {
		name := m.Name[:]
		name = name[:bytes.IndexByte(name, 0)]
		ss = append(ss, fmt.Sprintf(".... %d %s", m.PID, name))
	}
	s := strings.Join(ss, "\n")
	return fmt.Sprintf("PERF_RECORD_THREAD_MAP nr: %d threads:\n%s", len(tm), s)
}

func (r Record) ThreadMap() ThreadMaps {
	n := *myunsafe.Map[uint64](r.data[8:])
	return myunsafe.SliceCast[ThreadMaps](r.data[16 : 16+n*uint64(unsafe.Sizeof(ThreadMap{}))])
}

type Ksymbol struct {
	Addr  uint64
	Len   uint32
	Type  uint16
	Flags uint16
	Name  string
}

func (sym Ksymbol) String() string {
	return fmt.Sprintf("PERF_RECORD_KSYMBOL addr %d len %d type %d flags 0x%x name %s",
		sym.Addr, sym.Len, sym.Type, sym.Flags, sym.Name)
}

func (r Record) Ksymbol() Ksymbol {
	name := r.data[24:]
	name = name[:bytes.IndexByte(name, 0)]
	sym := Ksymbol{
		Addr:  *myunsafe.Map[uint64](r.data[8:]),
		Len:   *myunsafe.Map[uint32](r.data[16:]),
		Type:  *myunsafe.Map[uint16](r.data[20:]),
		Flags: *myunsafe.Map[uint16](r.data[22:]),
		Name:  unsafe.String(&name[0], len(name)),
	}
	return sym
}

// [ ] EventTypeMmap
// [ ] EventTypeLost
// [X] EventTypeComm
// [X] EventTypeExit
// [ ] EventTypeThrottle
// [ ] EventTypeUnthrottle
// [ ] EventTypeFork
// [ ] EventTypeRead
// [X] EventTypeSample
// [X] EventTypeMmap2
// [ ] EventTypeAux
// [ ] EventTypeItraceStart
// [ ] EventTypeLostSamples
// [ ] EventTypeSwitch
// [ ] EventTypeSwitchCPUWide
// [ ] EventTypeNamespaces
// [X] EventTypeKsymbol
// [ ] EventTypeBPFEvent
// [ ] EventTypeCgroup
// [ ] EventTypeTextPoke
// [ ] EventTypeAuxOutputHWID
//
// [ ] EventTypeHeaderAttr
// [ ] EventTypeHeaderEventType
// [ ] EventTypeHeaderTracingData
// [ ] EventTypeHeaderBuildID
// [X] EventTypeFinishedRound
// [ ] EventTypeIDIndex
// [ ] EventTypeAuxtraceInfo
// [ ] EventTypeAuxtrace
// [ ] EventTypeAuxtraceError
// [X] EventTypeThreadMap
// [ ] EventTypeCPUMap
// [ ] EventTypeStatConfig
// [ ] EventTypeStat
// [ ] EventTypeStatRound
// [ ] EventTypeEventUpdate
// [X] EventTypeTimeConv
// [ ] EventTypeHeaderFeature
// [ ] EventTypeCompressed
// [X] EventTypeFinishedInit

type Registers struct {
	Mask      uint64
	Registers [64]uint64
}

type ReadFormat struct {
	// XXX
}

type BranchEntry struct {
	// XXX
}

type Sample struct {
	ID       container.Option[uint64]
	IP       container.Option[uint64]
	PID      container.Option[uint32]
	TID      container.Option[uint32]
	Time     container.Option[uint64]
	Addr     container.Option[uint64]
	StreamID container.Option[uint64]
	CPU      container.Option[uint32]
	Res      container.Option[uint32]
	Period   container.Option[uint64]
	V        container.Option[ReadFormat]
	IPs      container.Option[[]uint64]
	Raw      container.Option[[]byte]
	Lbr      container.Option[[]BranchEntry]

	// XXX ABIUser and RegsUser should be a single Option
	ABIUser     container.Option[uint64]
	RegsUser    container.Option[Registers]
	Stack       container.Option[[]byte]
	Weight      container.Option[uint64]
	DataSrc     container.Option[uint64]
	Transaction container.Option[uint64]
	// XXX ABIIntr and RegsIntr should be a single Option
	ABIIntr      container.Option[uint64]
	RegsIntr     container.Option[Registers]
	PhysAddr     container.Option[uint64]
	Cgroup       container.Option[uint64]
	DataPageSize container.Option[uint64]
	CodePageSize container.Option[uint64]
	Aux          container.Option[[]byte]
}

func (r Record) Sample(f *File, s *Sample) {
	off := uint64(unsafe.Sizeof(EventHeader{}))
	u64 := func() uint64 {
		ret := *myunsafe.Map[uint64](r.data[off:])
		off += 8
		return ret
	}
	u32 := func() uint32 {
		ret := *myunsafe.Map[uint32](r.data[off:])
		off += 4
		return ret
	}
	typ := r.sampleType
	fmt.Println()
	fmt.Println(typ)
	if typ&SampleTypeIdentifier != 0 {
		s.ID = container.Some(u64())
	}
	if typ&SampleTypeIP != 0 {
		s.IP = container.Some(u64())
	}
	if typ&SampleTypeTID != 0 {
		s.PID, s.TID = container.Some(u32()), container.Some(u32())
	}
	if typ&SampleTypeTime != 0 {
		s.Time = container.Some(u64())
	}
	if typ&SampleTypeAddr != 0 {
		s.Addr = container.Some(u64())
	}
	if typ&SampleTypeID != 0 {
		s.ID = container.Some(u64())
	}
	if typ&SampleTypeStreamID != 0 {
		s.StreamID = container.Some(u64())
	}
	if typ&SampleTypeCPU != 0 {
		s.CPU, s.Res = container.Some(u32()), container.Some(u32())
	}
	if typ&SampleTypePeriod != 0 {
		s.Period = container.Some(u64())
	}
	if typ&SampleTypeRead != 0 {
		// XXX
	}
	if typ&SampleTypeCallchain != 0 {
		n := u64()
		s.IPs = container.Some(myunsafe.SliceCast[[]uint64](r.data[off : off+(n*8)]))
		off += n * 8
	}
	if typ&SampleTypeRaw != 0 {
		n := uint64(u32())
		s.Raw = container.Some(r.data[off : off+n])
		off += n
	}
	if typ&SampleTypeBranchStack != 0 {
		// XXX
	}
	regs := func(mask uint64) Registers {
		weight := bits.OnesCount64(mask)
		regs := myunsafe.SliceCast[[]uint64](r.data[off : off+uint64(weight)*8])
		out := Registers{
			Mask: mask,
		}
		for i, j := 0, 0; i < 64; i++ {
			if mask&(1<<i) != 0 {
				out.Registers[i] = regs[j]
				j++
			}
		}
		off += uint64(weight) * 8
		return out
	}
	if typ&SampleTypeRegsUser != 0 {
		abi := u64()
		s.ABIUser = container.Some(abi)
		if abi != 0 {
			if id, ok := s.ID.Get(); ok {
				attr := f.eventAttrs[id]
				mask := attr.sampleRegsUser
				s.RegsUser = container.Some(regs(mask))
			} else {
				// XXX if there is only one event type, then we don't need the ID
				panic("no id")
			}
		}
	}
	if typ&SampleTypeStackUser != 0 {
		// The first size is the full, padded size of the stack data.
		n := u64()
		if n > 0 {
			off2 := off
			off += n
			// The second size is the actual size of the stack data, with n2 <= n1.
			n = u64()
			s.Stack = container.Some(r.data[off2 : off2+n])
		}
	}
	if typ&SampleTypeWeight != 0 {
		s.Weight = container.Some(u64())
	} else if typ&SampleTypeWeightStruct != 0 {
		// TODO support the struct
		s.Weight = container.Some(u64())
	}
	if typ&SampleTypeDataSrc != 0 {
		s.DataSrc = container.Some(u64())
	}
	if typ&SampleTypeTransaction != 0 {
		s.Transaction = container.Some(u64())
	}
	if typ&SampleTypeRegsIntr != 0 {
		abi := u64()
		s.ABIIntr = container.Some(abi)
		if abi != 0 {
			if id, ok := s.ID.Get(); ok {
				attr := f.eventAttrs[id]
				mask := attr.sampleRegsIntr
				s.RegsIntr = container.Some(regs(mask))
			} else {
				// XXX if there is only one event type, then we don't need the ID
				panic("no id")
			}
		}
	}
	if typ&SampleTypePhysAddr != 0 {
		s.PhysAddr = container.Some(u64())
	}
	if typ&SampleTypeCgroup != 0 {
		s.Cgroup = container.Some(u64())
	}
	if typ&SampleTypeDataPageSize != 0 {
		s.DataPageSize = container.Some(u64())
	}
	if typ&SampleTypeCodePageSize != 0 {
		s.CodePageSize = container.Some(u64())
	}
	if typ&SampleTypeAux != 0 {
		n := u64()
		s.Aux = container.Some(r.data[off : off+n])
		off += n
	}
}

type TimeConv struct {
	TimeShift        uint64
	TimeMult         uint64
	TimeZero         uint64
	TimeCycles       uint64
	TimeMask         uint64
	CapUserTimeZero  uint8
	CapUserTimeShort uint8
	_                uint8
}

func (tc *TimeConv) String() string {
	f := `PERF_RECORD_TIME_CONV
... Time Shift      %d
... Time Muliplier  %d
... Time Zero       %d
... Time Cycles     %d
... Time Mask       %d
... Cap Time Zero   %d
... Cap Time Short  %d`
	return fmt.Sprintf(f, tc.TimeShift, tc.TimeMult, tc.TimeZero, tc.TimeCycles, tc.TimeMask, tc.CapUserTimeZero, tc.CapUserTimeShort)
}

func (r Record) TimeConv() *TimeConv {
	return myunsafe.Map[TimeConv](r.data[8:])
}

func (r Record) Header() *EventHeader {
	return myunsafe.Map[EventHeader](r.data)
}

func printSample(s *Sample) {
	f := `ID: %s
IP: %s
PID: %s
TID: %s
Time: %s
Addr: %s
StreamID: %s
CPU: %s
Res: %s
Period: %s
V: %s
IPs: %s
Raw: %s
Lbr: %s
ABIUser: %s
RegsUser: %s
Stack: %s
Weight: %s
DataSrc: %s
Transaction: %s
ABIIntr: %s
RegsIntr: %s
PhysAddr: %s
Cgroup: %s
DataPageSize: %s
CodePageSize: %s
Aux: %s
`

	fmt.Printf(f,
		s.ID, s.IP, s.PID, s.TID, s.Time, s.Addr, s.StreamID, s.CPU, s.Res, s.Period, s.V, s.IPs,
		s.Raw, s.Lbr, s.ABIUser, s.RegsUser, s.Stack, s.Weight, s.DataSrc, s.Transaction, s.ABIIntr, s.RegsIntr,
		s.PhysAddr, s.Cgroup, s.DataPageSize, s.CodePageSize, s.Aux,
	)
}

func elfStuff() {
	f, err := elf.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	// XXX check the sections exist
	pclntab := f.Section(".gopclntab")
	symtab := f.Section(".gosymtab")
	text := f.Section(".text")

	pclntabData, err := pclntab.Data()
	if err != nil {
		log.Fatal(err)
	}
	symtabData, err := symtab.Data()
	if err != nil {
		log.Fatal(err)
	}
	tab, err := gosym.NewTable(symtabData, gosym.NewLineTable(pclntabData, text.Addr))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(tab.PCToLine(4685687))
}

type IDIndexEntry struct {
	ID    uint64
	Index uint64
	CPU   uint64
	TID   uint64
}

type IDIndex []IDIndexEntry

func (index IDIndex) String() string {
	var ss []string
	for _, entry := range index {
		ss = append(ss, fmt.Sprintf("... id: %d idx: %d cpu: %d tid: %d", entry.ID, entry.Index, entry.CPU, entry.TID))
	}
	s := strings.Join(ss, "\n")
	return fmt.Sprintf("PERF_RECORD_ID_INDEX nr: %d\n%s", len(index), s)
}

func (r Record) IDIndex() IDIndex {
	n := *myunsafe.Map[uint64](r.data[8:])
	return myunsafe.SliceCast[IDIndex](r.data[16 : 16+n*uint64(unsafe.Sizeof(IDIndexEntry{}))])
}

func main() {
	// elfStuff()
	// return

	// XXX switch to normal file reads and seeking, as we intend to transform samples to columns
	data, err := os.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	f := NewFile(data)
	var sample Sample
	for i := 0; i < f.NumRecords(); i++ {
		r := f.Record(i)
		switch r.Header().typ {
		case EventTypeComm:
			fmt.Println("comm", r.Comm())
		case EventTypeExit:
			fmt.Println("exit", r.Exit())
		case EventTypeMmap2:
			var m Mmap2
			r.Mmap2(&m)
			fmt.Println(&m)
		case EventTypeSample:
			r.Sample(f, &sample)
			printSample(&sample)
		case EventTypeTimeConv:
			fmt.Println(r.TimeConv())
		case EventTypeThreadMap:
			fmt.Println(r.ThreadMap())
		case EventTypeKsymbol:
			fmt.Println(r.Ksymbol())
		case EventTypeIDIndex:
			fmt.Println(r.IDIndex())
		case EventTypeFinishedInit:
			fmt.Println("PERF_RECORD_FINISHED_INIT")
		case EventTypeFinishedRound:
			fmt.Println("PERF_RECORD_FINISHED_ROUND")
		default:
			fmt.Println(r.Header().typ)
		}
	}
	return

	for _, attr := range f.EventAttrs() {
		sec := f.data[attr.ids.offset : attr.ids.offset+attr.ids.size]
		fmt.Println("attr:", attr.typ, attr.sampleType)
		fmt.Println(myunsafe.SliceCast[[]uint64](sec))
	}

	{
		off := f.header.data.offset
		for {
			if off >= uint64(f.header.data.offset+f.header.data.size) {
				break
			}
			evhdr := myunsafe.Map[EventHeader](f.data[off:])

			if evhdr.typ == EventTypeSample {
				fmt.Println("sample_id:", *myunsafe.Map[uint64](f.data[off+uint64(unsafe.Sizeof(evhdr)):]))
			}

			off += uint64(evhdr.size)
		}
	}

	// XXX confirm header size
	// XXX detect endianness

	// XXX validate that offset + length is in bounds
}

// type Sample struct {
// 	Header EventHeader

// 	u64                      sample_id;          /* if PERF_SAMPLE_IDENTIFIER */
// 	u64                      ip;                 /* if PERF_SAMPLE_IP */
// 	u32                      pid, tid;           /* if PERF_SAMPLE_TID */
// 	u64                      time;               /* if PERF_SAMPLE_TIME */
// 	u64                      addr;               /* if PERF_SAMPLE_ADDR */
// 	u64                      id;                 /* if PERF_SAMPLE_ID */
// 	u64                      stream_id;          /* if PERF_SAMPLE_STREAM_ID */
// 	u32                      cpu, res;           /* if PERF_SAMPLE_CPU */
// 	u64                      period;             /* if PERF_SAMPLE_PERIOD */
// 	struct read_format       v;                  /* if PERF_SAMPLE_READ */
// 	u64                      nr;                 /* if PERF_SAMPLE_CALLCHAIN */
// 	u64                      ips[nr];            /* if PERF_SAMPLE_CALLCHAIN */
// 	u32                      size;               /* if PERF_SAMPLE_RAW */
// 	char                     data[size];         /* if PERF_SAMPLE_RAW */
// 	u64                      bnr;                /* if PERF_SAMPLE_BRANCH_STACK */
// 	struct perf_branch_entry lbr[bnr];           /* if PERF_SAMPLE_BRANCH_STACK */
// 	u64                      abi;                /* if PERF_SAMPLE_REGS_USER */
// 	u64                      regs[weight(mask)]; /* if PERF_SAMPLE_REGS_USER */
// 	u64                      size;               /* if PERF_SAMPLE_STACK_USER */
// 	char                     data[size];         /* if PERF_SAMPLE_STACK_USER */
// 	u64                      dyn_size;           /* if PERF_SAMPLE_STACK_USER && size != 0 */
// 	union perf_sample_weight weight;             /* if PERF_SAMPLE_WEIGHT || PERF_SAMPLE_WEIGHT_STRUCT */
// 	u64                      data_src;           /* if PERF_SAMPLE_DATA_SRC */
// 	u64                      transaction;        /* if PERF_SAMPLE_TRANSACTION */
// 	u64                      abi;                /* if PERF_SAMPLE_REGS_INTR */
// 	u64                      regs[weight(mask)]; /* if PERF_SAMPLE_REGS_INTR */
// 	u64                      phys_addr;          /* if PERF_SAMPLE_PHYS_ADDR */
// 	u64                      cgroup;             /* if PERF_SAMPLE_CGROUP */
// 	u64                      data_page_size;     /* if PERF_SAMPLE_DATA_PAGE_SIZE */
// 	u64                      code_page_size;     /* if PERF_SAMPLE_CODE_PAGE_SIZE */
// 	u64                      size;               /* if PERF_SAMPLE_AUX */
// 	char                     data[size];         /* if PERF_SAMPLE_AUX */
// }

type SampleData struct {
	Header EventHeader
	Data   []byte
}

type Event interface{}

type EventHeader struct {
	typ  EventType
	misc uint16
	size uint16
}

//go:generate stringer -trimprefix "EventType" -type EventType -linecomment
type EventType uint32

const (
	EventTypeMmap          EventType = 1
	EventTypeLost          EventType = 2
	EventTypeComm          EventType = 3
	EventTypeExit          EventType = 4
	EventTypeThrottle      EventType = 5
	EventTypeUnthrottle    EventType = 6
	EventTypeFork          EventType = 7
	EventTypeRead          EventType = 8
	EventTypeSample        EventType = 9
	EventTypeMmap2         EventType = 10
	EventTypeAux           EventType = 11
	EventTypeItraceStart   EventType = 12
	EventTypeLostSamples   EventType = 13
	EventTypeSwitch        EventType = 14
	EventTypeSwitchCPUWide EventType = 15
	EventTypeNamespaces    EventType = 16
	EventTypeKsymbol       EventType = 17
	EventTypeBPFEvent      EventType = 18
	EventTypeCgroup        EventType = 19
	EventTypeTextPoke      EventType = 20
	EventTypeAuxOutputHWID EventType = 21

	EventTypeHeaderAttr        EventType = 64
	EventTypeHeaderEventType   EventType = 65
	EventTypeHeaderTracingData EventType = 66
	EventTypeHeaderBuildID     EventType = 67
	EventTypeFinishedRound     EventType = 68
	EventTypeIDIndex           EventType = 69
	EventTypeAuxtraceInfo      EventType = 70
	EventTypeAuxtrace          EventType = 71
	EventTypeAuxtraceError     EventType = 72
	EventTypeThreadMap         EventType = 73
	EventTypeCPUMap            EventType = 74
	EventTypeStatConfig        EventType = 75
	EventTypeStat              EventType = 76
	EventTypeStatRound         EventType = 77
	EventTypeEventUpdate       EventType = 78
	EventTypeTimeConv          EventType = 79
	EventTypeHeaderFeature     EventType = 80
	EventTypeCompressed        EventType = 81
	EventTypeFinishedInit      EventType = 82
)

type SampleType uint64

const (
	SampleTypeIP           SampleType = 1 << 0
	SampleTypeTID          SampleType = 1 << 1
	SampleTypeTime         SampleType = 1 << 2
	SampleTypeAddr         SampleType = 1 << 3
	SampleTypeRead         SampleType = 1 << 4
	SampleTypeCallchain    SampleType = 1 << 5
	SampleTypeID           SampleType = 1 << 6
	SampleTypeCPU          SampleType = 1 << 7
	SampleTypePeriod       SampleType = 1 << 8
	SampleTypeStreamID     SampleType = 1 << 9
	SampleTypeRaw          SampleType = 1 << 10
	SampleTypeBranchStack  SampleType = 1 << 11
	SampleTypeRegsUser     SampleType = 1 << 12
	SampleTypeStackUser    SampleType = 1 << 13
	SampleTypeWeight       SampleType = 1 << 14
	SampleTypeDataSrc      SampleType = 1 << 15
	SampleTypeIdentifier   SampleType = 1 << 16
	SampleTypeTransaction  SampleType = 1 << 17
	SampleTypeRegsIntr     SampleType = 1 << 18
	SampleTypePhysAddr     SampleType = 1 << 19
	SampleTypeAux          SampleType = 1 << 20
	SampleTypeCgroup       SampleType = 1 << 21
	SampleTypeDataPageSize SampleType = 1 << 22
	SampleTypeCodePageSize SampleType = 1 << 23
	SampleTypeWeightStruct SampleType = 1 << 24
	SampleTypeNum                     = 25
)

func (st SampleType) String() string {
	ss := make([]string, 0, hdrMax)
	if st&SampleTypeIP != 0 {
		ss = append(ss, "ip")
	}
	if st&SampleTypeTID != 0 {
		ss = append(ss, "tid")
	}
	if st&SampleTypeTime != 0 {
		ss = append(ss, "time")
	}
	if st&SampleTypeAddr != 0 {
		ss = append(ss, "addr")
	}
	if st&SampleTypeRead != 0 {
		ss = append(ss, "read")
	}
	if st&SampleTypeCallchain != 0 {
		ss = append(ss, "callchain")
	}
	if st&SampleTypeID != 0 {
		ss = append(ss, "id")
	}
	if st&SampleTypeCPU != 0 {
		ss = append(ss, "cpu")
	}
	if st&SampleTypePeriod != 0 {
		ss = append(ss, "period")
	}
	if st&SampleTypeStreamID != 0 {
		ss = append(ss, "stream id")
	}
	if st&SampleTypeRaw != 0 {
		ss = append(ss, "raw")
	}
	if st&SampleTypeBranchStack != 0 {
		ss = append(ss, "branch stack")
	}
	if st&SampleTypeRegsUser != 0 {
		ss = append(ss, "regs user")
	}
	if st&SampleTypeStackUser != 0 {
		ss = append(ss, "stack user")
	}
	if st&SampleTypeWeight != 0 {
		ss = append(ss, "weight")
	}
	if st&SampleTypeDataSrc != 0 {
		ss = append(ss, "data src")
	}
	if st&SampleTypeIdentifier != 0 {
		ss = append(ss, "identifier")
	}
	if st&SampleTypeTransaction != 0 {
		ss = append(ss, "transaction")
	}
	if st&SampleTypeRegsIntr != 0 {
		ss = append(ss, "regs intr")
	}
	if st&SampleTypePhysAddr != 0 {
		ss = append(ss, "phys addr")
	}
	if st&SampleTypeAux != 0 {
		ss = append(ss, "aux")
	}
	if st&SampleTypeCgroup != 0 {
		ss = append(ss, "cgroup")
	}
	if st&SampleTypeDataPageSize != 0 {
		ss = append(ss, "data page size")
	}
	if st&SampleTypeCodePageSize != 0 {
		ss = append(ss, "code page size")
	}
	if st&SampleTypeWeightStruct != 0 {
		ss = append(ss, "weight struct")
	}
	st &^= (1 << SampleTypeNum) - 1
	if st != 0 {
		ss = append(ss, fmt.Sprintf("%b", st))
	}
	return strings.Join(ss, " | ")
}
