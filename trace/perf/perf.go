package main

// XXX this package shouldn't be in the trace folder. perf isn't tracing.
// XXX eliminate all panics
// XXX bounds check everything
// XXX all our conversions for filenames etc assume that there is no sample_id trailer

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

	"github.com/klauspost/compress/zstd"
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

//go:generate stringer -type RecordType
type RecordType uint32

const (
	PERF_RECORD_MMAP             RecordType = 1
	PERF_RECORD_LOST             RecordType = 2
	PERF_RECORD_COMM             RecordType = 3
	PERF_RECORD_EXIT             RecordType = 4
	PERF_RECORD_THROTTLE         RecordType = 5
	PERF_RECORD_UNTHROTTLE       RecordType = 6
	PERF_RECORD_FORK             RecordType = 7
	PERF_RECORD_READ             RecordType = 8
	PERF_RECORD_SAMPLE           RecordType = 9
	PERF_RECORD_MMAP2            RecordType = 10
	PERF_RECORD_AUX              RecordType = 11
	PERF_RECORD_ITRACE_START     RecordType = 12
	PERF_RECORD_LOST_SAMPLES     RecordType = 13
	PERF_RECORD_SWITCH           RecordType = 14
	PERF_RECORD_SWITCH_CPU_WIDE  RecordType = 15
	PERF_RECORD_NAMESPACES       RecordType = 16
	PERF_RECORD_KSYMBOL          RecordType = 17
	PERF_RECORD_BPF_EVENT        RecordType = 18
	PERF_RECORD_CGROUP           RecordType = 19
	PERF_RECORD_TEXT_POKE        RecordType = 20
	PERF_RECORD_AUX_OUTPUT_HW_ID RecordType = 21

	PERF_RECORD_HEADER_ATTR         RecordType = 64
	PERF_RECORD_HEADER_EVENT_TYPE   RecordType = 65
	PERF_RECORD_HEADER_TRACING_DATA RecordType = 66
	PERF_RECORD_HEADER_BUILD_ID     RecordType = 67
	PERF_RECORD_FINISHED_ROUND      RecordType = 68
	PERF_RECORD_ID_INDEX            RecordType = 69
	PERF_RECORD_AUXTRACE_INFO       RecordType = 70
	PERF_RECORD_AUXTRACE            RecordType = 71
	PERF_RECORD_AUXTRACE_ERROR      RecordType = 72
	PERF_RECORD_THREAD_MAP          RecordType = 73
	PERF_RECORD_CPU_MAP             RecordType = 74
	PERF_RECORD_STAT_CONFIG         RecordType = 75
	PERF_RECORD_STAT                RecordType = 76
	PERF_RECORD_STAT_ROUND          RecordType = 77
	PERF_RECORD_EVENT_UPDATE        RecordType = 78
	PERF_RECORD_TIME_CONV           RecordType = 79
	PERF_RECORD_HEADER_FEATURE      RecordType = 80
	PERF_RECORD_COMPRESSED          RecordType = 81
	PERF_RECORD_FINISHED_INIT       RecordType = 82
)

type SampleFlag uint64

const (
	PERF_SAMPLE_IP             SampleFlag = 1 << 0
	PERF_SAMPLE_TID            SampleFlag = 1 << 1
	PERF_SAMPLE_TIME           SampleFlag = 1 << 2
	PERF_SAMPLE_ADDR           SampleFlag = 1 << 3
	PERF_SAMPLE_READ           SampleFlag = 1 << 4
	PERF_SAMPLE_CALLCHAIN      SampleFlag = 1 << 5
	PERF_SAMPLE_ID             SampleFlag = 1 << 6
	PERF_SAMPLE_CPU            SampleFlag = 1 << 7
	PERF_SAMPLE_PERIOD         SampleFlag = 1 << 8
	PERF_SAMPLE_STREAM_ID      SampleFlag = 1 << 9
	PERF_SAMPLE_RAW            SampleFlag = 1 << 10
	PERF_SAMPLE_BRANCH_STACK   SampleFlag = 1 << 11
	PERF_SAMPLE_REGS_USER      SampleFlag = 1 << 12
	PERF_SAMPLE_STACK_USER     SampleFlag = 1 << 13
	PERF_SAMPLE_WEIGHT         SampleFlag = 1 << 14
	PERF_SAMPLE_DATA_SRC       SampleFlag = 1 << 15
	PERF_SAMPLE_IDENTIFIER     SampleFlag = 1 << 16
	PERF_SAMPLE_TRANSACTION    SampleFlag = 1 << 17
	PERF_SAMPLE_REGS_INTR      SampleFlag = 1 << 18
	PERF_SAMPLE_PHYS_ADDR      SampleFlag = 1 << 19
	PERF_SAMPLE_AUX            SampleFlag = 1 << 20
	PERF_SAMPLE_CGROUP         SampleFlag = 1 << 21
	PERF_SAMPLE_DATA_PAGE_SIZE SampleFlag = 1 << 22
	PERF_SAMPLE_CODE_PAGE_SIZE SampleFlag = 1 << 23
	PERF_SAMPLE_WEIGHT_STRUCT  SampleFlag = 1 << 24

	PERF_SAMPLE_NUM = 25
	PERF_SAMPLE_MAX = 1 << PERF_SAMPLE_NUM
)

func (st SampleFlag) String() string {
	ss := make([]string, 0, hdrMax)
	if st&PERF_SAMPLE_IP != 0 {
		ss = append(ss, "ip")
	}
	if st&PERF_SAMPLE_TID != 0 {
		ss = append(ss, "tid")
	}
	if st&PERF_SAMPLE_TIME != 0 {
		ss = append(ss, "time")
	}
	if st&PERF_SAMPLE_ADDR != 0 {
		ss = append(ss, "addr")
	}
	if st&PERF_SAMPLE_READ != 0 {
		ss = append(ss, "read")
	}
	if st&PERF_SAMPLE_CALLCHAIN != 0 {
		ss = append(ss, "callchain")
	}
	if st&PERF_SAMPLE_ID != 0 {
		ss = append(ss, "id")
	}
	if st&PERF_SAMPLE_CPU != 0 {
		ss = append(ss, "cpu")
	}
	if st&PERF_SAMPLE_PERIOD != 0 {
		ss = append(ss, "period")
	}
	if st&PERF_SAMPLE_STREAM_ID != 0 {
		ss = append(ss, "stream id")
	}
	if st&PERF_SAMPLE_RAW != 0 {
		ss = append(ss, "raw")
	}
	if st&PERF_SAMPLE_BRANCH_STACK != 0 {
		ss = append(ss, "branch stack")
	}
	if st&PERF_SAMPLE_REGS_USER != 0 {
		ss = append(ss, "regs user")
	}
	if st&PERF_SAMPLE_STACK_USER != 0 {
		ss = append(ss, "stack user")
	}
	if st&PERF_SAMPLE_WEIGHT != 0 {
		ss = append(ss, "weight")
	}
	if st&PERF_SAMPLE_DATA_SRC != 0 {
		ss = append(ss, "data src")
	}
	if st&PERF_SAMPLE_IDENTIFIER != 0 {
		ss = append(ss, "identifier")
	}
	if st&PERF_SAMPLE_TRANSACTION != 0 {
		ss = append(ss, "transaction")
	}
	if st&PERF_SAMPLE_REGS_INTR != 0 {
		ss = append(ss, "regs intr")
	}
	if st&PERF_SAMPLE_PHYS_ADDR != 0 {
		ss = append(ss, "phys addr")
	}
	if st&PERF_SAMPLE_AUX != 0 {
		ss = append(ss, "aux")
	}
	if st&PERF_SAMPLE_CGROUP != 0 {
		ss = append(ss, "cgroup")
	}
	if st&PERF_SAMPLE_DATA_PAGE_SIZE != 0 {
		ss = append(ss, "data page size")
	}
	if st&PERF_SAMPLE_CODE_PAGE_SIZE != 0 {
		ss = append(ss, "code page size")
	}
	if st&PERF_SAMPLE_WEIGHT_STRUCT != 0 {
		ss = append(ss, "weight struct")
	}
	st &^= PERF_SAMPLE_MAX - 1
	if st != 0 {
		ss = append(ss, fmt.Sprintf("%b", st))
	}
	return strings.Join(ss, " | ")
}

type flags [4]uint64

func (fl *flags) bit(x hdrType) bool {
	return fl[x/64]&(1<<(x-(x/64)*64)) != 0
}

func (fl *flags) String() string {
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
	return myunsafe.ToSliceStr[[]uint64](f.data[attr.ids.offset : attr.ids.offset+attr.ids.size])
}

type eventAttr struct {
	typ                     eventType
	size                    uint32
	config                  uint64
	samplePeriodOrFreq      uint64
	sampleType              SampleFlag
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
	PERF_TYPE_HARDWARE   eventType = 0
	PERF_TYPE_SOFTWARE   eventType = 1
	PERF_TYPE_TRACEPOINT eventType = 2
	PERF_TYPE_HW_CACHE   eventType = 3
	PERF_TYPE_RAW        eventType = 4
	PERF_TYPE_BREAKPOINT eventType = 5

	PERF_TYPE_NUM = 6
)

func (typ eventType) String() string {
	switch typ {
	case PERF_TYPE_HARDWARE:
		return "hardware"
	case PERF_TYPE_SOFTWARE:
		return "software"
	case PERF_TYPE_TRACEPOINT:
		return "tracepoint"
	case PERF_TYPE_HW_CACHE:
		return "hw cache"
	case PERF_TYPE_RAW:
		return "raw"
	case PERF_TYPE_BREAKPOINT:
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
	data     string

	// Map from event IDs to event descriptions
	eventAttrs map[uint64]*fileEventAttr
	// Map from record ID to file offset of its event header
	recordOffsets []uint64

	sharedType   container.Option[SampleFlag]
	sharedOffset container.Option[uint64]
}

func (f *File) EventAttrs() []fileEventAttr {
	return myunsafe.ToSliceStr[[]fileEventAttr](f.data[f.header.attrs.offset:][:f.header.attrs.size])
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
	return myunsafe.MapStr[Clock](f.data[s.offset:])
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
	return myunsafe.MapStr[fileSection](f.data[f.sections[section]:])
}

func (f *File) strSection(section hdrType) string {
	if f.sections[section] == 0 {
		return ""
	}
	s := f.section(section)
	return f.str(s.offset)
}

func (f *File) str(offset uint64) string {
	n := *myunsafe.MapStr[uint32](f.data[offset:])
	if n == 0 {
		return ""
	}
	str := f.data[offset+4 : offset+4+uint64(n)]
	// XXX verify there's a zero
	off := strings.IndexByte(str, 0)
	return str[0:off]
}

func NewFile(data []byte) *File {
	f := File{
		header:     myunsafe.Map[header](data),
		data:       unsafe.String(&data[0], len(data)),
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

	{

		s := f.section(hdrCompressed)
		b := []byte(f.data[s.offset:][:s.size])
		fmt.Println(1, *myunsafe.Map[uint32](b[0:]))
		fmt.Println(2, *myunsafe.Map[uint32](b[4:]))
		fmt.Println(3, *myunsafe.Map[uint32](b[8:]))
		fmt.Println(4, *myunsafe.Map[uint32](b[12:]))
		fmt.Println(5, *myunsafe.Map[uint32](b[16:]))

		// u32                     comp_ratio;
		// u32                     comp_ver;
		// u32                     comp_type;
		// u32                     comp_level;
		// u32                     comp_mmap_len;
	}

	// Figure out how to reliably determine the types of samples. When there are multiple event sources in a
	// single file, then different sources may have different sample types, i.e. have different sets of
	// fields. If they do have different types, then we must look them up before parsing. For this, we need
	// the samples' event IDs. If all sample_types include SampleTypeIdentifier, then the IDs are the first
	// field in the sample. If all sample_types include SampleTypeID, and the ID is always at the same offset,
	// then that's the offset we'll use. In all other cases we cannot reliably parse the file.
	attrs := f.EventAttrs()
	if len(attrs) > 0 {
		allHaveIdentifier := attrs[0].sampleType&PERF_SAMPLE_IDENTIFIER != 0
		allHaveID := attrs[0].sampleType&PERF_SAMPLE_ID != 0
		allShareType := true
		for i := range attrs {
			attr := &attrs[i]
			typ := attr.sampleType
			if typ != attrs[0].sampleType {
				allShareType = false
			}
			if typ&PERF_SAMPLE_IDENTIFIER == 0 {
				allHaveIdentifier = false
			}
			if typ&PERF_SAMPLE_ID == 0 {
				allHaveID = false
			}

			for _, id := range f.EventAttrIDs(attr) {
				f.eventAttrs[id] = attr
			}
		}

		idOffset := func(typ SampleFlag) uint64 {
			set := bits.OnesCount64(uint64(typ & (PERF_SAMPLE_IDENTIFIER | PERF_SAMPLE_IP | PERF_SAMPLE_TID | PERF_SAMPLE_TIME | PERF_SAMPLE_ADDR)))
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
	// XXX validate that b is long enough for the event type, including lengths inside the event.

	b := f.data[f.recordOffsets[idx]:]
	h := myunsafe.MapStr[EventHeader](b)
	switch h.typ {
	case PERF_RECORD_MMAP:
		return myunsafe.MapStr[Mmap](b)
	case PERF_RECORD_LOST:
		return myunsafe.MapStr[Lost](b)
	case PERF_RECORD_COMM:
		return myunsafe.MapStr[Comm](b)
	case PERF_RECORD_EXIT:
		return myunsafe.MapStr[Exit](b)
	// case PERF_RECORD_THROTTLE:
	// case PERF_RECORD_UNTHROTTLE:
	// case PERF_RECORD_FORK:
	// case PERF_RECORD_READ:
	case PERF_RECORD_SAMPLE:
		return myunsafe.MapStr[SampleHandle](b)
	case PERF_RECORD_MMAP2:
		return myunsafe.MapStr[Mmap2](b)
	// case PERF_RECORD_AUX:
	// case PERF_RECORD_ITRACE_START:
	// case PERF_RECORD_LOST_SAMPLES:
	case PERF_RECORD_SWITCH:
		return myunsafe.MapStr[Switch](b)
	case PERF_RECORD_SWITCH_CPU_WIDE:
		return myunsafe.MapStr[Switch](b)
	// case PERF_RECORD_NAMESPACES:
	case PERF_RECORD_KSYMBOL:
		return myunsafe.MapStr[Ksymbol](b)
	// case PERF_RECORD_BPF_EVENT:
	case PERF_RECORD_CGROUP:
		return myunsafe.MapStr[Cgroup](b)
	case PERF_RECORD_TEXT_POKE:
		return myunsafe.MapStr[TextPoke](b)
	// case PERF_RECORD_AUX_OUTPUT_HW_ID:
	// case PERF_RECORD_HEADER_ATTR:
	// case PERF_RECORD_HEADER_EVENT_TYPE:
	// case PERF_RECORD_HEADER_TRACING_DATA:
	// case PERF_RECORD_HEADER_BUILD_ID:
	// case PERF_RECORD_FINISHED_ROUND:
	case PERF_RECORD_ID_INDEX:
		n := *myunsafe.MapStr[uint64](b[8:])
		return myunsafe.ToSliceStr[IDIndex](b[16 : 16+n*uint64(unsafe.Sizeof(IDIndexEntry{}))])
	// case PERF_RECORD_AUXTRACE_INFO:
	// case PERF_RECORD_AUXTRACE:
	// case PERF_RECORD_AUXTRACE_ERROR:
	case PERF_RECORD_THREAD_MAP:
		n := *myunsafe.MapStr[uint64](b[8:])
		return myunsafe.ToSliceStr[ThreadMaps](b[16 : 16+n*uint64(unsafe.Sizeof(ThreadMap{}))])
	// case PERF_RECORD_CPU_MAP:
	// case PERF_RECORD_STAT_CONFIG:
	// case PERF_RECORD_STAT:
	// case PERF_RECORD_STAT_ROUND:
	// case PERF_RECORD_EVENT_UPDATE:
	case PERF_RECORD_TIME_CONV:
		return myunsafe.MapStr[TimeConv](b)
	// case PERF_RECORD_HEADER_FEATURE:
	case PERF_RECORD_COMPRESSED:
		sz := myunsafe.MapStr[EventHeader](b).size + 8
		fmt.Println(myunsafe.MapStr[EventHeader](b))
		d := b[8:sz]
		dec, _ := zstd.NewReader(nil, zstd.WithDecoderConcurrency(0))
		dd := unsafe.Slice((*byte)(unsafe.StringData(d)), len(d))
		fmt.Println(len(dd))
		e, err := dec.DecodeAll(dd, nil)
		if err != nil {
			panic(err)
		}
		fmt.Println(e)
		return nil
	// case PERF_RECORD_FINISHED_INIT:
	default:
		// XXX return error instead of panicking
		return nil
		panic(fmt.Sprintf("unhandled type %s", h.typ))
	}
}

type Inode struct {
	major           uint32
	minor           uint32
	inode           uint64
	inodeGeneration uint64
}

func (n *Inode) Major() uint32           { return n.major }
func (n *Inode) Minor() uint32           { return n.minor }
func (n *Inode) Inode() uint64           { return n.inode }
func (n *Inode) InodeGeneration() uint64 { return n.inodeGeneration }

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

	PERF_RECORD_MISC_CPUMODE_MASK    = 0b111
	PERF_RECORD_MISC_CPUMODE_UNKNOWN = 0
	PERF_RECORD_MISC_KERNEL          = 1
	PERF_RECORD_MISC_USER            = 2
	PERF_RECORD_MISC_HYPERVISOR      = 3
	PERF_RECORD_MISC_GUEST_KERNEL    = 4
	PERF_RECORD_MISC_GUEST_USER      = 5

	PERF_RECORD_MISC_MMAP_DATA  = 1 << 13
	PERF_RECORD_MISC_COMM_EXEC  = 1 << 13
	PERF_RECORD_MISC_FORK_EXEC  = 1 << 13
	PERF_RECORD_MISC_SWITCH_OUT = 1 << 13

	PERF_RECORD_MISC_EXACT_IP           = 1 << 14
	PERF_RECORD_MISC_SWITCH_OUT_PREEMPT = 1 << 14
	PERF_RECORD_MISC_MMAP_BUILD_ID      = 1 << 14
)

func (m *Mmap2) String() string {
	prot := m.Protection()
	flags := m.Flags()
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

	var s string
	if m.Misc()&PERF_RECORD_MISC_MMAP_BUILD_ID != 0 {
		s = fmt.Sprintf("%x", m.BuildID())
	} else {
		inode := m.Inode()
		s = fmt.Sprintf("%02x:%02x %d %d", inode.Major(), inode.Minor(), inode.Inode(), inode.InodeGeneration())
	}
	return fmt.Sprintf("PERF_RECORD_MMAP2 %d/%d: [0x%x(0x%x) @ %d %s]: %s %s",
		m.PID(), m.TID(), m.Addr(), m.Len(), m.PageOffset(), s, prots, m.Filename())
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

// [X] EventTypeMmap
// [X] EventTypeLost
// [X] EventTypeComm
// [X] EventTypeExit
// [ ] EventTypeThrottle
// [ ] EventTypeUnthrottle
// [ ] EventTypeFork
// [ ] EventTypeRead
// [ ] EventTypeSample
// [X] EventTypeMmap2
// [ ] EventTypeAux
// [ ] EventTypeItraceStart
// [ ] EventTypeLostSamples
// [X] EventTypeSwitch
// [X] EventTypeSwitchCPUWide
// [ ] EventTypeNamespaces
// [X] EventTypeKsymbol
// [ ] EventTypeBPFEvent
// [ ] EventTypeCgroup
// [X] EventTypeTextPoke
// [ ] EventTypeAuxOutputHWID
//
// [ ] EventTypeHeaderAttr
// [ ] EventTypeHeaderEventType
// [ ] EventTypeHeaderTracingData
// [ ] EventTypeHeaderBuildID
// [ ] EventTypeFinishedRound
// [X] EventTypeIDIndex
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
// [ ] EventTypeFinishedInit

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
	Misc     uint16
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
	Raw      container.Option[string]
	Lbr      container.Option[[]BranchEntry]

	// XXX ABIUser and RegsUser should be a single Option
	ABIUser     container.Option[uint64]
	RegsUser    container.Option[Registers]
	Stack       container.Option[string]
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
	Aux          container.Option[string]
}

func (s *Sample) String() string {
	f := `PERF_RECORD_SAMPLE
... ID: %v
... IP: %#x
... PID: %v
... TID: %v
... Time: %v
... Addr: %#x
... StreamID: %v
... CPU: %v
... Res: %v
... Period: %v
... V: %v
... IPs: %#x
... Raw: %v
... Lbr: %v
... ABIUser: %v
... RegsUser: %v
... Stack: %v
... Weight: %v
... DataSrc: %v
... Transaction: %v
... ABIIntr: %v
... RegsIntr: %v
... PhysAddr: %#x
... Cgroup: %v
... DataPageSize: %v
... CodePageSize: %v
... Aux: %v
`

	return fmt.Sprintf(f,
		s.ID, s.IP, s.PID, s.TID, s.Time, s.Addr, s.StreamID, s.CPU, s.Res, s.Period, s.V, s.IPs,
		s.Raw, s.Lbr, s.ABIUser, s.RegsUser, s.Stack, s.Weight, s.DataSrc, s.Transaction, s.ABIIntr, s.RegsIntr,
		s.PhysAddr, s.Cgroup, s.DataPageSize, s.CodePageSize, s.Aux,
	)
}

func (f *File) Sample(sh *SampleHandle, s *Sample) {
	p := unsafe.Pointer(sh)
	u64 := func() uint64 {
		ret := *(*uint64)(p)
		p = unsafe.Add(p, 8)
		return ret
	}
	u32 := func() uint32 {
		ret := *(*uint32)(p)
		p = unsafe.Add(p, 4)
		return ret
	}

	h := (*EventHeader)(unsafe.Pointer(p))
	p = unsafe.Add(p, unsafe.Sizeof(*h))

	var typ SampleFlag
	if h.typ == PERF_RECORD_SAMPLE {
		if t, ok := f.sharedType.Get(); ok {
			typ = t
		} else {
			off := f.sharedOffset.MustGet()
			off += uint64(unsafe.Sizeof(EventHeader{}))
			id := *myunsafe.MapStr[uint64](f.data[off:])
			typ = f.eventAttrs[id].sampleType
		}
	}
	s.Misc = h.misc
	if typ&PERF_SAMPLE_IDENTIFIER != 0 {
		s.ID = container.Some(u64())
	}
	if typ&PERF_SAMPLE_IP != 0 {
		s.IP = container.Some(u64())
	}
	if typ&PERF_SAMPLE_TID != 0 {
		s.PID, s.TID = container.Some(u32()), container.Some(u32())
	}
	if typ&PERF_SAMPLE_TIME != 0 {
		s.Time = container.Some(u64())
	}
	if typ&PERF_SAMPLE_ADDR != 0 {
		s.Addr = container.Some(u64())
	}
	if typ&PERF_SAMPLE_ID != 0 {
		s.ID = container.Some(u64())
	}
	if typ&PERF_SAMPLE_STREAM_ID != 0 {
		s.StreamID = container.Some(u64())
	}
	if typ&PERF_SAMPLE_CPU != 0 {
		s.CPU, s.Res = container.Some(u32()), container.Some(u32())
	}
	if typ&PERF_SAMPLE_PERIOD != 0 {
		s.Period = container.Some(u64())
	}
	if typ&PERF_SAMPLE_READ != 0 {
		// XXX
	}
	if typ&PERF_SAMPLE_CALLCHAIN != 0 {
		n := uintptr(u64())
		s.IPs = container.Some(myunsafe.ToSliceStr[[]uint64](unsafe.String((*byte)(p), n*8)))
		p = unsafe.Add(p, n*8)
	}
	if typ&PERF_SAMPLE_RAW != 0 {
		n := uintptr(u32())
		s.Raw = container.Some(unsafe.String((*byte)(p), n))
		p = unsafe.Add(p, n)
	}
	if typ&PERF_SAMPLE_BRANCH_STACK != 0 {
		// XXX
	}
	regs := func(mask uint64) Registers {
		weight := bits.OnesCount64(mask)
		regs := myunsafe.ToSliceStr[[]uint64](unsafe.String((*byte)(p), uint64(weight)*8))
		out := Registers{
			Mask: mask,
		}
		for i, j := 0, 0; i < 64; i++ {
			if mask&(1<<i) != 0 {
				out.Registers[i] = regs[j]
				j++
			}
		}
		p = unsafe.Add(p, uint64(weight)*8)
		return out
	}
	if typ&PERF_SAMPLE_REGS_USER != 0 {
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
	if typ&PERF_SAMPLE_STACK_USER != 0 {
		// The first size is the full, padded size of the stack data.
		n1 := u64()
		if n1 > 0 {
			// The second size is the actual size of the stack data, with n2 <= n1.
			n2 := u64()
			s.Stack = container.Some(unsafe.String((*byte)(p), n2))
			p = unsafe.Add(p, n1)
		}
	}
	if typ&PERF_SAMPLE_WEIGHT != 0 {
		s.Weight = container.Some(u64())
	} else if typ&PERF_SAMPLE_WEIGHT_STRUCT != 0 {
		// TODO support the struct
		s.Weight = container.Some(u64())
	}
	if typ&PERF_SAMPLE_DATA_SRC != 0 {
		s.DataSrc = container.Some(u64())
	}
	if typ&PERF_SAMPLE_TRANSACTION != 0 {
		s.Transaction = container.Some(u64())
	}
	if typ&PERF_SAMPLE_REGS_INTR != 0 {
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
	if typ&PERF_SAMPLE_PHYS_ADDR != 0 {
		s.PhysAddr = container.Some(u64())
	}
	if typ&PERF_SAMPLE_CGROUP != 0 {
		s.Cgroup = container.Some(u64())
	}
	if typ&PERF_SAMPLE_DATA_PAGE_SIZE != 0 {
		s.DataPageSize = container.Some(u64())
	}
	if typ&PERF_SAMPLE_CODE_PAGE_SIZE != 0 {
		s.CodePageSize = container.Some(u64())
	}
	if typ&PERF_SAMPLE_AUX != 0 {
		n := u64()
		s.Aux = container.Some(unsafe.String((*byte)(p), n))
		p = unsafe.Add(p, n)
	}
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

func main() {
	// elfStuff()
	// return

	data, err := os.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	f := NewFile(data)
	for i := 0; i < f.NumRecords(); i++ {
		r := f.Record(i)
		switch r := r.(type) {
		case *SampleHandle:
			var s Sample
			f.Sample(r, &s)
			fmt.Println(&s)
		case *Comm:
			fmt.Println(r.String())
		default:
			fmt.Println(r)
		}
		// switch r := r.(type) {
		// case *Comm:
		// 	fmt.Println(r.Comm())
		// case *Exit:
		// 	fmt.Println("exit", r.Exit())
		// case *Mmap2:
		// 	fmt.Println(r.Mmap2())
		// case *Sample:
		// 	var sample Sample
		// 	r.Sample(f, &sample)
		// 	fmt.Println(&sample)
		// case *TimeConv:
		// 	fmt.Println(r.TimeConv())
		// case *ThreadMap:
		// 	fmt.Println(r.ThreadMap())
		// case *Lost:
		// 	fmt.Println(r.Lost())
		// case *LostSamples:
		// 	fmt.Println(r.LostSamples())
		// case *Ksymbol:
		// 	fmt.Println(r.Ksymbol())
		// case *IDIndex:
		// 	fmt.Println(r.IDIndex())
		// case *FinishedInit:
		// 	fmt.Println("PERF_RECORD_FINISHED_INIT")
		// case *FinishedRound:
		// 	fmt.Println("PERF_RECORD_FINISHED_ROUND")
		// case *Switch:
		// 	fmt.Println(r.Switch())
		// case *TextPoke:
		// 	fmt.Println(r.TextPoke())
		// case *Cgroup:
		// 	fmt.Println(r.Cgroup())
		// default:
		// 	fmt.Println("womp womp", r.Header().typ)
		// }
	}

	// XXX confirm header size
	// XXX detect endianness

	// XXX validate that offset + length is in bounds
}

type Event interface{}

type EventHeader struct {
	typ  RecordType
	misc uint16
	size uint16
}

type recordCgroup struct {
	_  EventHeader
	id uint64
}

type recordComm struct {
	EventHeader
	pid uint32
	tid uint32
}

type recordExit struct {
	_    EventHeader
	pid  uint32
	ppid uint32
	tid  uint32
	ptid uint32
	time uint64
}

type recordFork struct {
	_    EventHeader
	pid  uint32
	ppid uint32
	tid  uint32
	ptid uint32
	time uint64
}

type recordMmap struct {
	_          EventHeader
	pid        uint32
	tid        uint32
	addr       uint64
	len        uint64
	pageOffset uint64
}

type recordMmap2 struct {
	EventHeader
	pid        uint32
	tid        uint32
	addr       uint64
	len        uint64
	pageOffset uint64
	inodeOrID  [24]byte
	protection uint32
	flags      uint32
}

type recordKsymbol struct {
	_     EventHeader
	addr  uint64
	len   uint32
	typ   uint16
	flags uint16
}

type recordLost struct {
	_    EventHeader
	id   uint64
	lost uint64
}

type recordLostSamples struct {
	_    EventHeader
	lost uint64
}

type recordSwitch struct {
	// In perf's on-disk file format, PERF_RECORD_SWITCH and PERF_RECORD_SWITCH_CPU_WIDE use the same struct
	// type that includes the pid and tid. This wouldn't be the case if we were talking to the kernel
	// directly.
	EventHeader
	pid uint32
	tid uint32
}

type recordTextPoke struct {
	_      EventHeader
	addr   uint64
	oldLen uint16
	newLen uint16
}

type recordTimeConv struct {
	_                EventHeader
	timeShift        uint64
	timeMult         uint64
	timeZero         uint64
	timeCycles       uint64
	timeMask         uint64
	capUserTimeZero  uint8
	capUserTimeShort uint8
	_                uint8
}

type Cgroup struct{ b byte }
type Comm struct{ b byte }
type Exit struct{ b byte }
type Fork struct{ b byte }
type Ksymbol struct{ b byte }
type Mmap struct{ b byte }
type Mmap2 struct{ b byte }
type SampleHandle struct{ b byte }
type Switch struct{ b byte }
type TimeConv struct{ b byte }
type TextPoke struct{ b byte }
type Lost struct{ b byte }
type LostSamples struct{ b byte }

// func (r Record) Header() *EventHeader { return cast[EventHeader](r.data) }

func (c *Cgroup) ID() uint64   { return cast[recordCgroup](c).id }
func (c *Cgroup) Path() string { return trailingString[recordCgroup](c) }
func (c *Cgroup) String() string {
	return fmt.Sprintf("PERF_RECORD_CGROUP cgroup: %d %s", c.ID(), c.Path())
}

func cast[T any, E any, P *E](x P) *T {
	return (*T)(unsafe.Pointer(x))
}

func trailingString[R any, E any, P *E](x P) string {
	total := uintptr((*EventHeader)(unsafe.Pointer(x)).size)
	off := unsafe.Sizeof(*new(R))
	s := unsafe.String((*byte)(unsafe.Add(unsafe.Pointer(x), off)), total-off)
	n := strings.IndexByte(s, 0)
	return s[:n]
}

func (c *Comm) Misc() uint16 { return cast[recordComm](c).misc }
func (c *Comm) PID() uint32  { return cast[recordComm](c).pid }
func (c *Comm) TID() uint32  { return cast[recordComm](c).tid }
func (c *Comm) Name() string { return trailingString[recordComm](c) }
func (c *Comm) String() string {
	return fmt.Sprintf("PERF_RECORD_COMM: %s:%d/%d", c.Name(), c.PID(), c.TID())
}

func (e *Exit) Pid() uint32  { return cast[recordExit](e).pid }
func (e *Exit) Ppid() uint32 { return cast[recordExit](e).ppid }
func (e *Exit) Tid() uint32  { return cast[recordExit](e).tid }
func (e *Exit) Ptid() uint32 { return cast[recordExit](e).ptid }
func (e *Exit) Time() uint64 { return cast[recordExit](e).time }
func (e *Exit) String() string {
	return fmt.Sprintf("PERF_RECORD_EXIT (%d:%d):(%d:%d)", e.Ppid(), e.Tid(), e.Ppid(), e.Ptid())
}

func (f *Fork) Pid() uint32  { return cast[recordFork](f).pid }
func (f *Fork) Ppid() uint32 { return cast[recordFork](f).ppid }
func (f *Fork) Tid() uint32  { return cast[recordFork](f).tid }
func (f *Fork) Ptid() uint32 { return cast[recordFork](f).ptid }
func (f *Fork) Time() uint64 { return cast[recordFork](f).time }

func (sym *Ksymbol) Addr() uint64  { return cast[recordKsymbol](sym).addr }
func (sym *Ksymbol) Len() uint32   { return cast[recordKsymbol](sym).len }
func (sym *Ksymbol) Type() uint16  { return cast[recordKsymbol](sym).typ }
func (sym *Ksymbol) Flags() uint16 { return cast[recordKsymbol](sym).flags }
func (sym *Ksymbol) Name() string  { return trailingString[recordKsymbol](sym) }
func (sym *Ksymbol) String() string {
	return fmt.Sprintf("PERF_RECORD_KSYMBOL addr %d len %d type %d flags 0x%x name %s",
		sym.Addr(), sym.Len(), sym.Type(), sym.Flags(), sym.Name())
}

func (l *Lost) ID() uint64     { return cast[recordLost](l).id }
func (l *Lost) Num() uint64    { return cast[recordLost](l).lost }
func (l *Lost) String() string { return fmt.Sprintf("PERF_RECORD_LOST lost %d", l.Num()) }

func (l *LostSamples) Num() uint64 { return cast[recordLostSamples](l).lost }

func (m *Mmap) PID() uint32        { return cast[recordMmap](m).pid }
func (m *Mmap) TID() uint32        { return cast[recordMmap](m).tid }
func (m *Mmap) Addr() uint64       { return cast[recordMmap](m).addr }
func (m *Mmap) Len() uint64        { return cast[recordMmap](m).len }
func (m *Mmap) PageOffset() uint64 { return cast[recordMmap](m).pageOffset }
func (m *Mmap) Filename() string   { return trailingString[recordMmap](m) }

func (m *Mmap) String() string {
	return fmt.Sprintf("PERF_RECORD_MMAP %d/%d: [0x%x(0x%x) @ %d]: %s",
		m.PID(), m.TID(), m.Addr(), m.Len(), m.PageOffset(), m.Filename())
}

func (m *Mmap2) Misc() uint16       { return cast[recordMmap2](m).misc }
func (m *Mmap2) PID() uint32        { return cast[recordMmap2](m).pid }
func (m *Mmap2) TID() uint32        { return cast[recordMmap2](m).tid }
func (m *Mmap2) Addr() uint64       { return cast[recordMmap2](m).addr }
func (m *Mmap2) Len() uint64        { return cast[recordMmap2](m).len }
func (m *Mmap2) PageOffset() uint64 { return cast[recordMmap2](m).pageOffset }
func (m *Mmap2) Protection() uint32 { return cast[recordMmap2](m).protection }
func (m *Mmap2) Flags() uint32      { return cast[recordMmap2](m).flags }
func (m *Mmap2) Inode() *Inode {
	return myunsafe.UnsafeMap[Inode](cast[recordMmap2](m).inodeOrID[:])
}
func (m *Mmap2) BuildID() []byte {
	type buildID struct {
		len uint8
		_   uint8
		_   uint16
		id  [20]byte
	}
	id := myunsafe.UnsafeMap[buildID](cast[recordMmap2](m).inodeOrID[:])
	return id.id[:id.len]
}
func (m *Mmap2) Filename() string {
	return trailingString[recordMmap2](m)
}

func (s *SampleHandle) String() string { return "PERF_RECORD_SAMPLE (handle)" }

func (s *Switch) Misc() uint16 { return cast[recordSwitch](s).misc }
func (s *Switch) Pid() uint32  { return cast[recordSwitch](s).pid }
func (s *Switch) Tid() uint32  { return cast[recordSwitch](s).tid }
func (s *Switch) String() string {
	if s.Misc()&PERF_RECORD_MISC_SWITCH_OUT != 0 {
		if s.Misc()&PERF_RECORD_MISC_SWITCH_OUT_PREEMPT != 0 {
			return "PERF_RECORD_MISC_SWITCH OUT preempt"
		} else {
			return "PERF_RECORD_MISC_SWITCH OUT"
		}
	} else {
		return "PERF_RECORD_MISC_SWITCH IN"
	}
}

func (t *TextPoke) Addr() uint64   { return cast[recordTextPoke](t).addr }
func (t *TextPoke) OldLen() uint16 { return cast[recordTextPoke](t).oldLen }
func (t *TextPoke) NewLen() uint16 { return cast[recordTextPoke](t).newLen }
func (t *TextPoke) Bytes() string {
	off := unsafe.Sizeof(recordTextPoke{})
	return unsafe.String((*byte)(unsafe.Add(unsafe.Pointer(t), off)), t.OldLen()+t.NewLen())
}
func (t *TextPoke) String() string {
	return fmt.Sprintf("PERF_RECORD_TEXT_POKE %p old len %d new len %d",
		t.Addr(), t.OldLen(), t.NewLen())
}

func (t *TimeConv) TimeShift() uint64       { return cast[recordTimeConv](t).timeShift }
func (t *TimeConv) TimeMult() uint64        { return cast[recordTimeConv](t).timeMult }
func (t *TimeConv) TimeZero() uint64        { return cast[recordTimeConv](t).timeZero }
func (t *TimeConv) TimeCycles() uint64      { return cast[recordTimeConv](t).timeCycles }
func (t *TimeConv) TimeMask() uint64        { return cast[recordTimeConv](t).timeMask }
func (t *TimeConv) CapUserTimeZero() uint8  { return cast[recordTimeConv](t).capUserTimeZero }
func (t *TimeConv) CapUserTimeShort() uint8 { return cast[recordTimeConv](t).capUserTimeShort }

func (tc *TimeConv) String() string {
	f := `PERF_RECORD_TIME_CONV
... Time Shift      %d
... Time Muliplier  %d
... Time Zero       %d
... Time Cycles     %d
... Time Mask       %d
... Cap Time Zero   %d
... Cap Time Short  %d`
	return fmt.Sprintf(
		f,
		tc.TimeShift(),
		tc.TimeMult(),
		tc.TimeZero(),
		tc.TimeCycles(),
		tc.TimeMask(),
		tc.CapUserTimeZero(),
		tc.CapUserTimeShort(),
	)
}

type Record interface {
	fmt.Stringer
}
