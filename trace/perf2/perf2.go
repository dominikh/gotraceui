package main

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"log"
	"math"
	"math/bits"
	"os"
	"strings"
	"unsafe"

	"golang.org/x/exp/slices"
	"honnef.co/go/gotraceui/container"
	myunsafe "honnef.co/go/gotraceui/unsafe"
)

//go:generate stringer -type HeaderType,RecordType,eventType
type HeaderType uint8
type RecordType uint32
type SampleFlag uint64
type eventType uint32

const (
	HEADER_FIRST_FEATURE   HeaderType = 1
	HEADER_TRACING_DATA    HeaderType = 1
	HEADER_BUILD_ID        HeaderType = 2
	HEADER_HOSTNAME        HeaderType = 3
	HEADER_OSRELEASE       HeaderType = 4
	HEADER_VERSION         HeaderType = 5
	HEADER_ARCH            HeaderType = 6
	HEADER_NRCPUS          HeaderType = 7
	HEADER_CPUDESC         HeaderType = 8
	HEADER_CPUID           HeaderType = 9
	HEADER_TOTAL_MEM       HeaderType = 10
	HEADER_CMDLINE         HeaderType = 11
	HEADER_EVENT_DESC      HeaderType = 12
	HEADER_CPU_TOPOLOGY    HeaderType = 13
	HEADER_NUMA_TOPOLOGY   HeaderType = 14
	HEADER_BRANCH_STACK    HeaderType = 15
	HEADER_PMU_MAPPINGS    HeaderType = 16
	HEADER_GROUP_DESC      HeaderType = 17
	HEADER_AUXTRACE        HeaderType = 18
	HEADER_STAT            HeaderType = 19
	HEADER_CACHE           HeaderType = 20
	HEADER_SAMPLE_TIME     HeaderType = 21
	HEADER_MEM_TOPOLOGY    HeaderType = 22
	HEADER_CLOCKID         HeaderType = 23
	HEADER_DIR_FORMAT      HeaderType = 24
	HEADER_BPF_PROG_INFO   HeaderType = 25
	HEADER_BPF_BTF         HeaderType = 26
	HEADER_COMPRESSED      HeaderType = 27
	HEADER_CPU_PMU_CAPS    HeaderType = 28
	HEADER_CLOCK_DATA      HeaderType = 29
	HEADER_HYBRID_TOPOLOGY HeaderType = 30
	HEADER_PMU_CAPS        HeaderType = 31
	HEADER_LAST_FEATURE    HeaderType = 32
)

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

const (
	PERF_TYPE_HARDWARE   eventType = 0
	PERF_TYPE_SOFTWARE   eventType = 1
	PERF_TYPE_TRACEPOINT eventType = 2
	PERF_TYPE_HW_CACHE   eventType = 3
	PERF_TYPE_RAW        eventType = 4
	PERF_TYPE_BREAKPOINT eventType = 5

	PERF_TYPE_NUM = 6
)

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

func (st SampleFlag) String() string {
	ss := make([]string, 0, HEADER_LAST_FEATURE)
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

type bp struct {
	// bp_addr or kprobe_func or uprobe_path or config1
	a uint64
	// bp_len or kprobe_addr or kprobe_offset or config2
	b uint64
}

type fileHeader struct {
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

type flags [4]uint64

func (fl *flags) bit(x HeaderType) bool {
	return fl[x/64]&(1<<(x-(x/64)*64)) != 0
}

func (fl *flags) String() string {
	// XXX include unknown bits in the output
	ss := make([]string, 0, HEADER_LAST_FEATURE)
	for i := HEADER_FIRST_FEATURE; i < HEADER_LAST_FEATURE; i++ {
		if fl.bit(i) {
			ss = append(ss, i.String())
		}
	}
	return strings.Join(ss, " | ")
}

type attrFlags uint64

type fileEventAttr struct {
	eventAttr
	ids fileSection
}

type File struct {
	// OPT(dh): check if we want buffering
	r     bufferedSeeker
	hdr   fileHeader
	attrs []fileEventAttr
	// Map from event IDs to event descriptions
	eventAttrs map[uint64]*fileEventAttr
	Machine    Machine

	// XXX this should be a union, only one can be set
	sharedType   container.Option[SampleFlag]
	sharedOffset container.Option[uint64]

	scratch [8]byte
}

func NewFile(r io.ReadSeeker) (*File, error) {
	f := &File{
		r: bufferedSeeker{
			r:      r,
			Reader: bufio.NewReader(r),
		},
		eventAttrs: make(map[uint64]*fileEventAttr),
	}
	err := f.init()
	return f, err
}

func (f *File) init() error {
	if _, err := io.ReadFull(f.r, myunsafe.AsBytes(&f.hdr)); err != nil {
		return fmt.Errorf("couldn't read header: %w", err)
	}
	if f.hdr.attrSize != uint64(unsafe.Sizeof(fileEventAttr{})) {
		return fmt.Errorf("unsupported attribute size %d", f.hdr.attrSize)
	}

	off := f.hdr.data.offset + f.hdr.data.size
	if err := f.r.Seek(off); err != nil {
		return fmt.Errorf("couldn't seek to %d: %w", off, err)
	}

	var sections [HEADER_LAST_FEATURE]fileSection
	for i := HEADER_FIRST_FEATURE; i < HEADER_LAST_FEATURE; i++ {
		if !f.hdr.flags.bit(i) {
			continue
		}
		if err := f.u64(&sections[i].offset, &sections[i].size); err != nil {
			return err
		}
	}

	for i, s := range sections {
		if s.size == 0 {
			continue
		}
		if err := f.r.Seek(s.offset); err != nil {
			return err
		}

		var err error
		switch HeaderType(i) {
		case HEADER_TRACING_DATA:
		case HEADER_BUILD_ID:
		case HEADER_HOSTNAME:
			f.Machine.Hostname, err = f.str()
		case HEADER_OSRELEASE:
			f.Machine.Release, err = f.str()
		case HEADER_VERSION:
			f.Machine.Version, err = f.str()
		case HEADER_ARCH:
			f.Machine.Architecture, err = f.str()
		case HEADER_NRCPUS:
			f.u32(&f.Machine.CPUsAvailable, &f.Machine.CPUsOnline)
		case HEADER_CPUDESC:
			f.Machine.CPUDesc, err = f.str()
		case HEADER_CPUID:
			f.Machine.CPUID, err = f.str()
		case HEADER_TOTAL_MEM:
			err = f.u64(&f.Machine.TotalMemory)
			f.Machine.TotalMemory *= 1024
		case HEADER_CMDLINE:
		case HEADER_EVENT_DESC:
		case HEADER_CPU_TOPOLOGY:
		case HEADER_NUMA_TOPOLOGY:
		case HEADER_BRANCH_STACK:
		case HEADER_PMU_MAPPINGS:
		case HEADER_GROUP_DESC:
		case HEADER_AUXTRACE:
		case HEADER_STAT:
		case HEADER_CACHE:
		case HEADER_SAMPLE_TIME:
		case HEADER_MEM_TOPOLOGY:
		case HEADER_CLOCKID:
		case HEADER_DIR_FORMAT:
		case HEADER_BPF_PROG_INFO:
		case HEADER_BPF_BTF:
		case HEADER_COMPRESSED:
		case HEADER_CPU_PMU_CAPS:
		case HEADER_CLOCK_DATA:
		case HEADER_HYBRID_TOPOLOGY:
		case HEADER_PMU_CAPS:
		}
		if err != nil {
			return err
		}
	}

	if f.hdr.attrs.size != 0 {
		f.r.Seek(f.hdr.attrs.offset)
		if f.hdr.attrs.size > math.MaxInt64 {
			return fmt.Errorf("size %d doesn't fit in int64", f.hdr.attrs.size)
		}
		r := io.LimitReader(f.r, int64(f.hdr.attrs.size))
		var attrs []fileEventAttr
		for {
			attrs = slices.Grow(attrs, 1)[:len(attrs)+1]
			if _, err := io.ReadFull(r, myunsafe.AsBytes(&attrs[len(attrs)-1])); err != nil {
				if err == io.EOF {
					attrs = attrs[:len(attrs)-1]
					break
				} else {
					return fmt.Errorf("couldn't read attribute: %w", err)
				}
			}
		}
		f.attrs = attrs

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

			if attr.ids.size != 0 {
				f.r.Seek((attr.ids.offset))
			}
			for j := uint64(0); j < attr.ids.size/8; j++ {
				var id uint64
				if err := f.u64(&id); err != nil {
					return fmt.Errorf("couldn't read attribute ID: %w", err)
				}
				f.eventAttrs[id] = attr
			}
		}

		// Figure out how to reliably determine the types of samples. When there are multiple event sources in a
		// single file, then different sources may have different sample types, i.e. have different sets of
		// fields. If they do have different types, then we must look them up before parsing. For this, we need
		// the samples' event IDs. If all sample_types include SampleTypeIdentifier, then the IDs are the first
		// field in the sample. If all sample_types include SampleTypeID, and the ID is always at the same offset,
		// then that's the offset we'll use. In all other cases we cannot reliably parse the file.
		if allShareType {
			f.sharedType = container.Some(attrs[0].sampleType)
		} else {
			if allHaveIdentifier {
				f.sharedOffset = container.Some[uint64](0)
			} else if allHaveID {
				idOffset := func(typ SampleFlag) uint64 {
					mask := PERF_SAMPLE_IDENTIFIER |
						PERF_SAMPLE_IP |
						PERF_SAMPLE_TID |
						PERF_SAMPLE_TIME |
						PERF_SAMPLE_ADDR
					set := bits.OnesCount64(uint64(typ & mask))
					return uint64(set) * 8
				}
				sharedOffset := idOffset(attrs[0].sampleType)
				for i := range attrs {
					if off := idOffset(attrs[i].sampleType); off != sharedOffset {
						panic("multiple events but no reliable identifier to identify them")
					}
				}
				f.sharedOffset = container.Some(sharedOffset)
			} else {
				return errors.New("multiple events but no reliable identifier to identify them")
			}
		}
	}

	// Seek to data so user can start reading events
	f.r.Seek(f.hdr.data.offset)

	return nil
}

type Machine struct {
	Hostname      string
	Release       string
	Version       string
	Architecture  string
	CPUDesc       string
	CPUID         string
	CPUsAvailable uint32
	CPUsOnline    uint32
	// Total system memory, in bytes
	TotalMemory uint64
}

func (f *File) u32(dsts ...*uint32) error {
	for _, dst := range dsts {
		if _, err := io.ReadFull(f.r, f.scratch[:4]); err != nil {
			return fmt.Errorf("couldn't read u32: %w", err)
		}
		*dst = binary.LittleEndian.Uint32(f.scratch[:4])
	}
	return nil
}

func (f *File) u64(dsts ...*uint64) error {
	for _, dst := range dsts {
		if _, err := io.ReadFull(f.r, f.scratch[:8]); err != nil {
			return fmt.Errorf("couldn't read u64: %w", err)
		}
		*dst = binary.LittleEndian.Uint64(f.scratch[:8])
	}
	return nil
}

func (f *File) str() (string, error) {
	var n uint32
	if err := f.u32(&n); err != nil {
		return "", fmt.Errorf("couldn't read string: %w", err)
	}
	if n == 0 {
		return "", nil
	}
	// XXX guard against absurdly large n, in the style of saferio
	b := make([]byte, n)
	if _, err := io.ReadFull(f.r, b); err != nil {
		return "", fmt.Errorf("couldn't read string: %w", err)
	}
	// XXX verify there's a zero
	off := bytes.IndexByte(b, 0)
	if off == -1 {
		return "", fmt.Errorf("found unterminated string")
	}
	return string(b[:off]), nil
}

func main() {
	r, err := os.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	defer r.Close()
	f, err := NewFile(r)
	if err != nil {
		log.Fatal(err)
	}
}

type bufferedSeeker struct {
	r io.ReadSeeker
	*bufio.Reader
}

func (bs *bufferedSeeker) Seek(offset uint64) error {
	if offset > math.MaxInt64 {
		return fmt.Errorf("offset %d doesn't fit in int64", offset)
	}
	if _, err := bs.r.Seek(int64(offset), io.SeekStart); err != nil {
		return err
	}
	bs.Reset(bs.r)
	return nil
}
