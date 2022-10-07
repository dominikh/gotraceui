// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package trace

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestCorruptedInputs(t *testing.T) {
	// These inputs crashed parser previously.
	tests := []string{
		"gotrace\x00\x020",
		"gotrace\x00Q00\x020",
		"gotrace\x00T00\x020",
		"gotrace\x00\xc3\x0200",
		"go 1.5 trace\x00\x00\x00\x00\x020",
		"go 1.5 trace\x00\x00\x00\x00Q00\x020",
		"go 1.5 trace\x00\x00\x00\x00T00\x020",
		"go 1.5 trace\x00\x00\x00\x00\xc3\x0200",
	}
	for _, data := range tests {
		res, err := Parse(strings.NewReader(data))
		if err == nil || res.Events != nil || res.Stacks != nil {
			t.Fatalf("no error on input: %q", data)
		}
	}
}

func FuzzParse(f *testing.F) {
	// Seed with our existing, pre-fuzzing testdata.
	files, err := os.ReadDir("./testdata")
	if err != nil {
		f.Fatalf("failed to read ./testdata: %v", err)
	}
	for _, fi := range files {
		info, err := fi.Info()
		if err != nil {
			f.Fatal(err)
		}
		if info.IsDir() {
			continue
		}
		name := filepath.Join("./testdata", fi.Name())
		data, err := os.ReadFile(name)
		if err != nil {
			f.Fatal(err)
		}
		f.Add(data)
	}

	f.Fuzz(func(t *testing.T, in []byte) {
		// Trivial test that makes sure parsing terminates without crashing.
		Parse(bytes.NewReader(in))
	})
}

func BenchmarkParse(b *testing.B) {
	files, err := os.ReadDir("./testdata")
	if err != nil {
		b.Fatalf("failed to read ./testdata: %v", err)
	}
	var datas []struct {
		name string
		b    []byte
	}
	for _, f := range files {
		if !strings.HasSuffix(f.Name(), "_good") {
			continue
		}
		name := filepath.Join("./testdata", f.Name())
		data, err := os.ReadFile(name)
		if err != nil {
			b.Fatal(err)
		}
		datas = append(datas, struct {
			name string
			b    []byte
		}{f.Name(), data})
	}
	b.ResetTimer()

	for _, data := range datas {
		b.Run(data.name, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				_, err = Parse(bytes.NewReader(data.b))
				if err != nil {
					b.Errorf("failed to parse good trace %s: %v", data.name, err)
				}
			}
		})
	}
}

func TestParseVersion(t *testing.T) {
	tests := map[string]int{
		"go 1.5 trace\x00\x00\x00\x00": 1005,
		"go 1.7 trace\x00\x00\x00\x00": 1007,
		"go 1.10 trace\x00\x00\x00":    1010,
		"go 1.25 trace\x00\x00\x00":    1025,
		"go 1.234 trace\x00\x00":       1234,
		"go 1.2345 trace\x00":          -1,
		"go 0.0 trace\x00\x00\x00\x00": -1,
		"go a.b trace\x00\x00\x00\x00": -1,
	}
	for header, ver := range tests {
		ver1, err := parseHeader([]byte(header))
		if ver == -1 {
			if err == nil {
				t.Fatalf("no error on input: %q, version %v", header, ver1)
			}
		} else {
			if err != nil {
				t.Fatalf("failed to parse: %q (%v)", header, err)
			}
			if ver != ver1 {
				t.Fatalf("wrong version: %v, want %v, input: %q", ver1, ver, header)
			}
		}
	}
}
