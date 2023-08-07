package main

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	rdebug "runtime/debug"
)

const Version = "v0.3.0"

// version returns a version descriptor and reports whether the
// version is a known release.
func version(human string) (_ string, known bool) {
	if human != "devel" {
		return human, true
	}
	v, ok := buildInfoVersion()
	if ok {
		return v, false
	}
	return "devel", false
}

func PrintVersion(human string) {
	human, release := version(human)

	if release {
		fmt.Printf("%s %s\n", filepath.Base(os.Args[0]), human)
	} else if human == "devel" {
		fmt.Printf("%s (no version)\n", filepath.Base(os.Args[0]))
	} else {
		fmt.Printf("%s (devel, %s)\n", filepath.Base(os.Args[0]), human)
	}
}

func PrintVerboseVersion(human string) {
	PrintVersion(human)
	fmt.Println()
	fmt.Println("Compiled with Go version:", runtime.Version())
	printBuildInfo()
}

func printBuildInfo() {
	if info, ok := rdebug.ReadBuildInfo(); ok {
		fmt.Println("Main module:")
		printModule(&info.Main)
		fmt.Println("Dependencies:")
		for _, dep := range info.Deps {
			printModule(dep)
		}
	} else {
		fmt.Println("Built without Go modules")
	}
}

func buildInfoVersion() (string, bool) {
	info, ok := rdebug.ReadBuildInfo()
	if !ok {
		return "", false
	}
	if info.Main.Version == "(devel)" {
		return "", false
	}
	return info.Main.Version, true
}

func printModule(m *rdebug.Module) {
	fmt.Printf("\t%s", m.Path)
	if m.Version != "(devel)" {
		fmt.Printf("@%s", m.Version)
	}
	if m.Sum != "" {
		fmt.Printf(" (sum: %s)", m.Sum)
	}
	if m.Replace != nil {
		fmt.Printf(" (replace: %s)", m.Replace.Path)
	}
	fmt.Println()
}
