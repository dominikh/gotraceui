package tinylfu

import "testing"

func TestAddAlreadyInCache(t *testing.T) {
	c := New[string, string](100, 10000)

	c.Add("foo", "bar")

	val, _ := c.Get("foo")
	if val != "bar" {
		t.Errorf("c.Get(foo)=%q, want %q", val, "bar")
	}

	c.Add("foo", "baz")

	val, _ = c.Get("foo")
	if val != "baz" {
		t.Errorf("c.Get(foo)=%q, want %q", val, "baz")
	}
}

var SinkString string
var SinkBool bool

func BenchmarkGet(b *testing.B) {
	t := New[string, string](64, 640)
	key := "some arbitrary key"
	val := "some arbitrary value"
	t.Add(key, val)
	for b.Loop() {
		SinkString, SinkBool = t.Get(key)
	}
}
