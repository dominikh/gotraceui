package main

func grow[E any, S ~[]E](x S, n int) S {
	if len(x) > n {
		return x
	}
	if cap(x) >= n {
		return x[:n]
	}
	y := make(S, n)
	copy(y, x)
	return y
}
