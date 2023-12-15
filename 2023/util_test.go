package twentytwentythree

import (
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

func Test_LPSArray(t *testing.T) {
	pattern := []int{87, 69, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63}
	want := []int{-1, 0, 1, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13}

	got := LPSArray(pattern[2:])
	AssertEquals(t, want, got, "LPSArray")
}

func Test_FindRepeatingPattern(t *testing.T) {
	pattern := []int{87, 69, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69, 69, 65, 64, 65, 63, 68, 69}
	want := []int{69, 69, 65, 64, 65, 63, 68}

	offset, got := FindRepeatingPattern(pattern)
	AssertEquals(t, want, got, "pattern")
	AssertEquals(t, uint(2), offset, "offset")
}
