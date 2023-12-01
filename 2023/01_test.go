package twentytwentythree

import (
	"slices"
	"strings"
	"testing"
	"unicode"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var part1Test, part1, part2Test, part2 = InputsForDay(1)

func findFirstDigit(s string) int {
	for _, r := range s {
		if unicode.IsDigit(r) {
			return int(r - '0')
		}
	}

	return 0
}

func findHiddenNumber(s string) int {
	number := findFirstDigit(s) * 10
	b := []byte(s)
	slices.Reverse(b)
	return number + findFirstDigit(string(b))
}

var numbers = []string{
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
}

func startsWithWrittenNumber(s string) int {
	for i, number := range numbers {
		if strings.HasPrefix(s, number) {
			return i + 1
		}
	}

	return 0
}

func findRealHiddenNumber(s string) int {
	var firstMatch, lastMatch int
	for i, r := range s {
		if unicode.IsDigit(r) {
			n := int(r - '0')
			if firstMatch == 0 {
				firstMatch = n
			}
			lastMatch = n
			continue
		}

		if n := startsWithWrittenNumber(s[i:]); n != 0 {
			if firstMatch == 0 {
				firstMatch = n
			}
			lastMatch = n
		}
	}

	return firstMatch*10 + lastMatch
}

func forEach[A, B any](a []A, f func(a A) B) []B {
	result := make([]B, len(a))
	for i := range a {
		result[i] = f(a[i])
	}

	return result
}

func sum(l []int) int {
	sum := 0
	for _, n := range l {
		sum += n
	}

	return sum
}

func Test01Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test)
	AssertNoError(t, err, "InputToLines")

	numbers := forEach(lines, findHiddenNumber)
	sum := sum(numbers)
	AssertEquals(t, 142, sum, "sum")
}

func Test01Part1(t *testing.T) {
	lines, err := InputToLines(part1)
	AssertNoError(t, err, "InputToLines")

	numbers := forEach(lines, findHiddenNumber)
	sum := sum(numbers)
	AssertEquals(t, 55029, sum, "sum")
}

func Test01Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test)
	AssertNoError(t, err, "InputToLines")

	numbers := forEach(lines, findRealHiddenNumber)
	sum := sum(numbers)
	AssertEquals(t, 281, sum, "sum")
}

func Test01Part2(t *testing.T) {
	lines, err := InputToLines(part2)
	AssertNoError(t, err, "InputToLines")

	numbers := forEach(lines, findRealHiddenNumber)
	sum := sum(numbers)
	AssertEquals(t, 55686, sum, "sum")
}
