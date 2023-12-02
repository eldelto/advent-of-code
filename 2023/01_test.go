package twentytwentythree

import (
	"slices"
	"strings"
	"testing"
	"unicode"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input01, part1Test01, part2Test01 = InputsForDay(1)

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

func Test01Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test01)
	AssertNoError(t, err, "InputToLines")

	numbers := Map(lines, findHiddenNumber)
	sum := Sum(numbers)
	AssertEquals(t, 142, sum, "sum")
}

func Test01Part1(t *testing.T) {
	lines, err := InputToLines(input01)
	AssertNoError(t, err, "InputToLines")

	numbers := Map(lines, findHiddenNumber)
	sum := Sum(numbers)
	AssertEquals(t, 55029, sum, "sum")
}

func Test01Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test01)
	AssertNoError(t, err, "InputToLines")

	numbers := Map(lines, findRealHiddenNumber)
	sum := Sum(numbers)
	AssertEquals(t, 281, sum, "sum")
}

func Test01Part2(t *testing.T) {
	lines, err := InputToLines(input01)
	AssertNoError(t, err, "InputToLines")

	numbers := Map(lines, findRealHiddenNumber)
	sum := Sum(numbers)
	AssertEquals(t, 55686, sum, "sum")
}
