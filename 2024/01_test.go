package twentytwentyfour

import (
	"slices"
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input01, part1Test01, part2Test01 = InputsForDay(1)

func toLocationLists(lines []string)([]int, []int, error) {
	var left, right []int
	for _, line := range lines {
		ints, err := StringToInts("  ")(line)
		if err != nil {
			return nil, nil, err
		}

		if len(ints) < 2 {
			continue
		}

		left = append(left, ints[0])
		right = append(right, ints[1])
	}

	slices.Sort(left)
	slices.Sort(right)

	return left, right, nil
}

func Test01Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test01)
	AssertNoError(t, err, "InputToLines")

	left, right, err := toLocationLists(lines)
	AssertNoError(t, err, "toLocationList")

	diff := ZipMap(left, right, func(l, r int) uint { return Abs(r - l) })
	sum := Sum(diff)
	AssertEquals(t, uint(11), sum, "Sum")
}

func Test01Part1(t *testing.T) {
	lines, err := InputToLines(input01)
	AssertNoError(t, err, "InputToLines")

	left, right, err := toLocationLists(lines)
	AssertNoError(t, err, "toLocationList")

	diff := ZipMap(left, right, func(l, r int) uint { return Abs(r - l) })
	sum := Sum(diff)
	AssertEquals(t, uint(3508942), sum, "Sum")
}

func Test01Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test01)
	AssertNoError(t, err, "InputToLines")

	left, right, err := toLocationLists(lines)
	AssertNoError(t, err, "toLocationList")

	counts := Map(left, func(x int) uint { return Contains(x, right) })
	score := ZipMap(left, counts, func(l int, c uint) int { return l*int(c) })
	sum := Sum(score)
	AssertEquals(t, 31, sum, "Sum")
}

func Test01Part2(t *testing.T) {
	lines, err := InputToLines(input01)
	AssertNoError(t, err, "InputToLines")

	left, right, err := toLocationLists(lines)
	AssertNoError(t, err, "toLocationList")

	counts := Map(left, func(x int) uint { return Contains(x, right) })
	score := ZipMap(left, counts, func(l int, c uint) int { return l*int(c) })
	sum := Sum(score)
	AssertEquals(t, 26593248, sum, "Sum")
}
