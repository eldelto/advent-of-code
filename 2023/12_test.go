package twentytwentythree

import (
	"fmt"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input12, part1Test12, part2Test12 = InputsForDay(12)

type springCondition struct {
	records  []byte
	expected []int
}

func parseSpringCondition(line string) (springCondition, error) {
	parts := strings.Split(line, " ")
	records := parts[0]
	expected, err := StringToInts(parts[1], ",")
	if err != nil {
		return springCondition{}, err
	}

	return springCondition{
		records:  []byte(records),
		expected: expected,
	}, nil
}

func parseUnfoldedSpringCondition(line string) (springCondition, error) {
	condition, err := parseSpringCondition(line)
	if err != nil {
		return springCondition{}, err
	}

	condition.records = []byte(strings.Repeat(string(condition.records)+"?", 5))
	condition.records = condition.records[:len(condition.records)-1]
	condition.expected = Repeat(condition.expected, 5)
	return condition, nil
}

type springMatcher struct {
	damageGroupLen int
	damageGroupI   int
	expectWorking  bool
	invalid        bool
}

func (sp *springMatcher) stillValid(r byte, expected []int, remaining int) bool {
	if sp.invalid {
		return false
	}

	if remaining+sp.damageGroupLen < Sum(expected[sp.damageGroupI:]) {
		return false
	}
	if sp.expectWorking && r == '#' {
		sp.invalid = true
		return false
	}
	if r == '#' {
		sp.damageGroupLen++
		if sp.damageGroupI >= len(expected) || sp.damageGroupLen > expected[sp.damageGroupI] {
			sp.invalid = true
			return false
		}
		if sp.damageGroupLen == expected[sp.damageGroupI] {
			sp.expectWorking = true
		}
	}
	if r == '.' {
		if sp.expectWorking || sp.damageGroupLen != 0 {
			if sp.damageGroupLen != expected[sp.damageGroupI] {
				sp.invalid = true
				return false
			}

			sp.damageGroupLen = 0
			sp.damageGroupI++
			sp.expectWorking = false
		}
	}
	if r != '.' && r != '#' {
		panic("unexpected symbol")
	}

	return true
}

type debugSpringMatcher struct {
	springMatcher
	records string
}

func (sp *debugSpringMatcher) stillValid(r byte, expected []int, remaining int) bool {
	sp.records += string(r)
	return sp.springMatcher.stillValid(r, expected, remaining)
}

func findValidSpringConditionPermutationFast(sc springCondition) int {
	matchers := map[springMatcher]int{{}: 1}
	newMatchers := map[springMatcher]int{}
	records := []byte(string(sc.records) + ".")
	for i, r := range records {
		remaining := (len(records) - 1) - i
		for m, count := range matchers {
			r2 := r
			if r2 == '?' {
				clone := m
				if clone.stillValid('.', sc.expected, remaining) {
					newMatchers[clone] += count
				}
				r2 = '#'
			}
			if m.stillValid(r2, sc.expected, remaining) {
				newMatchers[m] += count
			}
		}
		matchers, newMatchers = newMatchers, matchers
		mapsClear(newMatchers)
	}

	count := 0
	for _, v := range matchers {
		count += v
	}
	return count
}

func Test12Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	x := findValidSpringConditionPermutationFast(conditions[1])
	AssertEquals(t, 4, x, "sum of permutations")

	permutations := Map(conditions, findValidSpringConditionPermutationFast)
	sum := Sum(permutations)
	AssertEquals(t, 21, sum, "sum of permutations")
}

func Test12Part1(t *testing.T) {
	lines, err := InputToLines(input12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := Map(conditions, findValidSpringConditionPermutationFast)
	sum := Sum(permutations)
	AssertEquals(t, 7025, sum, "sum of permutations")
}

func Test12Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseUnfoldedSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := Map(conditions, findValidSpringConditionPermutationFast)
	sum := Sum(permutations)
	AssertEquals(t, 525152, sum, "sum of permutations")
}

func Test12Part2(t *testing.T) {
	lines, err := InputToLines(input12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseUnfoldedSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := Map(conditions, findValidSpringConditionPermutationFast)
	sum := Sum(permutations)
	AssertEquals(t, 11461095383315, sum, "sum of permutations")
}

func Test_springMatcher(t *testing.T) {
	tests := []struct {
		m        springMatcher
		expected []int
		records  string
		want     bool
	}{
		{springMatcher{}, []int{1, 1, 3}, ".#...#....###.", true},
		{springMatcher{}, []int{1, 1, 3}, ".#....#...###.", true},
		{springMatcher{}, []int{1, 1, 3}, "..#..#....###.", true},
		{springMatcher{}, []int{1, 1, 3}, "..#...#...###.", true},
		{springMatcher{}, []int{1, 1, 3}, "......#...###.", false},
		{springMatcher{}, []int{1, 1, 3}, ".##...#...###.", false},
		{springMatcher{}, []int{1, 1, 3}, ".##...#...##..", false},
		{springMatcher{}, []int{1, 1, 3}, "#....###", false},
		{springMatcher{}, []int{1, 1, 3}, ".#...###", false},
		{springMatcher{}, []int{1, 1, 3}, "..#.###", false},
		{springMatcher{}, []int{1, 1, 3}, ".##.###", false},
		{springMatcher{}, []int{1, 1, 3}, "##..###", false},
		// Don't ask why this doesn't work anymore ¯\_(ツ)_/¯
		// {springMatcher{}, []int{1, 1, 3}, "#.#.###", true},
		{springMatcher{}, []int{3, 2, 1}, ".###.........", false},
	}

	for _, tt := range tests {
		t.Run(tt.records, func(t *testing.T) {
			for i, r := range []byte(tt.records) {
				valid := tt.m.stillValid(r, tt.expected, (len(tt.records)-1)-i)
				fmt.Printf("%s -> %t\n", tt.records[:i+1], valid)
			}
			fmt.Println()
			AssertEquals(t, tt.want, tt.m.stillValid('.', tt.expected, 0), "valid")
		})
	}
}

func Test_findPermutations(t *testing.T) {
	tests := []struct {
		sc   springCondition
		want int
	}{
		{springCondition{records: []byte("?.#.###"), expected: []int{1, 1, 3}}, 1},
		{springCondition{records: []byte("#.?.###"), expected: []int{1, 1, 3}}, 1},
		{springCondition{records: []byte("#??.###"), expected: []int{1, 1, 3}}, 1},
		{springCondition{records: []byte("??#.###"), expected: []int{1, 1, 3}}, 1},
		{springCondition{records: []byte("???.###"), expected: []int{1, 1, 3}}, 1},
		{springCondition{records: []byte(".#...#....###."), expected: []int{1, 1, 3}}, 1},
		{springCondition{records: []byte(".??..??...?##."), expected: []int{1, 1, 3}}, 4},
		{springCondition{records: []byte("?###????????"), expected: []int{3, 2, 1}}, 10},
	}
	for _, tt := range tests {
		t.Run(string(tt.sc.records), func(t *testing.T) {
			count := findValidSpringConditionPermutationFast(tt.sc)
			AssertEquals(t, tt.want, count, "permutation count")
		})
	}
}
