package twentytwentythree

import (
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input12, part1Test12, part2Test12 = InputsForDay(12)

type springCondition struct {
	records  string
	expected []uint
}

func parseSpringCondition(line string) (springCondition, error) {
	parts := strings.Split(line, " ")
	records := parts[0]
	expected, err := StringToUInts(parts[1], ",")
	if err != nil {
		return springCondition{}, err
	}

	return springCondition{records, expected}, nil
}

func (sc *springCondition) canBeValid() bool {
	placeholderCount := strings.Count(sc.records, "?")
	damageCount := strings.Count(sc.records, "#")
	maxDamage := int(Sum(sc.expected))
	if damageCount > maxDamage || placeholderCount+damageCount < maxDamage {
		return false
	}

	damageClusters := strings.Split(strings.ReplaceAll(sc.records, "?", "."), ".")
	damageClusters = Filter(damageClusters, func(s string) bool { return s != "" })
	max := Max(sc.expected)
	for _, cluster := range damageClusters {
		if len(cluster) > int(max) {
			return false
		}
	}

	return true
}

func (sc *springCondition) isValid() bool {
	if strings.Contains(sc.records, "?") {
		return false
	}

	damageClusters := strings.Split(sc.records, ".")
	damageClusters = Filter(damageClusters, func(s string) bool { return s != "" })
	if len(damageClusters) != len(sc.expected) {
		return false
	}

	for i, cluster := range damageClusters {
		if len(cluster) != int(sc.expected[i]) {
			return false
		}
	}

	return true
}

func findValidSpringConditionPermutation(sc springCondition) int {
	if !strings.Contains(sc.records, "?") {
		if sc.isValid() {
			return 1
		}

		return 0
	}

	if !sc.canBeValid() {
		return 0
	}

	a := sc
	a.records = strings.Replace(sc.records, "?", "#", 1)
	b := sc
	b.records = strings.Replace(sc.records, "?", ".", 1)

	return findValidSpringConditionPermutation(a) + findValidSpringConditionPermutation(b)
}

func findValidUnfoldedSpringConditionPermutation(sc springCondition) int {
	start := sc
	start.records += "?"
	startPerms := findValidSpringConditionPermutation(start)

	middle := sc
	middle.records = "?" + middle.records + "?"
	middlePerms := findValidSpringConditionPermutation(middle)

	end := sc
	end.records = "?" + end.records
	endPerms := findValidSpringConditionPermutation(end)

	return startPerms * (3 * middlePerms) * endPerms
}

func Test12Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := Map(conditions, findValidSpringConditionPermutation)
	sum := Sum(permutations)
	AssertEquals(t, 21, sum, "sum of permutations")
}

func Test12Part1(t *testing.T) {
	lines, err := InputToLines(input12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := Map(conditions, findValidSpringConditionPermutation)
	sum := Sum(permutations)
	AssertEquals(t, 7025, sum, "sum of permutations")
}

func Test12Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := Map(conditions, findValidUnfoldedSpringConditionPermutation)
	sum := Sum(permutations)
	AssertEquals(t, 525152, sum, "sum of permutations")
}
