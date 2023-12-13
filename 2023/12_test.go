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

func parseUnfoldedSpringCondition(line string) (springCondition, error) {
	condition, err := parseSpringCondition(line)
	if err != nil {
		return springCondition{}, err
	}

	condition.records = strings.Repeat(condition.records+"?", 5)
	condition.records = condition.records[:len(condition.records)-1]
	condition.expected = Repeat(condition.expected, 5)
	return condition, nil
}

func (sc *springCondition) canBeValid() bool {
	placeholderCount := strings.Count(sc.records, "?")
	damageCount := strings.Count(sc.records, "#")
	maxDamage := int(Sum(sc.expected))
	if damageCount > maxDamage || placeholderCount+damageCount < maxDamage {
		return false
	}

	expectedLen := len(sc.expected)
	damageCount = 0
	i := 0
	for j, r := range sc.records {
		if r == '?' {
			return true
		} else if r == '#' {
			damageCount++
		}
		if r == '.' || j == len(sc.records)-1 {
			if damageCount > 0 {
				if i >= expectedLen {
					return false
				}
				if damageCount != int(sc.expected[i]) {
					return false
				}
				damageCount = 0
				i++
			}
		}
	}

	return true
}

func (sc *springCondition) canBeValid2() bool {
	placeholderCount := strings.Count(sc.records, "?")
	damageCount := strings.Count(sc.records, "#")
	maxDamage := int(Sum(sc.expected))
	if damageCount > maxDamage || placeholderCount+damageCount < maxDamage {
		return false
	}

	hashCount := 0
	expectedIndex := 0
	for _, r := range sc.records {
		if expectedIndex >= len(sc.expected) {
			return true
		}

		switch r {
		case '#':
			hashCount++
		case '.':
			if hashCount != 0 {
				if hashCount > int(sc.expected[expectedIndex]) {
					return false
				}
				expectedIndex++
			}
			hashCount = 0
		case '?':
			return true
		}
	}

	// minDamageClusters := strings.Split(strings.ReplaceAll(sc.records, "?", "."), ".")
	// minDamageClusters = Filter(minDamageClusters, func(s string) bool { return s != "" })
	// for i, cluster := range minDamageClusters {
	// 	if i >= len(sc.expected) {
	// 		break
	// 	}
	// 	if len(cluster) > int(sc.expected[i]) {
	// 		return false
	// 	}
	// 	if len(cluster) != int(sc.expected[i]) {
	// 		return true
	// 	}
	// }

	// maxDamageClusters := strings.Split(strings.ReplaceAll(sc.records, "?", "#"), ".")
	// maxDamageClusters = Filter(maxDamageClusters, func(s string) bool { return s != "" })
	// for i, cluster := range maxDamageClusters {
	// 	if i >= len(sc.expected) {
	// 		break
	// 	}
	// 	if strings.Count(cluster, "#") < int(sc.expected[i]) {
	// 		return false
	// 	}
	// }

	return true
}

func (sc *springCondition) isValid() bool {
	expectedLen := len(sc.expected)
	damageCount := 0
	i := 0
	for j, r := range sc.records {
		if r == '?' {
			return false
		} else if r == '#' {
			damageCount++
		}
		if r == '.' || j == len(sc.records)-1 {
			if damageCount > 0 {
				if i >= expectedLen {
					return false
				}
				if damageCount != int(sc.expected[i]) {
					return false
				}
				damageCount = 0
				i++
			}
		}
	}

	return i == expectedLen
}

func (sc *springCondition) isValid2() bool {
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

	conditions, err := MapWithErr(lines, parseUnfoldedSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := ParallelMap(conditions, findValidSpringConditionPermutation)
	sum := Sum(permutations)
	AssertEquals(t, 525152, sum, "sum of permutations")
}

func Test12Part2(t *testing.T) {
	lines, err := InputToLines(input12)
	AssertNoError(t, err, "InputToLines")

	conditions, err := MapWithErr(lines, parseUnfoldedSpringCondition)
	AssertNoError(t, err, "parseSpringCondition")

	permutations := Map(conditions, findValidSpringConditionPermutation)
	sum := Sum(permutations)
	AssertEquals(t, 525152, sum, "sum of permutations")
}
