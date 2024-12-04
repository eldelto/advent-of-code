package twentytwentyfour

import (
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

func IsIncreasing(l []int) (isIncreasing bool, maxStep int) {
	for i := range l {
		if i == 0 {
			continue
		}

		diff := l[i] - l[i-1]
		if diff <= 0 {
			return false, maxStep
		}
		maxStep = max(diff, maxStep)
	}

	return true, maxStep
}

func IsDecreasing(l []int) (isIncreasing bool, maxStep int) {
	for i := range l {
		if i == 0 {
			continue
		}

		diff := l[i] - l[i-1]
		if diff >= 0 {
			return false, maxStep
		}
		diff = int(Abs(diff))
		maxStep = max(diff, maxStep)
	}

	return true, maxStep
}

func isValidReport(report []int) bool {
	increasing, maxStep := IsIncreasing(report)
	if increasing && maxStep <= 3 {
		return true
	}

	decreasing, maxStep := IsDecreasing(report)
	if decreasing && maxStep <= 3 {
		return true
	}

	return false
}

func isValidReportWithDampener(report []int) bool {
	if isValidReport(report) {
		return true
	}

	for i := range report {
		dampenedReport := make([]int, len(report))
		copy(dampenedReport, report)
		dampenedReport = append(dampenedReport[:i], dampenedReport[i+1:]...)

		if isValidReport(dampenedReport) {
			return true
		}
	}

	return false
}

var input02, part1Test02, part2Test02 = InputsForDay(2)

func Test02Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test02)
	AssertNoError(t, err, "InputToLines")

	reports, err := MapWithErr(lines, StringToInts(" "))
	AssertNoError(t, err, "Split reports")

	validReports := Filter(reports, isValidReport)
	AssertEquals(t, 2, len(validReports), "Report count")
}

func Test02Part1(t *testing.T) {
	lines, err := InputToLines(input02)
	AssertNoError(t, err, "InputToLines")

	reports, err := MapWithErr(lines, StringToInts(" "))
	AssertNoError(t, err, "Split reports")

	validReports := Filter(reports, isValidReport)
	AssertEquals(t, 559, len(validReports), "Report count")
}

func Test02Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test02)
	AssertNoError(t, err, "InputToLines")

	reports, err := MapWithErr(lines, StringToInts(" "))
	AssertNoError(t, err, "Split reports")

	validReports := Filter(reports, isValidReportWithDampener)
	AssertEquals(t, 4, len(validReports), "Report count")
}

func Test02Part2(t *testing.T) {
	lines, err := InputToLines(input02)
	AssertNoError(t, err, "InputToLines")

	reports, err := MapWithErr(lines, StringToInts(" "))
	AssertNoError(t, err, "Split reports")

	validReports := Filter(reports, isValidReportWithDampener)
	AssertEquals(t, 601, len(validReports), "Report count")
}
