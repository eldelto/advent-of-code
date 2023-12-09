package twentytwentythree

import (
	"fmt"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input09, part1Test09, part2Test09 = InputsForDay(9)

type oasisSensorValues []int

func (s oasisSensorValues) derivate() oasisSensorValues {
	result := make(oasisSensorValues, len(s)-1)
	for i := 0; i < len(s)-1; i++ {
		result[i] = s[i+1] - s[i]
	}

	return result
}

func (s oasisSensorValues) isConstant() bool {
	for i := 0; i < len(s)-1; i++ {
		if s[i] != s[i+1] {
			return false
		}
	}

	return true
}

func (s oasisSensorValues) nextValue() int {
	derivates := []oasisSensorValues{}
	currentDerivate := s
	for !currentDerivate.isConstant() {
		currentDerivate = currentDerivate.derivate()
		derivates = append(derivates, currentDerivate)
	}

	currentDelta := 0
	for i := len(derivates) - 1; i >= 0; i-- {
		derivate := derivates[i]
		currentDelta += derivate[len(derivate)-1]
	}

	return s[len(s)-1] + currentDelta
}

func (s oasisSensorValues) previousValue() int {
	derivates := []oasisSensorValues{}
	currentDerivate := s
	for !currentDerivate.isConstant() {
		currentDerivate = currentDerivate.derivate()
		derivates = append(derivates, currentDerivate)
	}

	currentDelta := 0
	for i := len(derivates) - 1; i >= 0; i-- {
		derivate := derivates[i]
		currentDelta = derivate[0] - currentDelta
	}

	return s[0] - currentDelta
}

func parseSensorValues(line string) (oasisSensorValues, error) {
	values, err := StringToInts(line, " ")
	if err != nil {
		return nil, fmt.Errorf("failed to parse %q as sensor values: %w", line, err)
	}

	return oasisSensorValues(values), err
}

func Test09Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test09)
	AssertNoError(t, err, "InputToLine")

	sensorValues, err := MapWithErr(lines, parseSensorValues)
	AssertNoError(t, err, "parseSensorValues")

	nexValues := Map(sensorValues, func(s oasisSensorValues) int { return s.nextValue() })
	sum := Sum(nexValues)
	AssertEquals(t, 114, sum, "sum")
}

func Test09Part1(t *testing.T) {
	lines, err := InputToLines(input09)
	AssertNoError(t, err, "InputToLine")

	sensorValues, err := MapWithErr(lines, parseSensorValues)
	AssertNoError(t, err, "parseSensorValues")

	nexValues := Map(sensorValues, func(s oasisSensorValues) int { return s.nextValue() })
	sum := Sum(nexValues)
	AssertEquals(t, 1987402313, sum, "sum")
}

func Test09Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test09)
	AssertNoError(t, err, "InputToLine")

	sensorValues, err := MapWithErr(lines, parseSensorValues)
	AssertNoError(t, err, "parseSensorValues")

	nexValues := Map(sensorValues, func(s oasisSensorValues) int { return s.previousValue() })
	sum := Sum(nexValues)
	AssertEquals(t, 2, sum, "sum")
}

func Test09Part2(t *testing.T) {
	lines, err := InputToLines(input09)
	AssertNoError(t, err, "InputToLine")

	sensorValues, err := MapWithErr(lines, parseSensorValues)
	AssertNoError(t, err, "parseSensorValues")

	nexValues := Map(sensorValues, func(s oasisSensorValues) int { return s.previousValue() })
	sum := Sum(nexValues)
	AssertEquals(t, 900, sum, "sum")
}
