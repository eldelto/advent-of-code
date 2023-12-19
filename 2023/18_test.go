package twentytwentythree

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input18, part1Test18, _ = InputsForDay(18)

type trenchOperation struct {
	direction Direction
	amount    uint
	color     string
}

func parseTrenchOperation(line string) (trenchOperation, error) {
	parts := strings.Split(line, " ")

	var direction Direction
	switch parts[0] {
	case "U":
		direction = North
	case "D":
		direction = South
	case "L":
		direction = West
	case "R":
		direction = East
	default:
		return trenchOperation{}, fmt.Errorf("failed to parse direction from %q", line)
	}

	amount, err := strconv.ParseUint(parts[1], 10, 64)
	if err != nil {
		return trenchOperation{},
			fmt.Errorf("failed to parse direction amount from %q: %w", line, err)
	}

	return trenchOperation{
		direction: direction,
		amount:    uint(amount),
		color:     parts[2],
	}, nil
}

func parseRealTrenchOperation(line string) (trenchOperation, error) {
	parts := strings.Split(line, " ")
	hex := strings.Trim(parts[2], "()")[1:]

	var direction Direction
	switch hex[len(hex)-1] {
	case '0':
		direction = East
	case '1':
		direction = South
	case '2':
		direction = West
	case '3':
		direction = North
	default:
		return trenchOperation{}, fmt.Errorf("failed to parse direction from %q", line)
	}

	amount, err := strconv.ParseUint(hex[:len(hex)-1], 16, 64)
	if err != nil {
		return trenchOperation{},
			fmt.Errorf("failed to parse direction amount from %q: %w", line, err)
	}

	return trenchOperation{
		direction: direction,
		amount:    uint(amount),
		color:     parts[2],
	}, nil
}

func trenchOperationsToPoints(ops []trenchOperation) []Vec2 {
	points := []Vec2{}
	currentPos := Vec2{}

	for _, op := range ops {
		deltaPos := Vec2(op.direction).Scale(int(op.amount))
		currentPos = currentPos.Add(deltaPos)
		points = append(points, currentPos)
	}

	return points
}

func shoelaceArea(points []Vec2) int {
	area := 0
	for i := 1; i < len(points); i++ {
		area += points[i-1].CrossProduct(points[i])
	}

	return area / 2
}

func trenchArea(ops []trenchOperation) int {
	points := trenchOperationsToPoints(ops)
	area := shoelaceArea(points)
	border := Sum(Map(ops, func(o trenchOperation) int { return int(o.amount) }))

	return area + border/2 + 1
}

func Test18Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test18)
	AssertNoError(t, err, "InputToLines")

	ops, err := MapWithErr(lines, parseTrenchOperation)
	AssertNoError(t, err, "parseTrenchOperation")

	area := trenchArea(ops)
	AssertEquals(t, 62, area, "trench area")
}

func Test18Part1(t *testing.T) {
	lines, err := InputToLines(input18)
	AssertNoError(t, err, "InputToLines")

	ops, err := MapWithErr(lines, parseTrenchOperation)
	AssertNoError(t, err, "parseTrenchOperation")

	area := trenchArea(ops)
	AssertEquals(t, 49578, area, "trench area")
}

func Test18Part2Test(t *testing.T) {
	lines, err := InputToLines(part1Test18)
	AssertNoError(t, err, "InputToLines")

	ops, err := MapWithErr(lines, parseRealTrenchOperation)
	AssertNoError(t, err, "parseTrenchOperation")

	area := trenchArea(ops)
	AssertEquals(t, 952408144115, area, "trench area")
}

func Test18Part2(t *testing.T) {
	lines, err := InputToLines(input18)
	AssertNoError(t, err, "InputToLines")

	ops, err := MapWithErr(lines, parseRealTrenchOperation)
	AssertNoError(t, err, "parseTrenchOperation")

	area := trenchArea(ops)
	AssertEquals(t, 52885384955882, area, "trench area")
}
