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

	direction := North
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

type trenchTile struct {
	dugOut bool
	color  string
}

func (t *trenchTile) String() string {
	if t.dugOut {
		return "#"
	}

	return "."
}

func initTrenchMatrix(width, height int) Matrix[trenchTile] {
	matrix := make(Matrix[trenchTile], height)
	for ri := range matrix {
		matrix[ri] = make([]trenchTile, width)
	}

	return matrix
}

func trenchDimensions(ops []trenchOperation) (int, int, Vec2) {
	leftCorner := Vec2{0, 0}
	rightCorner := Vec2{0, 0}
	currentPos := Vec2{}
	for _, op := range ops {
		for i := 0; i < int(op.amount); i++ {
			currentPos = currentPos.Add(Vec2(op.direction))
			if currentPos.X < leftCorner.X {
				leftCorner.X = currentPos.X
			}
			if currentPos.Y < leftCorner.Y {
				leftCorner.Y = currentPos.Y
			}
			if currentPos.X > rightCorner.X {
				rightCorner.X = currentPos.X
			}
			if currentPos.Y > rightCorner.Y {
				rightCorner.Y = currentPos.Y
			}
		}
	}

	return (rightCorner.X - leftCorner.X) + 1, (rightCorner.Y - leftCorner.Y) + 1, leftCorner.Scale(-1)
}

func digTrench(m Matrix[trenchTile], ops []trenchOperation, startPos Vec2) {
	currentPos := startPos
	for _, op := range ops {
		tile := trenchTile{
			dugOut: true,
			color:  op.color,
		}
		m.Set(currentPos, tile)

		for i := 0; i < int(op.amount); i++ {
			currentPos = currentPos.Add(Vec2(op.direction))
			m.Set(currentPos, tile)
		}
	}
}

func floodFill[T any](m Matrix[T], pos Vec2, isBorder func(x T) bool, fill func(old T) T, done map[Vec2]struct{}) {
	if _, ok := done[pos]; ok {
		return
	}

	tile := m.Get(pos)
	if isBorder(tile) {
		return
	}

	m.Set(pos, fill(tile))
	done[pos] = struct{}{}

	positions := []Vec2{
		pos.Add(Vec2(North)),
		pos.Add(Vec2(East)),
		pos.Add(Vec2(South)),
		pos.Add(Vec2(West)),
	}

	for _, pos := range positions {
		floodFill(m, pos, isBorder, fill, done)
	}
}

func excavateTrenchLoop(m Matrix[trenchTile], seed Vec2) {
	done := map[Vec2]struct{}{}

	floodFill(m, seed,
		func(x trenchTile) bool { return x.dugOut },
		func(old trenchTile) trenchTile {
			old.dugOut = true
			return old
		},
		done)
}

func countTrenchTiles(m Matrix[trenchTile]) int {
	sum := 0
	for _, row := range m {
		for _, tile := range row {
			if tile.dugOut {
				sum++
			}
		}
	}

	return sum
}

func printTrench(m Matrix[trenchTile]) {
	for ri, row := range m {
		for ci := range row {
			fmt.Print(m[ri][ci].String())
		}
		fmt.Println()
	}
	fmt.Println()
}

func Test18Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test18)
	AssertNoError(t, err, "InputToLines")

	ops, err := MapWithErr(lines, parseTrenchOperation)
	AssertNoError(t, err, "parseTrenchOperation")

	width, height, startPos := trenchDimensions(ops)
	matrix := initTrenchMatrix(width, height)
	digTrench(matrix, ops, startPos)
	printTrench(matrix)
	excavateTrenchLoop(matrix, Vec2{1, 1})
	printTrench(matrix)

	sum := countTrenchTiles(matrix)
	AssertEquals(t, 62, sum, "sum of trench tiles")
}

func Test18Part1(t *testing.T) {
	lines, err := InputToLines(input18)
	AssertNoError(t, err, "InputToLines")

	ops, err := MapWithErr(lines, parseTrenchOperation)
	AssertNoError(t, err, "parseTrenchOperation")

	width, height, startPos := trenchDimensions(ops)
	matrix := initTrenchMatrix(width, height)
	digTrench(matrix, ops, startPos)
	printTrench(matrix)
	excavateTrenchLoop(matrix, Vec2{30, 10})
	printTrench(matrix)

	sum := countTrenchTiles(matrix)
	AssertEquals(t, 49578, sum, "sum of trench tiles")
}
