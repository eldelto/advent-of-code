package twentytwentyfour

import (
	"errors"
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input06, part1Test06, part2Test06 = InputsForDay(6)

type guard struct {
	pos Vec2
	dir Direction
}

func findGuard(matrix Matrix[GenericTile]) (guard, error) {
	for y, line := range matrix {
		for x, tile := range line {
			if tile.symbol == '^' {
				guard := guard{
					pos: Vec2{X: x, Y: y},
					dir: North,
				}
				return guard, nil
			}
		}
	}

	return guard{}, errors.New("no guard '^' in matrix")
}

func runGuardStep(guard *guard, field Matrix[GenericTile]) bool {
	nextPos := guard.pos.Add(Vec2(guard.dir))
	if !field.WithinBounds(nextPos) {
		return false
	}

	tile := field.Get(nextPos)
	switch tile.symbol {
	case '#':
		guard.dir = guard.dir.Right()
		return runGuardStep(guard, field)
	default:
		guard.pos = nextPos
	}

	return true
}

func findVisitedTiles(guard *guard, field Matrix[GenericTile]) map[Vec2]struct{} {
	visitedTiles := map[Vec2]struct{}{}
	visitedTiles[guard.pos] = struct{}{}

	for runGuardStep(guard, field) {
		visitedTiles[guard.pos] = struct{}{}
	}

	return visitedTiles
}

func makesGuardLoop(guard *guard, field Matrix[GenericTile]) bool {
	visitedTiles := map[Vec2]struct{}{}
	visitedTiles[guard.pos] = struct{}{}

	loopCounter := 0

	for runGuardStep(guard, field) {
		oldLen := len(visitedTiles)

		visitedTiles[guard.pos] = struct{}{}

		if len(visitedTiles) == oldLen {
			loopCounter++
		} else {
			loopCounter = 0
		}

		if loopCounter > 1000 {
			return true
		}
	}

	return false
}

func findGuardLoops(guard *guard, field Matrix[GenericTile]) int {
	startingPos := guard.pos
	visitedTiles := findVisitedTiles(guard, field)
	loopCount := 0

	for pos := range visitedTiles {
		guard.pos = startingPos
		guard.dir = North

		originalTile := field.Get(pos)
		field.Set(pos, GenericTile{symbol: '#'})

		if makesGuardLoop(guard, field) {
			loopCount++
		}

		field.Set(pos, originalTile)
	}

	return loopCount
}

func Test06Part1Test(t *testing.T) {
	matrix, err := InputToMatrix(part1Test06)
	AssertNoError(t, err, "InputToMatrix")

	guard, err := findGuard(matrix)
	AssertNoError(t, err, "find guard")

	steps := len(findVisitedTiles(&guard, matrix))
	AssertEquals(t, 41, steps, "guard steps")
}

func Test06Part1(t *testing.T) {
	matrix, err := InputToMatrix(input06)
	AssertNoError(t, err, "InputToMatrix")

	guard, err := findGuard(matrix)
	AssertNoError(t, err, "find guard")

	steps := len(findVisitedTiles(&guard, matrix))
	AssertEquals(t, 5404, steps, "guard steps")
}

func Test06Part2Test(t *testing.T) {
	matrix, err := InputToMatrix(part1Test06)
	AssertNoError(t, err, "InputToMatrix")

	guard, err := findGuard(matrix)
	AssertNoError(t, err, "find guard")

	loopCount := findGuardLoops(&guard, matrix)
	AssertEquals(t, 6, loopCount, "loop count ")
}

func Test06Part2(t *testing.T) {
	matrix, err := InputToMatrix(input06)
	AssertNoError(t, err, "InputToMatrix")

	guard, err := findGuard(matrix)
	AssertNoError(t, err, "find guard")

	loopCount := findGuardLoops(&guard, matrix)
	AssertEquals(t, 1984, loopCount, "loop count ")
}
