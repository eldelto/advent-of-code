package twentytwentyfour

import (
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input04, part1Test04, part2Test04 = InputsForDay(4)

var (
	directions = []Direction{
		North,
		NorthEast,
		East,
		SouthEast,
		South,
		SouthWest,
		West,
		NorthWest,
	}

	xDirections = []Direction{
		NorthEast,
		SouthEast,
		SouthWest,
		NorthWest,
	}

	xmas = []rune("XMAS")
)

func couldBeXMAS(matrix Matrix[GenericTile], pos Vec2, index int) bool {
	if index >= len(xmas) || !matrix.WithinBounds(pos) {
		return false
	}

	tile := matrix.Get(pos)
	return tile.symbol == xmas[index]
}

func isXMAS(matrix Matrix[GenericTile], pos Vec2, direction Direction) bool {
	for i := range xmas {
		if !couldBeXMAS(matrix, pos, i) {
			return false
		}
		pos = pos.Add(Vec2(direction))
	}

	return true
}

func countXMASAllDirections(matrix Matrix[GenericTile], startPos Vec2) int {
	xmasCount := 0
	for _, direction := range directions {
		if isXMAS(matrix, startPos, direction) {
			xmasCount++
		}
	}

	return xmasCount
}

func countXMAS(matrix Matrix[GenericTile]) int {
	xmasCount := 0
	for y := range matrix {
		for x := range matrix[y] {
			pos := Vec2{X: x, Y: y}
			xmasCount += countXMASAllDirections(matrix, pos)
		}
	}

	return xmasCount
}

func tileNeighbours(matrix Matrix[GenericTile], pos Vec2) []GenericTile {
	neighbours := []GenericTile{}
	for _, dir := range xDirections {
		newPos := pos.Add(Vec2(dir))
		if matrix.WithinBounds(newPos) {
			neighbours = append(neighbours, matrix.Get(newPos))
		}
	}

	return neighbours
}

func isXShapedMAS(matrix Matrix[GenericTile], pos Vec2) bool {
	center := matrix.Get(pos)
	if !(string(center.symbol) == "A") {
		return false
	}

	neighbours := tileNeighbours(matrix, pos)
	if len(neighbours) != 4 {
		return false
	}

	mCount := 0
	sCount := 0
	for _, neighbour := range neighbours {
		switch string(neighbour.symbol) {
		case "M":
			mCount++
		case "S":
			sCount++
		}
	}

	return mCount == 2 && sCount == 2 &&
		neighbours[0].symbol != neighbours[2].symbol &&
		neighbours[1].symbol != neighbours[3].symbol
}

func countXShapedMAS(matrix Matrix[GenericTile]) int {
	xmasCount := 0
	for y := range matrix {
		for x := range matrix[y] {
			pos := Vec2{X: x, Y: y}
			if isXShapedMAS(matrix, pos) {
				xmasCount++
			}
		}
	}

	return xmasCount
}

func Test04Part1Test(t *testing.T) {
	matrix, err := InputToMatrix(part1Test04)
	AssertNoError(t, err, "InputToMatrix")

	count := countXMAS(matrix)
	AssertEquals(t, 18, count, "XMAS count")
}

func Test04Part1(t *testing.T) {
	matrix, err := InputToMatrix(input04)
	AssertNoError(t, err, "InputToMatrix")

	count := countXMAS(matrix)
	AssertEquals(t, 2458, count, "XMAS count")
}

func Test04Part2Test(t *testing.T) {
	matrix, err := InputToMatrix(part2Test04)
	AssertNoError(t, err, "InputToMatrix")

	count := countXShapedMAS(matrix)
	AssertEquals(t, 9, count, "XMAS count")
}

func Test04Part2(t *testing.T) {
	matrix, err := InputToMatrix(input04)
	AssertNoError(t, err, "InputToMatrix")

	count := countXShapedMAS(matrix)
	AssertEquals(t, 1945, count, "XMAS count")
}
