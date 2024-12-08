package twentytwentyfour

import (
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input08, part1Test08, part2Test08 = InputsForDay(8)

func findAntennas(matrix Matrix[GenericTile]) []GenericTile {
	return matrix.Filter(func(tile GenericTile) bool {
		return tile.symbol != '.'
	})
}

func calculateFrequencyNodes(matrix Matrix[GenericTile], a1, a2 GenericTile) []Vec2 {
	result := []Vec2{}

	distance := a2.pos.Sub(a1.pos)
	node1 := a1.pos.Sub(distance)
	if matrix.WithinBounds(node1) {
		result = append(result, node1)
	}
	node2 := a2.pos.Add(distance)
	if matrix.WithinBounds(node2) {
		result = append(result, node2)
	}

	return result
}

func findFrequencyNodes(matrix Matrix[GenericTile], antennas []GenericTile) map[Vec2]struct{} {
	result := map[Vec2]struct{}{}
	for _, a1 := range antennas {
		for _, a2 := range antennas {
			if a1.pos == a2.pos || a1.symbol != a2.symbol {
				continue
			}

			nodes := calculateFrequencyNodes(matrix, a1, a2)
			for _, n := range nodes {
				result[n] = struct{}{}
			}
		}
	}

	return result
}

func calculateFrequencyNodes2(matrix Matrix[GenericTile], a1, a2 GenericTile) []Vec2 {
	result := []Vec2{a1.pos, a2.pos}

	distance := a2.pos.Sub(a1.pos)

	for node1 := a1.pos.Sub(distance); matrix.WithinBounds(node1); node1 = node1.Sub(distance) {
		result = append(result, node1)
	}
	for node2 := a2.pos.Add(distance); matrix.WithinBounds(node2); node2 = node2.Add(distance) {
		result = append(result, node2)
	}

	return result
}

func findFrequencyNodes2(matrix Matrix[GenericTile], antennas []GenericTile) map[Vec2]struct{} {
	result := map[Vec2]struct{}{}
	for _, a1 := range antennas {
		for _, a2 := range antennas {
			if a1.pos == a2.pos || a1.symbol != a2.symbol {
				continue
			}

			nodes := calculateFrequencyNodes2(matrix, a1, a2)
			for _, n := range nodes {
				result[n] = struct{}{}
			}
		}
	}

	return result
}

func Test08Part1Test(t *testing.T) {
	matrix, err := InputToMatrix(part1Test08)
	AssertNoError(t, err, "InputToMatrix")

	antennas := findAntennas(matrix)
	nodes := findFrequencyNodes(matrix, antennas)

	AssertEquals(t, 14, len(nodes), "node count")
}

func Test08Part1(t *testing.T) {
	matrix, err := InputToMatrix(input08)
	AssertNoError(t, err, "InputToMatrix")

	antennas := findAntennas(matrix)
	nodes := findFrequencyNodes(matrix, antennas)

	AssertEquals(t, 364, len(nodes), "node count")
}

func Test08Part2Test(t *testing.T) {
	matrix, err := InputToMatrix(part1Test08)
	AssertNoError(t, err, "InputToMatrix")

	antennas := findAntennas(matrix)
	nodes := findFrequencyNodes2(matrix, antennas)

	AssertEquals(t, 34, len(nodes), "node count")
}

func Test08Part2(t *testing.T) {
	matrix, err := InputToMatrix(input08)
	AssertNoError(t, err, "InputToMatrix")

	antennas := findAntennas(matrix)
	nodes := findFrequencyNodes2(matrix, antennas)

	AssertEquals(t, 1231, len(nodes), "node count")
}
