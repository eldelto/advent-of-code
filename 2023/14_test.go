package twentytwentythree

import (
	"fmt"
	"path/filepath"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input14, part1Test14, part2Test14 = InputsForDay(14)

func slideRock(matrix [][]genericTile, pos Vec2, direction Direction) bool {
	slid := false
	for {
		tile := matrix[pos.Y][pos.X]
		if tile.symbol != 'O' {
			panic(fmt.Sprintf("not a rock at %v", pos))
		}

		above := pos.Add(Vec2(direction))
		if above.Y < 0 || above.Y >= len(matrix) ||
			above.X < 0 || above.X >= len(matrix[0]) {
			break
		}

		target := matrix[above.Y][above.X]
		if target.symbol == 'O' {
			if !slideRock(matrix, above, direction) {
				break
			}
			continue
		}
		if target.symbol != '.' {
			break
		}

		matrix[above.Y][above.X] = tile
		matrix[pos.Y][pos.X] = target
		slid = true
		pos = above
	}

	return slid
}

func slideRocks(matrix [][]genericTile, direction Direction) {
	for ri, row := range matrix {
		for ci := range row {
			if matrix[ri][ci].symbol == 'O' {
				slideRock(matrix, Vec2{ci, ri}, direction)
			}
		}
	}
}

type cycleCounter struct {
	value int
	lastI int
	cycle int
}

func cycleRocks(matrix [][]genericTile) {
	cycles := map[int]cycleCounter{}
	cache := map[int][][]genericTile{}
	oldWeight := 0
	directions := []Direction{North, West, South, East}
	for i := 0; i < 1000000000; i++ {
		m, ok := cache[oldWeight]
		if ok {
			matrix = m
			continue
		}
		for _, direction := range directions {
			slideRocks(matrix, direction)
		}
		weight := weighRocks(matrix)
		cycle := cycles[weight]
		cycles[weight] = cycleCounter{value: weight, lastI: i, cycle: i - cycle.lastI}
		cache[oldWeight] = matrix
		oldWeight = weight
		// fmt.Println(weighRocks(matrix))
		// printMatrix(matrix)
	}
	fmt.Println(cycles)
}

func printMatrix(matrix [][]genericTile) {
	for ri, row := range matrix {
		for ci := range row {
			fmt.Printf("%c", matrix[ri][ci].symbol)
		}
		fmt.Println()
	}
	fmt.Println()
}

func weighRocks(matrix [][]genericTile) int {
	weight := 0
	for ri, row := range matrix {
		for ci := range row {
			if matrix[ri][ci].symbol == 'O' {
				weight += len(matrix) - ri
			}
		}
	}

	return weight
}

// TODO: Refactor to matrix type with get,set,string methods

func Test14Part1Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part1Test14))
	AssertNoError(t, err, "open file")
	defer file.Close()

	matrix := ParseGenericMatrix(file)
	slideRocks(matrix, North)

	weight := weighRocks(matrix)
	AssertEquals(t, 136, weight, "weight of rocks")
}

func Test14Part1(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input14))
	AssertNoError(t, err, "open file")
	defer file.Close()

	matrix := ParseGenericMatrix(file)
	slideRocks(matrix, North)

	weight := weighRocks(matrix)
	AssertEquals(t, 112773, weight, "weight of rocks")
}

func Test14Part2Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part1Test14))
	AssertNoError(t, err, "open file")
	defer file.Close()

	matrix := ParseGenericMatrix(file)
	cycleRocks(matrix)

	weight := weighRocks(matrix)
	AssertEquals(t, 136, weight, "weight of rocks")
}
