package twentytwentythree

import (
	"fmt"
	"path/filepath"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input14, part1Test14, _ = InputsForDay(14)

func slideRock(matrix Matrix[GenericTile], pos Vec2, direction Direction) bool {
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

func slideRocks(matrix Matrix[GenericTile], direction Direction) {
	for ri, row := range matrix {
		for ci := range row {
			if matrix[ri][ci].symbol == 'O' {
				slideRock(matrix, Vec2{ci, ri}, direction)
			}
		}
	}
}

func cycleRocks(matrix Matrix[GenericTile]) int {
	weights := []int{}
	directions := []Direction{North, West, South, East}
	for i := 0; i < 1000; i++ {
		for _, direction := range directions {
			slideRocks(matrix, direction)
		}

		weights = append(weights, weighRocks(matrix))
		offset, pattern := FindRepeatingPattern(weights)
		if len(weights) > 100 && len(pattern) > 0 {
			return pattern[(1000000000-(offset-1))%4]
		}
	}

	return -1
}

func weighRocks(matrix Matrix[GenericTile]) int {
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

func Test14Part1Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part1Test14))
	AssertNoError(t, err, "open file")
	defer file.Close()

	matrix := ParseMatrix(file)
	slideRocks(matrix, North)

	weight := weighRocks(matrix)
	AssertEquals(t, 136, weight, "weight of rocks")
}

func Test14Part1(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input14))
	AssertNoError(t, err, "open file")
	defer file.Close()

	matrix := ParseMatrix(file)
	slideRocks(matrix, North)

	weight := weighRocks(matrix)
	AssertEquals(t, 112773, weight, "weight of rocks")
}

func Test14Part2Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part1Test14))
	AssertNoError(t, err, "open file")
	defer file.Close()

	matrix := ParseMatrix(file)
	weight := cycleRocks(matrix)
	AssertEquals(t, 64, weight, "weight of rocks")
}

func Test14Part2(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input14))
	AssertNoError(t, err, "open file")
	defer file.Close()

	matrix := ParseMatrix(file)
	weight := cycleRocks(matrix)
	AssertEquals(t, 98894, weight, "weight of rocks")
}
