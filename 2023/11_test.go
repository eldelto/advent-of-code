package twentytwentythree

import (
	"fmt"
	"io"
	"path/filepath"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input11, part1Test11, part2Test11 = InputsForDay(11)

type genericTile struct {
	symbol rune
}

type universe struct {
	tiles    [][]genericTile
	galaxies map[Vec2]genericTile
}

func newUniverse() *universe {
	return &universe{
		galaxies: map[Vec2]genericTile{},
	}
}

func (u *universe) findEmptyRowsAndColumns() (map[int]struct{}, map[int]struct{}) {
	emptyRows := map[int]struct{}{}
	emptyColumns := map[int]struct{}{}

rowsOuter:
	for row := 0; row < len(u.tiles); row++ {
		for column := 0; column < len(u.tiles[0]); column++ {
			tile := u.tiles[row][column]
			if tile.symbol == '#' {
				continue rowsOuter
			}
		}
		emptyRows[row] = struct{}{}
	}

columnsOuter:
	for column := 0; column < len(u.tiles[0]); column++ {
		for row := 0; row < len(u.tiles); row++ {
			tile := u.tiles[row][column]
			if tile.symbol == '#' {
				continue columnsOuter
			}
		}
		emptyColumns[column] = struct{}{}
	}

	return emptyRows, emptyColumns
}

func parseUniverse(r io.Reader) (*universe, error) {
	universe := newUniverse()

	tiles, err := ParseIntoMatrix(r, func(r rune, row int, column int) (genericTile, error) {
		tile := genericTile{r}
		if r == '#' {
			pos := Vec2{column, row}
			universe.galaxies[pos] = tile
		}

		return tile, nil
	})

	universe.tiles = tiles
	return universe, err
}

func keyFromPositions(a, b Vec2) string {
	if a.X < b.X {
		a, b = b, a
	}
	if a.Y < b.Y {
		a, b = b, a
	}
	return fmt.Sprintf("%d:%d-%d:%d", a.X, a.Y, b.X, b.Y)
}

func calculateExpandedDistance(a, b, expansion int, emptySpaces map[int]struct{}) int {
	x := 0
	if a > b {
		a, b = b, a
	}
	for i := a; i < b; i++ {
		_, ok := emptySpaces[i]
		if ok {
			x += expansion
		} else {
			x++
		}
	}

	return x
}

func (u *universe) calculateExpandedDistance(a, b Vec2, expansion int, emptyRows, emptyColumns map[int]struct{}) int {
	x := calculateExpandedDistance(a.X, b.X, expansion, emptyColumns)
	y := calculateExpandedDistance(a.Y, b.Y, expansion, emptyRows)

	return x + y
}

func (u *universe) calculateAllExpandedDistances(expansion int, emptyRows, emptyColumns map[int]struct{}) []int {
	distanceSet := map[string]int{}
	for a := range u.galaxies {
		for b := range u.galaxies {
			key := keyFromPositions(a, b)
			if _, ok := distanceSet[key]; !ok {
				d := u.calculateExpandedDistance(a, b, expansion, emptyRows, emptyColumns)
				distanceSet[key] = d
			}
		}
	}

	distances := []int{}
	for _, d := range distanceSet {
		distances = append(distances, d)
	}

	return distances
}

func Test11Part1Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part1Test11))
	AssertNoError(t, err, "open file")
	defer file.Close()

	universe, err := parseUniverse(file)
	AssertNoError(t, err, "parseUnivers")

	emptyRows, emptyColumns := universe.findEmptyRowsAndColumns()
	distances := universe.calculateAllExpandedDistances(2, emptyRows, emptyColumns)
	sum := Sum(distances)
	AssertEquals(t, 374, sum, "sum of distances")
}

func Test11Part1(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input11))
	AssertNoError(t, err, "open file")
	defer file.Close()

	universe, err := parseUniverse(file)
	AssertNoError(t, err, "parseUnivers")

	emptyRows, emptyColumns := universe.findEmptyRowsAndColumns()
	distances := universe.calculateAllExpandedDistances(2, emptyRows, emptyColumns)
	sum := Sum(distances)
	AssertEquals(t, 9686930, sum, "sum of distances")
}

func Test11Part2(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input11))
	AssertNoError(t, err, "open file")
	defer file.Close()

	universe, err := parseUniverse(file)
	AssertNoError(t, err, "parseUnivers")

	emptyRows, emptyColumns := universe.findEmptyRowsAndColumns()
	distances := universe.calculateAllExpandedDistances(1000000, emptyRows, emptyColumns)
	sum := Sum(distances)
	AssertEquals(t, 630728425490, sum, "sum of distances")
}
