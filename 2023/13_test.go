package twentytwentythree

import (
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input13, part1Test13, part2Test13 = InputsForDay(13)

type mirrorTiles [][]genericTile

func parseMirrorTiles(data string) []mirrorTiles {
	rawTiles := strings.Split(data, "\n\n")
	return Map(rawTiles, func(s string) mirrorTiles {
		tiles := ParseGenericMatrix(strings.NewReader(s))
		return mirrorTiles(tiles)
	})
}

func findCleanHorizontalMirrors(matrix mirrorTiles) int {
	return findHorizontalMirrors(matrix, 0)
}

func findDirtyHorizontalMirrors(matrix mirrorTiles) int {
	return findHorizontalMirrors(matrix, 1)
}

func findHorizontalMirrors(matrix mirrorTiles, maxErrors int) int {
	result := 0

rowLoop:
	for ri, row := range matrix[:len(matrix)-1] {
		errorCount := 0
		for ci := range row {
			offset := 0
			for {
				upperRow := ri - offset
				lowerRow := ri + 1 + offset
				if upperRow < 0 || lowerRow >= len(matrix) {
					break
				}

				tile := matrix[upperRow][ci]
				below := matrix[lowerRow][ci]
				if tile != below {
					errorCount++
				}
				if errorCount > maxErrors {
					continue rowLoop
				}
				offset++
			}
		}

		if errorCount == maxErrors {
			result += ri + 1
		}
	}

	return result
}

func findCleanVerticalMirrors(matrix mirrorTiles) int {
	return findVerticalMirrors(matrix, 0)
}
func findDirtyVerticalMirrors(matrix mirrorTiles) int {
	return findVerticalMirrors(matrix, 1)
}

func findVerticalMirrors(matrix mirrorTiles, maxErrors int) int {
	result := 0

columnLoop:
	for ci := range matrix[0][:len(matrix[0])-1] {
		errorCount := 0
		for ri := 0; ri < len(matrix); ri++ {
			offset := 0
			for {
				leftColumn := ci - offset
				rightColumn := ci + 1 + offset
				if leftColumn < 0 || rightColumn >= len(matrix[0]) {
					break
				}

				left := matrix[ri][leftColumn]
				right := matrix[ri][rightColumn]
				if left != right {
					errorCount++
				}
				if errorCount > maxErrors {
					continue columnLoop
				}
				offset++
			}
		}
		if errorCount == maxErrors {
			result += ci + 1
		}
	}

	return result
}

func Test13Part1Test(t *testing.T) {
	content, err := InputToString(part1Test13)
	AssertNoError(t, err, "InputToContent")

	tiles := parseMirrorTiles(content)
	horizontalMirrors := Map(tiles, findCleanHorizontalMirrors)
	verticalMirrors := Map(tiles, findCleanVerticalMirrors)

	sum := Sum(horizontalMirrors)*100 + Sum(verticalMirrors)
	AssertEquals(t, 405, sum, "sum")
}

func Test13Part1(t *testing.T) {
	content, err := InputToString(input13)
	AssertNoError(t, err, "InputToContent")

	tiles := parseMirrorTiles(content)
	horizontalMirrors := Map(tiles, findCleanHorizontalMirrors)
	verticalMirrors := Map(tiles, findCleanVerticalMirrors)

	sum := Sum(horizontalMirrors)*100 + Sum(verticalMirrors)
	AssertEquals(t, 35521, sum, "sum")
}

func Test13Part2Test(t *testing.T) {
	content, err := InputToString(part1Test13)
	AssertNoError(t, err, "InputToContent")

	tiles := parseMirrorTiles(content)
	horizontalMirrors := Map(tiles, findDirtyHorizontalMirrors)
	verticalMirrors := Map(tiles, findDirtyVerticalMirrors)

	sum := Sum(horizontalMirrors)*100 + Sum(verticalMirrors)
	AssertEquals(t, 400, sum, "sum")
}

func Test13Part2(t *testing.T) {
	content, err := InputToString(input13)
	AssertNoError(t, err, "InputToContent")

	tiles := parseMirrorTiles(content)
	horizontalMirrors := Map(tiles, findDirtyHorizontalMirrors)
	verticalMirrors := Map(tiles, findDirtyVerticalMirrors)

	sum := Sum(horizontalMirrors)*100 + Sum(verticalMirrors)
	AssertEquals(t, 34795, sum, "sum")
}
