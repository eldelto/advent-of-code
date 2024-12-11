package twentytwentyfour

import (
	"strconv"
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input10, part1Test10, part2Test10 = InputsForDay(10)

func findTrailheads(field Matrix[GenericTile]) []GenericTile {
	return field.Filter(func(t GenericTile) bool {
		return t.symbol == '0'
	})
}

func possibleTrailPaths(tile GenericTile, field Matrix[GenericTile]) []GenericTile {
	height, err := strconv.Atoi(string(tile.symbol))
	if err != nil {
		panic(err)
	}

	return Filter(TileNeighbours(field, tile.pos), func(t GenericTile) bool {
		h, err := strconv.Atoi(string(t.symbol))
		if err != nil {
			panic(err)
		}
		return h == height+1
	})
}

func walkTrail(field Matrix[GenericTile], trailhead GenericTile) int {
	reachedMountainTops := map[GenericTile]struct{}{}
	positions := []GenericTile{trailhead}
	nextPositions := []GenericTile{}
	for {
		for _, path := range positions {
			nextSteps := possibleTrailPaths(path, field)
			nextPositions = append(nextPositions, nextSteps...)
		}

		if len(nextPositions) < 1 {
			return len(reachedMountainTops)
		}

		mountainTops := Filter(nextPositions, func(t GenericTile) bool {
			return t.symbol == '9'
		})
		for _, top := range mountainTops {
			reachedMountainTops[top] = struct{}{}
		}

		positions = nextPositions
		nextPositions = []GenericTile{}
	}
}

func walkTrails(field Matrix[GenericTile], trailheads []GenericTile) int {
	return Sum(Map(trailheads, func(t GenericTile) int {
		return walkTrail(field, t)
	}))
}

func findTrails(field Matrix[GenericTile], trailheads []GenericTile) int {
	trails := 0
	positions := trailheads
	nextPositions := []GenericTile{}
	for len(positions) > 1 {
		for _, path := range positions {
			nextSteps := possibleTrailPaths(path, field)
			nextPositions = append(nextPositions, nextSteps...)
		}

		mountainTops := Filter(nextPositions, func(t GenericTile) bool {
			return t.symbol == '9'
		})
		trails += len(mountainTops)

		positions = nextPositions
		nextPositions = []GenericTile{}
	}

	return trails
}

func Test10Part1Test(t *testing.T) {
	field, err := InputToMatrix(part1Test10)
	AssertNoError(t, err, "InputToMatrix")

	trailheads := findTrailheads(field)
	paths := walkTrails(field, trailheads)

	AssertEquals(t, 36, paths, "destinations")
}

func Test10Part1(t *testing.T) {
	field, err := InputToMatrix(input10)
	AssertNoError(t, err, "InputToMatrix")

	trailheads := findTrailheads(field)
	paths := walkTrails(field, trailheads)

	AssertEquals(t, 430, paths, "destinations")
}

func Test10Part2Test(t *testing.T) {
	field, err := InputToMatrix(part1Test10)
	AssertNoError(t, err, "InputToMatrix")

	trailheads := findTrailheads(field)
	paths := findTrails(field, trailheads)

	AssertEquals(t, 81, paths, "trails")
}

func Test10Part2(t *testing.T) {
	field, err := InputToMatrix(input10)
	AssertNoError(t, err, "InputToMatrix")

	trailheads := findTrailheads(field)
	paths := findTrails(field, trailheads)

	AssertEquals(t, 928, paths, "trails")
}
