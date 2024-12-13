package twentytwentyfour

import (
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input12, part1Test12, part2Test12 = InputsForDay(12)

type plot struct {
	tiles []GenericTile
}

func (p *plot) area() int {
	return len(p.tiles)
}

func (p *plot) borders() int {
	sharedBorders := 0
	for _, t1 := range p.tiles {
		for _, t2 := range p.tiles {
			if t1 == t2 {
				continue
			}

			if t1.pos.X == t2.pos.X && Abs(t1.pos.Y-t2.pos.Y) == 1 {
				sharedBorders++
			}
			if t1.pos.Y == t2.pos.Y && Abs(t1.pos.X-t2.pos.X) == 1 {
				sharedBorders++
			}
		}
	}

	return len(p.tiles)*4 - sharedBorders
}

func (p *plot) cost() int {
	return p.area() * p.borders()
}

func isOtherPlot(original GenericTile) func(other GenericTile) bool {
	return func(other GenericTile) bool {
		return original.symbol != other.symbol
	}
}

func findPlots(field Matrix[GenericTile]) []plot {
	plots := []plot{}
	visited := map[Vec2]struct{}{}
	field.ForEach(func(tile GenericTile) {
		if tile.symbol != '.' {
			currentPlot := plot{tiles: []GenericTile{}}

			FloodFill(field, tile.pos, isOtherPlot(tile),
				func(old GenericTile) GenericTile {
					currentPlot.tiles = append(currentPlot.tiles, old)
					return old
				},
				visited)

			plots = append(plots, currentPlot)
		}
	})

	return plots
}

func Test12Part1Test(t *testing.T) {
	field, err := InputToMatrix(part1Test12)
	AssertNoError(t, err, "InputToMatrix")

	plots := findPlots(field)
	costs := Map(plots, func(p plot) int {
		return p.cost()
	})

	totalCost := Sum(costs)
	AssertEquals(t, 1930, totalCost, "total cost")
}

func Test12Part1(t *testing.T) {
	field, err := InputToMatrix(input12)
	AssertNoError(t, err, "InputToMatrix")

	plots := findPlots(field)
	costs := Map(plots, func(p plot) int {
		return p.cost()
	})

	totalCost := Sum(costs)
	AssertEquals(t, 1477924, totalCost, "total cost")
}

func Test12Part2Test(t *testing.T) {
}

func Test12Part2(t *testing.T) {
}
