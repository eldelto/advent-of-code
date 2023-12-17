package twentytwentythree

import (
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input16, part1Test16, _ = InputsForDay(16)

type lightRay struct {
	pos       Vec2
	direction Direction
}

func (lr *lightRay) step() {
	lr.pos = lr.pos.Add(Vec2(lr.direction))
}

func forwardLightRay(m Matrix[GenericTile], lr *lightRay) []lightRay {
	if !m.WithinBounds(lr.pos) {
		return nil
	}
	target := m.Get(lr.pos)

	splitRays := []lightRay{}
	switch target.symbol {
	case '\\':
		switch lr.direction {
		case North:
			lr.direction = West
		case East:
			lr.direction = South
		case South:
			lr.direction = East
		case West:
			lr.direction = North
		}
	case '/':
		switch lr.direction {
		case North:
			lr.direction = East
		case East:
			lr.direction = North
		case South:
			lr.direction = West
		case West:
			lr.direction = South
		}
	case '-':
		if lr.direction == North || lr.direction == South {
			lr.direction = West
			splitRay := *lr
			splitRay.direction = East
			splitRays = append(splitRays, splitRay)
		}
	case '|':
		if lr.direction == West || lr.direction == East {
			lr.direction = North
			splitRay := *lr
			splitRay.direction = South
			splitRays = append(splitRays, splitRay)
		}
	}

	lr.step()
	return splitRays
}

func energiseTiles(m Matrix[GenericTile], initialLightRay lightRay) int {
	lightRayCache := map[lightRay]struct{}{}
	energisedTiles := map[Vec2]struct{}{}

	splitRays := []lightRay{initialLightRay}
	for len(splitRays) > 0 {
		lightRays := make([]lightRay, len(splitRays))
		copy(lightRays, splitRays)
		splitRays = []lightRay{}

	lightRayLoop:
		for _, lr := range lightRays {
			for m.WithinBounds(lr.pos) {
				if _, ok := lightRayCache[lr]; ok {
					continue lightRayLoop
				}
				lightRayCache[lr] = struct{}{}
				energisedTiles[lr.pos] = struct{}{}
				splitRays = append(splitRays, forwardLightRay(m, &lr)...)

				// fmt.Println(lr.pos)
				// fmt.Println(splitRays)
			}
		}
	}

	return len(energisedTiles)
}

func energiseMaximumTiles(m Matrix[GenericTile]) int {
	maxTiles := 0
	for ri := range m {
		if tiles := energiseTiles(m, lightRay{pos: Vec2{0, ri}, direction: East}); tiles > maxTiles {
			maxTiles = tiles
		}
		if tiles := energiseTiles(m, lightRay{pos: Vec2{len(m) - 1, ri}, direction: West}); tiles > maxTiles {
			maxTiles = tiles
		}
	}
	for ci := range m[0] {
		if tiles := energiseTiles(m, lightRay{pos: Vec2{ci, 0}, direction: South}); tiles > maxTiles {
			maxTiles = tiles
		}
		if tiles := energiseTiles(m, lightRay{pos: Vec2{ci, len(m[0]) - 1}, direction: North}); tiles > maxTiles {
			maxTiles = tiles
		}
	}

	return maxTiles
}

func Test16Part1Test(t *testing.T) {
	matrix, err := InputToMatrix(part1Test16)
	AssertNoError(t, err, "InputToMatrix")

	lightRay := lightRay{pos: Vec2{0, 0}, direction: East}
	tiles := energiseTiles(matrix, lightRay)
	AssertEquals(t, 46, tiles, "energised tiles")
}

func Test16Part1(t *testing.T) {
	matrix, err := InputToMatrix(input16)
	AssertNoError(t, err, "InputToMatrix")

	lightRay := lightRay{pos: Vec2{0, 0}, direction: East}
	tiles := energiseTiles(matrix, lightRay)
	AssertEquals(t, 8901, tiles, "energised tiles")
}

func Test16Part2Test(t *testing.T) {
	matrix, err := InputToMatrix(part1Test16)
	AssertNoError(t, err, "InputToMatrix")

	tiles := energiseMaximumTiles(matrix)
	AssertEquals(t, 51, tiles, "energised tiles")
}

func Test16Part2(t *testing.T) {
	matrix, err := InputToMatrix(input16)
	AssertNoError(t, err, "InputToMatrix")

	tiles := energiseMaximumTiles(matrix)
	AssertEquals(t, 9064, tiles, "energised tiles")
}
