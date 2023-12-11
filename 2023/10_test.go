package twentytwentythree

import (
	"fmt"
	"io"
	"path/filepath"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input10, part1Test10, part2Test10 = InputsForDay(10)

type pipeMazeTile interface {
	symbol() rune
}

type emptyLand struct{}

func (l *emptyLand) symbol() rune {
	return '.'
}

type startingPosition struct{}

func (s *startingPosition) symbol() rune {
	return 'S'
}

type pipe struct {
	pipeSymbol rune
	openingA   Direction
	openingB   Direction
}

func (p *pipe) symbol() rune {
	return p.pipeSymbol
}

func parsePipeTile(r rune) (pipeMazeTile, error) {
	switch r {
	case '|':
		return &pipe{r, North, South}, nil
	case '-':
		return &pipe{r, West, East}, nil
	case 'L':
		return &pipe{r, North, East}, nil
	case 'J':
		return &pipe{r, North, West}, nil
	case '7':
		return &pipe{r, South, West}, nil
	case 'F':
		return &pipe{r, South, East}, nil
	case '.':
		return &emptyLand{}, nil
	case 'S':
		return &startingPosition{}, nil
	default:
		return nil, fmt.Errorf("failed to parse pipe tile from %q", r)
	}
}

type loopSegment struct {
	pipeMazeTile
	position Vec2
}

type pipeWalker struct {
	currentPosition  Vec2
	previousPosition Vec2
	startingPosition Vec2
	loopSegments     []loopSegment
	tiles            [][]pipeMazeTile
}

func newPipeWalker() *pipeWalker {
	return &pipeWalker{
		loopSegments: []loopSegment{},
	}
}

func (l *pipeWalker) adjacentPipePositions() []Vec2 {
	pos := l.currentPosition
	var adjacent []Vec2

	tile := l.tiles[pos.Y][pos.X]
	switch t := tile.(type) {
	case *startingPosition:
		adjacent = []Vec2{
			{X: pos.X - 1, Y: pos.Y},
			{X: pos.X + 1, Y: pos.Y},
			{X: pos.X, Y: pos.Y - 1},
			{X: pos.X, Y: pos.Y + 1},
		}
	case *pipe:
		adjacent = []Vec2{pos.Add(Vec2(t.openingA)), pos.Add(Vec2(t.openingB))}
	case *emptyLand:
		panic("you exited the pipe!")
	}

	return Filter(adjacent, func(v Vec2) bool {
		return v != l.previousPosition &&
			v.X >= 0 && v.X < len(l.tiles[0]) &&
			v.Y >= 0 && v.Y < len(l.tiles)
	})
}

func (l *pipeWalker) canWalk(pos Vec2) bool {
	tile := l.tiles[pos.Y][pos.X]
	switch t := tile.(type) {
	case *startingPosition:
		return true
	case *pipe:
		entry := DirectionFromPositions(l.currentPosition, pos)
		return entry == t.openingA || entry == t.openingB
	default:
		return false
	}
}

func (l *pipeWalker) step() pipeMazeTile {
	positions := l.adjacentPipePositions()
	for _, pos := range positions {
		if l.canWalk(pos) {
			l.previousPosition = l.currentPosition
			l.currentPosition = pos
			return l.tiles[pos.Y][pos.X]
		}
	}

	panic("nowhere to walk!")
}

func (l *pipeWalker) walk() int {
	for {
		tile := l.step()
		l.loopSegments = append(l.loopSegments, loopSegment{pipeMazeTile: tile, position: l.currentPosition})
		if l.currentPosition == l.startingPosition {
			return len(l.loopSegments)
		}
	}
}

func (l *pipeWalker) loopSegmentMap() map[Vec2]pipeMazeTile {
	segments := map[Vec2]pipeMazeTile{}
	for _, segment := range l.loopSegments {
		segments[segment.position] = segment.pipeMazeTile
	}

	return segments
}

func (l *pipeWalker) adjacentTilePositions(pos Vec2) []Vec2 {
	adjacent := []Vec2{
		{X: pos.X - 1, Y: pos.Y},
		{X: pos.X + 1, Y: pos.Y},
		{X: pos.X, Y: pos.Y - 1},
		{X: pos.X, Y: pos.Y + 1},
	}

	return Filter(adjacent, func(v Vec2) bool {
		return v.X >= 0 && v.X < len(l.tiles[0]) &&
			v.Y >= 0 && v.Y < len(l.tiles)
	})
}

func (l *pipeWalker) addTileAndAdjacent(insideTiles, loopTiles map[Vec2]pipeMazeTile, pos Vec2) {
	if pos.X < 0 || pos.X >= len(l.tiles[0]) ||
		pos.Y < 0 || pos.Y >= len(l.tiles) {
		return
	}

	if _, ok := loopTiles[pos]; ok {
		return
	}
	if _, ok := insideTiles[pos]; ok {
		return
	}

	insideTiles[pos] = l.tiles[pos.Y][pos.X]
	for _, pos := range l.adjacentTilePositions(pos) {
		l.addTileAndAdjacent(insideTiles, loopTiles, pos)
	}
}

func (l *pipeWalker) walkNextToLoop(turnRight bool) map[Vec2]pipeMazeTile {
	insideTiles := map[Vec2]pipeMazeTile{}
	loopTiles := l.loopSegmentMap()

	for _, segment := range l.loopSegments {
		heading := DirectionFromPositions(l.currentPosition, segment.position)
		l.previousPosition = l.currentPosition
		l.currentPosition = segment.position

		_, ok := segment.pipeMazeTile.(*pipe)
		if !ok {
			continue
		}

		turnedHeading := heading.Left()
		if turnRight {
			turnedHeading = heading.Right()
		}

		testTilePos := segment.position.Add(Vec2(turnedHeading))
		l.addTileAndAdjacent(insideTiles, loopTiles, testTilePos)

		testTilePos = l.previousPosition.Add(Vec2(turnedHeading))
		l.addTileAndAdjacent(insideTiles, loopTiles, testTilePos)
	}

	return insideTiles
}

func (l *pipeWalker) visualize(insideTiles map[Vec2]pipeMazeTile) string {
	segments := l.loopSegmentMap()
	b := strings.Builder{}

	for y, row := range l.tiles {
		for x, tile := range row {
			if _, ok := insideTiles[Vec2{x, y}]; ok {
				b.WriteString(Magenta(string(tile.symbol())))
				continue
			}
			if _, ok := segments[Vec2{x, y}]; ok {
				b.WriteString(Green(string(tile.symbol())))
				continue
			}

			b.WriteRune(tile.symbol())
		}
		b.WriteRune('\n')
	}

	return b.String()
}

func parsePipeWalker(r io.Reader) (*pipeWalker, error) {
	walker := newPipeWalker()

	tiles, err := ParseIntoMatrix(r, func(r rune, row, column int) (pipeMazeTile, error) {
		tile, err := parsePipeTile(r)
		if err != nil {
			return nil, err
		}

		if _, ok := tile.(*startingPosition); ok {
			walker.startingPosition = Vec2{X: column, Y: row}
			walker.currentPosition = walker.startingPosition
			walker.previousPosition = walker.currentPosition
		}

		return tile, nil
	})
	walker.tiles = tiles

	return walker, err
}

func Test10Part1Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part1Test10))
	AssertNoError(t, err, "open file")
	defer file.Close()

	luke, err := parsePipeWalker(file)
	AssertNoError(t, err, "parsePipeWalker")

	loopLength := luke.walk()
	AssertEquals(t, 4, loopLength/2, "farthest position")
}

func Test10Part1(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input10))
	AssertNoError(t, err, "open file")
	defer file.Close()

	luke, err := parsePipeWalker(file)
	AssertNoError(t, err, "parsePipeWalker")

	loopLength := luke.walk()
	AssertEquals(t, 6738, loopLength/2, "farthest position")
}

func Test10Part2Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part2Test10))
	AssertNoError(t, err, "open file")
	defer file.Close()

	luke, err := parsePipeWalker(file)
	AssertNoError(t, err, "parsePipeWalker")

	luke.walk()
	insideTiles := luke.walkNextToLoop(true)
	AssertEquals(t, 10, len(insideTiles), "inside tiles")

	// Uncomment for visualization
	fmt.Println(luke.visualize(insideTiles))
}

func Test10Part2(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input10))
	AssertNoError(t, err, "open file")
	defer file.Close()

	luke, err := parsePipeWalker(file)
	AssertNoError(t, err, "parsePipeWalker")

	luke.walk()
	insideTiles := luke.walkNextToLoop(false)
	AssertEquals(t, 579, len(insideTiles), "inside tiles")

	// Uncomment for visualization
	fmt.Println(luke.visualize(insideTiles))
}
