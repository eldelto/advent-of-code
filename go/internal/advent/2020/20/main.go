package main

import (
	"errors"
	"fmt"
	"math"
	"os"
	"strconv"
	"time"

	"strings"

	"github.com/gdamore/tcell/v2"
)

func main() {
	tiles := parseTiles(exampleInput)

	screen := initTcell()
	defer screen.Fini()

	waitForEsc(screen, func(s tcell.Screen, quit <-chan struct{}) {
		positionTiles(tiles, s, quit)
	})
}

func parseTiles(input string) []*Tile {
	rawTiles := strings.Split(input, "\n\n")
	tiles := []*Tile{}
	for _, rawTile := range rawTiles {
		tile := parseTile(rawTile)
		tiles = append(tiles, tile)
	}

	return tiles
}

func parseTile(input string) *Tile {
	rows := strings.Split(input, "\n")
	rawNumber := strings.Split(rows[0], " ")[1]
	rawNumber = strings.Trim(rawNumber, ":")
	id, err := strconv.Atoi(rawNumber)
	if err != nil {
		err = fmt.Errorf("not a valid number: %s", rawNumber)
		panic(err)
	}

	data := make([][]rune, len(rows)-1)
	for i, row := range rows {
		if i == 0 {
			continue
		}

		data[i-1] = []rune(row)
	}

	return NewTile(id, data)
}

func positionTiles(tiles []*Tile, s tcell.Screen, quit <-chan struct{}) {
	i := 0
	for { //_, startTile := range tiles {
		startTile := tiles[i]
		startTile.Position = Vec2{0, 0}

		for {
			nextPos, err := findNextPosition(tiles)
			if err != nil {
				fmt.Println("Done!")
				return
			}

			if !attachTile(tiles, nextPos, s, quit) {
				resetTiles(tiles)
				s.Clear()
				break
			}
		}

		i++
		if i >= len(tiles) {
			i = 0
		}
	}

	fmt.Println("No layout found")
}

func resetTiles(tiles []*Tile) {
	for _, tile := range tiles {
		tile.Position = Vec2{-1, -1}
	}
}

func attachTile(tiles []*Tile, pos Vec2, s tcell.Screen, quit <-chan struct{}) bool {
	neighbourEdges := findNeighbourEdges(tiles, pos)
	for _, tile := range tiles {
		if tile.Position != (Vec2{-1, -1}) {
			continue
		}

		select {
		case <-quit:
			os.Exit(0)
		default:
		}

		tile.Position = pos
		drawTiles(s, tiles)
		time.Sleep(time.Millisecond * 10)

		if canAttach(tile, neighbourEdges) {
			return true
		}
		for i := 0; i < 3; i++ {
			if tile.Rotate(); canAttach(tile, neighbourEdges) {
				return true
			}
		}

		if tile.Flip(); canAttach(tile, neighbourEdges) {
			return true
		}
		for i := 0; i < 4; i++ {
			if tile.Rotate(); canAttach(tile, neighbourEdges) {
				return true
			}
		}

		tile.Position = Vec2{-1, -1}
	}

	return false
}

func canAttach(tile *Tile, neighbourEdges [4]Edge) bool {
	for i, neighbourEdge := range neighbourEdges {
		if len(neighbourEdge) == 0 {
			continue
		}

		if !neighbourEdge.Equals(tile.Edges[i]) {
			return false
		}
	}

	return true
}

func findNeighbourEdges(tiles []*Tile, pos Vec2) [4]Edge {
	edges := [4]Edge{}
	positions := [4]Vec2{
		{pos.X, pos.Y - 1},
		{pos.X + 1, pos.Y},
		{pos.X, pos.Y + 1},
		{pos.X - 1, pos.Y},
	}

	for i, p := range positions {
		neighbour := getTileAtPosition(tiles, p)
		if neighbour != nil {
			edges[i] = neighbour.Edges[(i+2)%4]
		}
	}

	return edges
}

func getTileAtPosition(tiles []*Tile, pos Vec2) *Tile {
	for _, tile := range tiles {
		if tile.Position == pos {
			return tile
		}
	}

	return nil
}

func findNextPosition(tiles []*Tile) (Vec2, error) {
	maxLen := int(math.Sqrt(float64(len(tiles)))) - 1
	nextX := 0
	nextY := 0
	for _, tile := range tiles {
		if tile.Position.X > nextX {
			nextX = tile.Position.X
			nextY = 0
		}

		if tile.Position.Y > nextY && tile.Position.X == nextX {
			nextY = tile.Position.Y
		}
	}
	nextY++

	if nextY > maxLen {
		nextX++
		nextY = 0

		if nextX > maxLen {
			return Vec2{}, errors.New("all tiles placed")
		}
	}

	return Vec2{nextX, nextY}, nil
}

type Vec2 struct {
	X int
	Y int
}

type Edge []rune

func (e Edge) Equals(other Edge) bool {
	for i := range e {
		if e[i] != other[i] {
			return false
		}
	}

	return true
}

type Tile struct {
	ID       int
	Data     [][]rune
	Position Vec2
	Edges    [4]Edge
}

func NewTile(id int, data [][]rune) *Tile {
	return &Tile{
		ID:       id,
		Data:     data,
		Position: Vec2{-1, -1},
		Edges:    edgesFromData(data),
	}
}

func (t *Tile) Flip() {
	len := len(t.Data)
	newData := make([][]rune, len)

	for i, row := range t.Data {
		newData[len-(i+1)] = row
	}

	t.Data = newData
	t.Edges = edgesFromData(newData)
}

func (t *Tile) Rotate() {
	len := len(t.Data)
	newData := make([][]rune, len)

	for _, row := range t.Data {
		for j, r := range row {
			newData[j] = append(newData[j], r)
		}
	}

	t.Data = newData
	t.Edges = edgesFromData(newData)
}

func edgesFromData(data [][]rune) [4]Edge {
	len := len(data)
	top := data[0]
	bottom := data[len-1]

	right := make([]rune, len)
	left := make([]rune, len)
	for i, row := range data {
		right[i] = row[len-1]
		left[i] = row[0]
	}

	return [4]Edge{top, right, bottom, left}
}

const exampleInput = `Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...`

const input = `Tile 2539:
#.##......
##......#.
#.#...#..#
###..#...#
....#.#.##
#........#
..#..###.#
#..#..#...
.....#####
##.##..##.

Tile 2909:
....##..#.
#........#
....##....
..###..#..
#..#......
#...#..###
.........#
#...#...#.
....#.....
.##..##.##

Tile 1373:
#..###.##.
#......#.#
..#.....##
..##.#...#
........##
....#...#.
..........
#..##.#.#.
.....#...#
..##.#....

Tile 2953:
######....
........#.
..#.......
#.#.......
##........
##........
..#.....##
#.#......#
##..#....#
#.####.###

Tile 2437:
#..##..##.
.....#...#
#.#..##...
.#......##
#.....#..#
#......#..
...#.#...#
#...#....#
#.........
.##.....#.

Tile 1823:
##.....###
##.#..#..#
....##....
#.........
..#.....#.
#..#......
..#.......
..#.#....#
.....#..#.
####....#.

Tile 3889:
..##...#.#
#........#
#.......##
.......###
..........
....#..#..
..#...#..#
.##...#..#
##.....#.#
.######.#.

Tile 2549:
..#.#.###.
..........
.........#
#........#
#.........
#..#....##
..........
#.....#...
#.#...####
#...#####.

Tile 3797:
#.#.#..#..
#........#
.#.##.....
#........#
#........#
#.........
#.#...##..
#..#.....#
#.#.##....
.#######..

Tile 1249:
.##..##...
.....#....
#....#...#
..#......#
###..#...#
...#....#.
#.....#..#
.#.#......
#.#.##.#.#
#.#.##..#.

Tile 3821:
.####.##.#
.........#
#...##...#
.##.#...##
##..#.##..
.....#.#.#
..#...#.##
#.......##
...#.....#
....#.###.

Tile 3329:
##.##.###.
......#...
...#....##
....#....#
#.........
..#...#...
....#.....
..#......#
..........
...#.##..#

Tile 1279:
##...#....
#.#.#...##
.#..#...#.
.#....#.#.
.##......#
.........#
##......##
..........
.#........
..#..##...

Tile 3907:
.#....##.#
#.#...###.
##.....###
##......#.
..####....
.#...#....
#....#..#.
#.#.......
.#......#.
##....###.

Tile 3833:
....#.###.
.......#.#
...#...#..
#.#.......
..#.....##
#.........
.....#.#..
#.....#...
..#..#....
.#####....

Tile 2011:
.#...#...#
#.#.......
#..#....##
.....#....
.......#..
.#.......#
......#.#.
#.##...###
##..#..###
###..#.###

Tile 3881:
###..#...#
#..#.....#
##..#...##
.....#.##.
.........#
.#.......#
....#..#.#
..#.....##
##......##
...#..#.#.

Tile 2341:
.....##.#.
#....#..#.
....#.#...
#.....#...
.....#....
#.......#.
.........#
##....#..#
.........#
##.#..###.

Tile 2917:
###.##....
##.......#
#........#
#......#..
#.........
........##
.........#
....#....#
#.##...#..
..#######.

Tile 2333:
#####.....
.....#...#
....##...#
..........
...#...#.#
....##....
....#.....
.......#..
###....#.#
..##.#..#.

Tile 2287:
.#..#.#.#.
##...#....
....#.....
.......#..
.....#...#
#....#.###
.........#
#.##..###.
#......#.#
.##.#.#.##

Tile 3391:
#..#.#..#.
......#...
#......#..
#.#.#....#
....#....#
#..#....##
....#....#
..#.....##
#........#
.###.#..##

Tile 2677:
#..#.#....
..#...#..#
#...##...#
##.......#
.........#
..........
#...##.###
#....#....
..##..#.#.
...#...##.

Tile 2273:
.###.####.
##.##...##
.....###.#
#..#..#..#
.##......#
#.#..#...#
#........#
#.........
........##
####.#....

Tile 3967:
....#..##.
..#......#
#....##...
#.....#.##
...#...#.#
#.#.#..##.
#.......#.
#.......##
...##.#..#
...#.#..##

Tile 1553:
.###.#..#.
##...#....
.....#...#
......#.##
.........#
..........
#..##.#..#
#...#.....
#.........
..#..##..#

Tile 1291:
.#.......#
..#......#
......#..#
##.#..#..#
##........
..........
.........#
......#.#.
##....#.##
##...#.#.#

Tile 2003:
..####.##.
##.##....#
#......##.
.........#
..........
...#.##..#
#.#.....#.
..#.#....#
#..#.....#
..##..##.#

Tile 2267:
..#...#...
...#....#.
#..#..#...
#...#.....
...#.....#
.#........
#.#...#..#
...#..#...
#.#......#
.####.#...

Tile 1237:
#.#....##.
.......#.#
.....#....
...#.##...
....#.....
#.#.#....#
#..#......
##........
#.........
#.#.##..#.

Tile 3187:
#.####..##
.....#....
#..#......
.#.#......
..##....##
..#......#
...#.#....
#........#
.....##.#.
.###.#.##.

Tile 2393:
#.#.###.##
#.........
#..#.#...#
#.#.......
#.#....#.#
#.#...#...
...#.....#
#.#....#.#
#...#....#
..#.###...

Tile 3413:
.##...#...
#.....#...
#.........
#..#..#..#
#....#.#..
##..#.....
#.....####
#........#
....##....
###.###.##

Tile 3083:
#####.####
#..##.....
..........
........#.
.#.......#
.........#
#...#.#..#
#....##.#.
##..#.##.#
....####.#

Tile 1523:
###..##..#
.#.#..#..#
....#...##
.....#....
#..#.....#
#....#...#
........##
..........
##.....#..
#.#..##..#

Tile 3673:
#.####.###
#........#
.#...#..#.
.........#
##.....#..
..........
#.........
......#..#
##.#.....#
.#..##..##

Tile 2111:
#.#.#.####
......#.#.
..#......#
........##
#.....##..
.........#
#..#......
........#.
#........#
..#..#.#..

Tile 1153:
.#.#..#..#
#.....##..
...#.....#
...#.....#
#..#.....#
##.#......
#.####....
#........#
#.........
..#.....#.

Tile 3457:
..#......#
#.##...#..
#.........
...#...##.
....#.....
.#........
#........#
......##.#
..........
#.#.....#.

Tile 2693:
.####.....
#........#
..#....#..
.......###
..#...#...
.....##...
#.#.#....#
#.#.......
##.##.#..#
.####.#..#

Tile 1367:
##...##.#.
.......#..
...#..#...
#....#....
##........
##....##.#
#......#.#
#...#.##.#
#...#.#..#
.##.....##

Tile 2543:
#..###.#.#
#....###.#
##......#.
...#.....#
#....#..##
......#..#
#..#.#.#..
...#....#.
....##.###
#...######

Tile 1229:
#.#....##.
#...#...##
.##.......
#...#..#.#
#.#.#..#..
#..#..#..#
##....#...
#...#...##
#.#.......
##.#.#..#.

Tile 2833:
.#...#..#.
........#.
#.#..#....
...##..#.#
#....#....
........##
.........#
.........#
#........#
.###.#.#.#

Tile 2089:
.####.###.
..###....#
#....#.#..
##........
..#......#
###...#..#
###...#.##
......#...
#.........
.###...##.

Tile 3209:
.######..#
.#......#.
#.#.#.....
##.....#.#
#......###
##...##...
......#...
#.#.......
#..#....#.
...#...###

Tile 1979:
#...###.##
#.......##
#.#.#....#
....#.....
...#.#..##
#.##...#..
##.#.....#
#...#.....
.....#....
....##..#.

Tile 3779:
##.##..#..
..#......#
#.#.....#.
.#........
#...##..##
##.....#..
...#.#....
##........
#..#...#..
.###.##.##

Tile 3089:
##.#.##..#
#..#......
..#......#
...#....#.
#.#.......
..........
#.....#...
#........#
#......#.#
.####.##.#

Tile 1627:
#..#....#.
.#.####.#.
#...#.....
...##.#.##
....#...#.
...#.##.##
.......#.#
#..#..#..#
#.....##.#
#...##.#.#

Tile 2389:
.##...#.#.
#..#..#...
#.........
.........#
.....#....
....#...#.
.#.#...#.#
....###..#
#....##.#.
####..#.##

Tile 1193:
.#.##.####
.#......##
#...##...#
..##..#.#.
..........
.....##.##
#......#..
...#.##...
....#..##.
##.#######

Tile 3323:
#..#...###
........#.
##.....#.#
#....#...#
....#.....
#...#..#..
#.........
.#.....#.#
..#......#
#..#.#....

Tile 2719:
####......
..#.....#.
.#....#...
#...#..#..
#.........
#......#..
#..#....##
.....#....
#.##...##.
#.#.#..#.#

Tile 2399:
#..##.####
.........#
#.........
...#...##.
.........#
#...#....#
..........
........##
..........
.#.#...#..

Tile 1543:
##..#.####
....#....#
....######
#..####.##
...##..#..
#..#.#...#
#.#.......
...#....#.
##..###.#.
#.##.....#

Tile 1723:
.#.##..#..
...#....#.
...#......
##....#.##
.#....##..
#.#......#
##.##....#
.....#...#
..#.......
#..#.##..#

Tile 2503:
..###...##
..........
#..#.##..#
...#.#...#
...###..##
.....#....
#.....#..#
.#.#.#.##.
#......#.#
..####..##

Tile 2887:
......##.#
##........
.#........
#.......##
#.##...##.
#...#...##
##.....##.
....#.....
........##
#.######..

Tile 3467:
#..#..#.#.
....###...
....##...#
....#.#...
..........
........#.
......#.#.
#.#...#..#
#........#
#.##.##.##

Tile 3677:
....##...#
#.##..#...
.#....##..
.#.#......
#.##..#...
....#.....
......#.#.
#..###..#.
#.#....#..
.####.##..

Tile 1493:
.##.#...##
#.##...##.
.#...#####
#..#....#.
#.#......#
.........#
##......#.
#......#.#
#....#.#.#
#.#...#..#

Tile 2687:
.##.##..##
.......#.#
#........#
##...##..#
...#....##
.....#.#..
#.##.#.#.#
#.........
#....#...#
###.....##

Tile 1861:
####..##..
...#..#...
.......#.#
#.........
#.........
#........#
...#.....#
#.#......#
#.#..##.#.
.#..#..#.#

Tile 2081:
#..#####.#
...#.....#
.#......#.
#........#
.......#.#
#....####.
#....###.#
.#....####
....#.....
.##.##...#

Tile 2113:
#..#..##..
.#.....##.
.....#.#.#
#........#
.......#.#
...##.##..
......##..
#....#.#.#
#.....#..#
#...#..#.#

Tile 2203:
.#.##..#.#
....###..#
.#.#....##
#...#.....
##.#...#.#
..#..#..#.
#..#......
#......#..
#...###...
.#...##.#.

Tile 3943:
#....###..
#.....##.#
#....#...#
#..#....##
###..#...#
.#.#.#...#
.......##.
#....##..#
.#.......#
.#..#.#.##

Tile 2579:
.#...###..
#.........
#........#
.......#.#
##.#......
....#....#
#.......#.
.#...#....
.....#...#
..#.##..##

Tile 1103:
.#.##.###.
#......#.#
....#....#
....#.#...
#......#.#
..........
..........
#...#....#
#........#
....#####.

Tile 1993:
.##..#....
....#.#..#
##...#....
.........#
#...#....#
..........
..#..#.#..
.....##...
.##...##.#
..##.##...

Tile 1801:
#..##.####
#.......##
#......#.#
#.#....#.#
#.##....#.
.#..#.###.
...#......
#.....##.#
.#.......#
##.###..##

Tile 3319:
.###.##.#.
##......#.
.#........
#.#......#
#..#...#.#
#.#......#
.#.......#
#........#
#......#..
....###..#

Tile 3229:
###...###.
#....##..#
....#...##
...#......
..#.#....#
.......#.#
......#.#.
.##..#.#.#
...#..###.
##...#.#..

Tile 1033:
#..#.##.##
....##....
#.......#.
#......#.#
#........#
##.......#
#..##..#.#
.......###
#.........
.##.#.#..#

Tile 3769:
###...#.##
........#.
#.........
#........#
..#.#.####
#...#...##
.#.#.....#
..........
#........#
#.##.#...#

Tile 1571:
.####.....
#......#.#
#.#.#.#.#.
.........#
##.......#
.......#.#
..#.....#.
#.........
#..#....#.
######...#

Tile 1657:
...#.#.##.
........##
#.........
#.#..#....
#..#.#.#.#
...#.###..
....#.....
...#.....#
#.....##.#
#####.#.##

Tile 3541:
.####.####
..........
#......##.
#......#..
#........#
#.....#..#
........#.
##.....#.#
.......#..
#.#.###.##

Tile 3019:
...##..##.
#..##...#.
#....##...
#........#
#..#.#...#
#..#.....#
.#........
#.#..#...#
...#.....#
.#......##

Tile 2383:
.#.#....##
#....#....
......#..#
#.....#..#
#.##.#..#.
#..#....##
.....#...#
....#...#.
#.....#.#.
.#.#.#.###

Tile 3929:
...#.###.#
#.#..#....
.........#
..#......#
.#......##
#.....#.##
#...#...##
#.##......
#.....##..
#...##....

Tile 1697:
##.##.....
...#.....#
#..#.....#
.........#
...#...##.
.#..#...##
...###...#
#..#..#...
.#.#..#...
..#...#...

Tile 1999:
..##....#.
.........#
...#.#...#
#.........
..#.#.#...
#......#.#
.#.##...#.
#....#...#
#....#.#.#
#.#..##...

Tile 1289:
#..###....
...#......
....#.....
#.....##.#
#.##....#.
...#.#..##
#.#...#...
..........
..#......#
####..###.

Tile 1163:
.###.#..#.
#..#....#.
#...#...##
...#.....#
#.....#...
.........#
..........
###.......
#........#
###...####

Tile 3121:
##....#...
#..#.#.#.#
.#....#...
..#......#
#....#.#..
.##......#
......#.##
....#.#...
...#......
#...#...#.

Tile 2939:
..##.#.###
#....##..#
....#..###
###....#..
#..###..#.
..##.....#
#..#....##
..#.#.....
#......#..
.#.#.#..##

Tile 1129:
....#..#.#
#........#
...#.#....
...#...#.#
#........#
..........
..#...####
...#.#....
#....#..##
##.#.##..#

Tile 1877:
..##..#...
#.#...#...
..#...##.#
#......#..
......#.##
.##....#.#
....#..#..
......#...
#..#..#..#
#..#..##..

Tile 3061:
..#.#.....
.#........
..........
#...#.#...
..#......#
........##
#......#..
#..#.....#
#.#....#..
.####.#.##

Tile 2851:
.#####..##
..##...###
#......#..
.#..#....#
.....###..
.........#
.......###
.....#.###
#......#..
.##.##..##

Tile 1409:
##.##..###
#.........
#...###..#
.........#
........##
#.#.#...#.
.#...#.##.
....#..#..
#..#...#.#
#.###.....

Tile 3853:
...###.#.#
##.#......
.....##.#.
.#.......#
....#....#
#.#.#..#.#
....##....
........##
#.#.......
.#.#.##.##

Tile 2803:
.#...####.
#..##...#.
#.#..#....
.....#...#
.........#
#....##...
#........#
##....#...
#..#..#..#
..#.#...##

Tile 1709:
....#.....
.........#
.........#
....#....#
..##.#....
....#.....
#.#.......
#.......##
#...#....#
.#.....#.#

Tile 2351:
..#.#.#...
##.......#
....#.#...
......#..#
..........
.......#.#
##........
#.......#.
.....#..#.
.#...##..#

Tile 2347:
###..#..##
###......#
#..##.#.##
#...#...#.
#.........
.....#.###
...#....##
#......#..
#.#.##...#
.###..#...

Tile 1307:
##..#####.
.#....#..#
..........
#...#..#..
.....#....
#.##.#...#
###..##.#.
.##.#..#.#
#.#.#.....
..######.#

Tile 1901:
######..#.
##.#.....#
#....#.#.#
........#.
....#....#
#.......##
#........#
.......#.#
.#..##...#
.#.#.#####

Tile 2029:
#...##.##.
#........#
##....##.#
#.#..#....
.##..#....
.#....#...
.##......#
..........
##..#.....
#.......##

Tile 3571:
##...#.#.#
##....#..#
....#.#...
..#......#
#......#.#
..#....###
..........
#.#......#
.##.....##
.##.#...##

Tile 1399:
...###...#
......#..#
##...#.#.#
#........#
#.#.#.#..#
....####..
#.....#..#
.....#.#.#
#......#.#
#...#..#.#

Tile 1381:
#..#....#.
#....#...#
......##..
#....#...#
..#.#...#.
##..#....#
#....#...#
.....#....
#.......##
#####..#.#

Tile 1549:
##...####.
........#.
........#.
..#...##.#
.........#
#.#.#.#..#
.#.....#.#
.....##.#.
##.......#
..#.##....

Tile 3623:
..#....###
...#...#..
.......#..
.##......#
..#..##...
.........#
#...#.....
..#......#
..#......#
..##..###.

Tile 2143:
#.#..#.#..
#...#...#.
#......#..
.#..#...##
#.....#.##
......##.#
..#.#...#.
..........
#...#....#
#....#..##

Tile 3631:
#######...
#.......##
..........
.....##..#
........##
#..#.#....
.......#.#
........#.
#.......#.
..####..##

Tile 3359:
#..#...#..
...#......
...#.#.#.#
.......#..
...#.#....
##.....#.#
#.........
#..#......
#........#
#.....#.##

Tile 2239:
......#.#.
###.#.....
.#....#...
..##......
##.####..#
..........
..#...#..#
#..##....#
##...#....
##.#.#...#

Tile 1741:
....###.##
##...#...#
....##...#
#.......##
#.#.......
...###...#
#.......#.
.##....#..
#.........
..###.####

Tile 1597:
..#..###..
#...##.###
#.#..#....
#.#...#..#
#....#...#
#.......##
......#..#
##...##...
#.####....
..#.#....#

Tile 3761:
#.###.####
..#.#.#..#
....#.##.#
.###....#.
#......#.#
#......#..
#.#......#
##.......#
#.#.......
.#..#.#.#.

Tile 3203:
#......#.#
#.....#...
#.#.#..#..
......#...
#..##.....
##...#...#
.....#...#
..#...#..#
#.........
.#.#..###.

Tile 1579:
###.#.....
.#.###..#.
.....###.#
#...#.##.#
...#..#...
...#.....#
...#.....#
#.......##
..#.#..#..
###.....#.

Tile 3617:
#......#..
#.....#...
.....#.##.
#....#.#.#
...#..#..#
......#.#.
#..##....#
##....#...
.#...#....
#.....##.#

Tile 1667:
#.....#.##
##....#..#
..........
##........
.#..#.....
#........#
...#.#.#.#
#.........
.#........
..#.#.###.

Tile 2153:
....##.##.
........#.
.#..#.....
....###..#
..##...#..
.#.....###
.#...###..
#.#..#.##.
..##....##
####.#....

Tile 1721:
.###..##..
#.#....#.#
.....##..#
#.........
##...#...#
#......#.#
.#......##
#........#
...#...###
####..#...

Tile 3539:
#.##.#.#.#
.........#
#..#.....#
.....#....
..#.......
##.#....##
..#.##...#
........##
#....#....
#.#..#.#.#

Tile 3313:
...#..####
.......#.#
#........#
.....#.#..
#..#..##..
#...#.....
#..#...#.#
...#......
##....#.#.
.#.###..#.

Tile 3851:
...#..##..
#......#.#
...#..##..
#........#
#....#...#
#.....#..#
#...##.##.
##.......#
.#.##..##.
..###..#..

Tile 3389:
.####.##..
.#...#..##
..........
.#......##
.##.......
###......#
...#.#...#
.....#...#
#.#....#.#
..####.#.#

Tile 1933:
..###.#..#
..#......#
#.#....#.#
#..#......
##...#....
.....#....
....#..#.#
.#..##...#
#......#..
......#.#.

Tile 1361:
#..##...##
..........
.........#
#.#......#
#........#
..##..#..#
..#..#..##
.....#....
....#....#
#.#.###.#.

Tile 2789:
.##.##.###
#....#.#.#
.....#.#..
..........
...#.#...#
.........#
....#.....
......##..
#......#..
#.###.####

Tile 3637:
.##.##....
###...#...
###...#.#.
..........
###......#
#.......##
##.....###
....###..#
#.......#.
...##..#.#

Tile 2411:
.##.#.###.
......#.#.
#...#..###
##.#....##
#.........
..####....
#..#.#...#
..........
#...#..#.#
#.##...##.

Tile 1487:
...#.#...#
.#..##...#
#....#....
###.###..#
..##..#..#
#..#......
##...##...
..#..#....
.....#.#..
.....##.##

Tile 1061:
.#####..#.
#.......#.
...#......
....#...#.
....#.#...
#.###...#.
....##...#
###.....#.
...##...#.
#....#.#..

Tile 1669:
##...##..#
#........#
.#.#......
.###.##...
#...#.....
..#.....##
#...#..#.#
#.##....#.
#........#
##.#.#.##.

Tile 1123:
...###...#
....#.....
#...#....#
...#.....#
#.........
#....#..#.
.........#
.#....##.#
......##.#
#.#..#.#..

Tile 2767:
..###.####
#...##....
.........#
.........#
#....##...
#.#.......
.........#
.#........
#........#
#...#...##

Tile 1931:
##..###...
##..##..##
..##..##..
.#...#....
...#......
#...##....
.#.......#
...#.....#
....#.....
.#.#.###.#

Tile 2731:
...#.#..##
..........
..#......#
##....#..#
..#..#.#.#
..........
#.....#...
.##.##.#..
#....##...
.###..#.#.

Tile 2689:
###.#..###
.......###
.#....#..#
......##.#
##...#...#
........##
#...#...##
.........#
....##....
....##.#.#

Tile 2897:
#.........
#.#...#...
##.#......
....#.##.#
#........#
#.......#.
.#....#...
.....#...#
.....#..##
..#.####.#

Tile 3931:
#.#..##.#.
#......#..
#...###.#.
.##.##....
#...###..#
#...###...
.....#.#.#
#......##.
.........#
#.#######.

Tile 1783:
#......###
.........#
##....#..#
.#..#.#.##
........#.
#..#......
..........
..........
##...#...#
...##.#.##

Tile 2293:
##...#...#
..###.....
.........#
.........#
..#..#....
.....#...#
#....##.#.
#.........
...#.....#
.###..#.##

Tile 3709:
#####.###.
.###......
#..#....#.
#........#
##..#.#..#
.#.......#
.....#...#
........#.
####..##..
.#..##.###

Tile 2357:
.#..###..#
##..#....#
.....#..##
#...#.....
..........
##...#...#
.#....#..#
##.#....##
....#.#...
#.##.#...#

Tile 3331:
#.#..#.###
#...##.###
....#.#...
..#..#...#
.#.##..#.#
#.....#.#.
..#....#..
#.#.##.#..
#...###...
##..####.#

Tile 1297:
..####.###
#..#....#.
#....#...#
#.........
###.#....#
....#..#..
.##.......
.........#
#..#......
#...#.....`