package main

import (
	"fmt"
	"math"
	"os"
	"strconv"

	// "time"

	"strings"

	"github.com/gdamore/tcell/v2"
)

func main() {

	// Example
	exampleTiles := parseTiles(exampleInput)
	exampleResult := part1Shortcut(exampleTiles)
	fmt.Printf("Example: %d\n", exampleResult)

	// Scenario 1
	tiles := parseTiles(input)
	result := part1Shortcut(tiles)
	fmt.Printf("Scenario 1: %d\n", result)

	// Example 2
	exampleTiles2 := parseTiles(exampleInput)
	exampleGrid2, err := positionTiles(exampleTiles2)
	if err != nil {
		panic(err)
	}
	exampleImage2 := exampleGrid2.GenerateImage()
	exampleImage2.ReplaceAllDirectionMatches(nessieWindow, 'X')
	fmt.Println(exampleImage2.String())
	exampleResult2 := exampleImage2.CountOccurances('#')
	fmt.Printf("Example 2: %d\n", exampleResult2)

	// Scenario 2
	tiles2 := parseTiles(input)
	grid2, err := positionTiles(tiles2)
	if err != nil {
		panic(err)
	}
	image2 := grid2.GenerateImage()
	image2.ReplaceAllDirectionMatches(nessieWindow, 'X')
	fmt.Println(image2.String())
	result2 := image2.CountOccurances('#')
	fmt.Printf("Scenario 2: %d\n", result2)

	// Uncomment for animation
	// screen := initTcell()
	// defer screen.Fini()

	// waitForEsc(screen, func(s tcell.Screen, quit <-chan struct{}) {
	// 	positionAndDrawTiles(tiles, s, quit)
	// })
}

const nessiePattern = `                  # 
#    ##    ##    ###
 #  #  #  #  #  #   `

var nessieWindow = patternToWindow(nessiePattern)

func patternToWindow(pattern string) SearchWindow {
	rows := strings.Split(pattern, "\n")
	data := [][]rune{}
	for _, row := range rows {
		data = append(data, []rune(row))
	}

	return SearchWindow(data)
}

func buildTileMatchMap(tiles []*Tile) map[int][]*Tile {
	tileMap := map[int][]*Tile{}
	for _, tile := range tiles {
		tileMap[tile.ID] = []*Tile{}
		for _, other := range tiles {
			if tile == other {
				continue
			}

			for _, myEdge := range tile.PossibleEdges {
				for _, otherEdge := range other.PossibleEdges {
					if myEdge.Equals(otherEdge) {
						tileMap[tile.ID] = append(tileMap[tile.ID], other)
					}
				}
			}
		}
	}

	return tileMap
}

func findCornerPieces(tiles []*Tile) []*Tile {
	tileMap := buildTileMatchMap(tiles)

	corners := []*Tile{}
	for k, v := range tileMap {
		if len(v) != 4 {
			continue
		}

		for _, tile := range tiles {
			if k == tile.ID {
				corners = append(corners, tile)
				break
			}
		}
	}

	return corners
}

func part1Shortcut(tiles []*Tile) int {
	tileMap := buildTileMatchMap(tiles)

	// Uncomment for debug output
	// for k, v := range tileMap {
	// 	fmt.Printf("%d matches for %d: ", len(v), k)
	// 	for _, tile := range v {
	// 		fmt.Printf("%d, ", tile.ID)
	// 	}
	// 	fmt.Println()
	// }

	product := 1
	for k, v := range tileMap {
		if len(v) != 4 {
			continue
		}

		product *= k
	}

	return product
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

func positionAndDrawTiles(tiles []*Tile, s tcell.Screen, quit <-chan struct{}) (*Grid, error) {
	corners := findCornerPieces(tiles)
	corner := corners[0]
	for i := 0; i < 4; i++ {
		corner.Position = Vec2{0, 0}

		grid := NewGrid(len(tiles))
		grid.Tiles[0][0] = corner

		for {
			nextPositions := grid.GetNextPositions()
			if len(nextPositions) == 0 {
				return grid, nil
			}

			if !attachTileAndDraw(grid, tiles, nextPositions, s, quit) {
				resetTiles(tiles)
				s.Clear()
				break
			}
		}

		corner.Rotate()
	}

	return nil, fmt.Errorf("no layout found")
}

func positionTiles(tiles []*Tile) (*Grid, error) {
	corners := findCornerPieces(tiles)
	corner := corners[0]
	for i := 0; i < 4; i++ {
		corner.Position = Vec2{0, 0}

		grid := NewGrid(len(tiles))
		grid.Tiles[0][0] = corner

		for {
			nextPositions := grid.GetNextPositions()
			if len(nextPositions) == 0 {
				return grid, nil
			}

			if !attachTile(grid, tiles, nextPositions) {
				resetTiles(tiles)
				break
			}
		}

		corner.Rotate()
	}

	return nil, fmt.Errorf("no layout found")
}

func resetTiles(tiles []*Tile) {
	for _, tile := range tiles {
		tile.Position = Vec2{-1, -1}
	}
}

func attachTileAndDraw(grid *Grid, tiles []*Tile, positions []Vec2, s tcell.Screen, quit <-chan struct{}) bool {
	for _, tile := range tiles {
		if tile.Position != (Vec2{-1, -1}) {
			continue
		}

		select {
		case <-quit:
			os.Exit(0)
		default:
		}

		for _, pos := range positions {
			neighbourEdges := grid.FindNeighbourEdges(pos)
			tile.Position = pos
			drawTiles(s, tiles)
			// time.Sleep(time.Millisecond * 1)

			// if !couldAttach(tile, neighbourEdges) {
			// 	continue
			// }

			if canAttach(tile, neighbourEdges) {
				drawTiles(s, tiles)
				grid.Tiles[pos.Y][pos.X] = tile
				return true
			}
			for i := 0; i < 4; i++ {
				if tile.Rotate(); canAttach(tile, neighbourEdges) {
					drawTiles(s, tiles)
					grid.Tiles[pos.Y][pos.X] = tile
					return true
				}
			}

			if tile.Flip(); canAttach(tile, neighbourEdges) {
				drawTiles(s, tiles)
				grid.Tiles[pos.Y][pos.X] = tile
				return true
			}
			for i := 0; i < 4; i++ {
				if tile.Rotate(); canAttach(tile, neighbourEdges) {
					drawTiles(s, tiles)
					grid.Tiles[pos.Y][pos.X] = tile
					return true
				}
			}

			tile.Position = Vec2{-1, -1}
		}
	}

	return false
}

func attachTile(grid *Grid, tiles []*Tile, positions []Vec2) bool {
	for _, tile := range tiles {
		if tile.Position != (Vec2{-1, -1}) {
			continue
		}

		for _, pos := range positions {
			neighbourEdges := grid.FindNeighbourEdges(pos)
			tile.Position = pos

			// if !couldAttach(tile, neighbourEdges) {
			// 	continue
			// }

			if canAttach(tile, neighbourEdges) {
				grid.Tiles[pos.Y][pos.X] = tile
				return true
			}
			for i := 0; i < 4; i++ {
				if tile.Rotate(); canAttach(tile, neighbourEdges) {
					grid.Tiles[pos.Y][pos.X] = tile
					return true
				}
			}

			if tile.Flip(); canAttach(tile, neighbourEdges) {
				grid.Tiles[pos.Y][pos.X] = tile
				return true
			}
			for i := 0; i < 4; i++ {
				if tile.Rotate(); canAttach(tile, neighbourEdges) {
					grid.Tiles[pos.Y][pos.X] = tile
					return true
				}
			}

			tile.Position = Vec2{-1, -1}
		}
	}

	return false
}

func couldAttach(tile *Tile, neighbourEdges [4]Edge) bool {
Outer:
	for _, otherEdge := range neighbourEdges {
		if len(otherEdge) == 0 {
			continue
		}

		for _, myEdge := range tile.PossibleEdges {
			if myEdge.Equals(otherEdge) {
				continue Outer
			}
		}

		return false
	}

	return true
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
	ID            int
	Data          [][]rune
	Position      Vec2
	Edges         [4]Edge
	PossibleEdges [8]Edge
}

func NewTile(id int, data [][]rune) *Tile {
	tile := &Tile{
		ID:       id,
		Data:     data,
		Position: Vec2{-1, -1},
		Edges:    edgesFromData(data),
	}

	possibleEdges := [8]Edge{}
	for i, edge := range tile.Edges {
		possibleEdges[i] = edge
		possibleEdges[i+4] = reverse(edge)
	}
	tile.PossibleEdges = possibleEdges

	// fmt.Printf("%d\n", id)
	// for _, edge := range possibleEdges {
	// 	fmt.Println(string(edge))
	// }
	// fmt.Println()
	// TODO: Debug flip edges

	return tile
}

func (t *Tile) Flip() {
	len := len(t.Data)
	newData := make([][]rune, len)

	for i, row := range t.Data {
		newData[len-i-1] = row
	}

	t.Data = newData
	t.Edges = edgesFromData(newData)
}

func (t *Tile) Rotate() {
	len := len(t.Data)
	newData := make([][]rune, len)
	for i := range newData {
		newData[i] = make([]rune, len)
	}

	for y := range t.Data {
		for x := range t.Data[y] {
			newData[y][x] = t.Data[len-x-1][y]
		}
	}

	t.Data = newData
	t.Edges = edgesFromData(newData)
}

func edgesFromData(data [][]rune) [4]Edge {
	len := len(data)
	top := make([]rune, len)
	copy(top, data[0])
	bottom := make([]rune, len)
	copy(bottom, data[len-1])

	right := make([]rune, len)
	left := make([]rune, len)
	for i, row := range data {
		right[i] = row[len-1]
		left[len-i-1] = row[0]
	}

	return [4]Edge{top, right, reverse(bottom), left}
}

func reverse(runes []rune) []rune {
	len := len(runes)
	newRunes := make([]rune, len)
	for i := range runes {
		newRunes[len-i-1] = runes[i]
	}

	return newRunes
}

type Grid struct {
	Tiles [][]*Tile
	Width int
}

func NewGrid(tileCount int) *Grid {
	width := int(math.Sqrt(float64(tileCount)))
	tiles := make([][]*Tile, width)
	for i := range tiles {
		tiles[i] = make([]*Tile, width)
	}

	return &Grid{
		Tiles: tiles,
		Width: width,
	}
}

func (g *Grid) GetNextPositions() []Vec2 {
	positions := []Vec2{}

	for y := range g.Tiles {
		for x := range g.Tiles[y] {
			if g.Tiles[y][x] != nil {
				continue
			}

			if x > 0 && g.Tiles[y][x-1] != nil {
				positions = append(positions, Vec2{x, y})
			}

			if x == 0 && y > 0 && g.Tiles[y-1][x] != nil {
				positions = append(positions, Vec2{x, y})
			}

			if x == 0 && y > 0 && g.Tiles[y-1][x] == nil {
				return positions
			}
		}
	}

	return positions
}

func (g *Grid) FindNeighbourEdges(pos Vec2) [4]Edge {
	edges := [4]Edge{}
	positions := [4]Vec2{
		{pos.X, pos.Y - 1},
		{pos.X + 1, pos.Y},
		{pos.X, pos.Y + 1},
		{pos.X - 1, pos.Y},
	}

	for i, p := range positions {
		if p.X < 0 || p.Y < 0 || p.X >= g.Width || p.Y >= g.Width {
			continue
		}

		neighbour := g.Tiles[p.Y][p.X]
		if neighbour != nil {
			edges[i] = reverse(neighbour.Edges[(i+2)%4])
		}
	}

	return edges
}

func (g *Grid) GenerateImage() *Image {
	tileWidth := len(g.Tiles[0][0].Data)
	imageData := [][]rune{}
	for _, tileRow := range g.Tiles {
		for rowCursor := 0; rowCursor < tileWidth; rowCursor++ {
			if rowCursor == 0 || rowCursor == tileWidth-1 {
				continue
			}

			imageRow := []rune{}
			for _, tile := range tileRow {
				for i, r := range tile.Data[rowCursor] {
					// if i == 0 {
					// 	imageRow = append(imageRow, ' ')
					// }

					if i == 0 || i == tileWidth-1 {
						continue
					}

					imageRow = append(imageRow, r)
				}
			}
			imageData = append(imageData, imageRow)
		}
		// imageData = append(imageData, []rune(""))
	}

	return &Image{imageData}
}

type SearchWindow [][]rune

type Image struct {
	Data [][]rune
}

func (i *Image) String() string {
	builder := strings.Builder{}
	for _, row := range i.Data {
		builder.WriteString(string(row))
		builder.WriteString("\n")
	}

	return builder.String()
}

func (i *Image) Rotate() {
	len := len(i.Data)
	newData := make([][]rune, len)
	for i := range newData {
		newData[i] = make([]rune, len)
	}

	for y := range i.Data {
		for x := range i.Data[y] {
			newData[y][x] = i.Data[len-x-1][y]
		}
	}

	i.Data = newData
}

func (i *Image) Flip() {
	len := len(i.Data)
	newData := make([][]rune, len)

	for i, row := range i.Data {
		newData[len-i-1] = row
	}

	i.Data = newData
}

func (im *Image) ReplaceMatches(window SearchWindow, replacement rune) bool {
	didReplace := false
	windowWidth := len(window[0])
	windowHeight := len(window)
	for y := 0; y+windowHeight < len(im.Data); y++ {
		for x := 0; x+windowWidth < len(im.Data[0]); x++ {
			if matchesWindow(im.Data, window, x, y) {
				didReplace = true
				replaceWindow(im.Data, window, x, y, replacement)
			}
		}
	}

	return didReplace
}

func (im *Image) ReplaceAllDirectionMatches(window SearchWindow, replacement rune) {
	for i := 0; i < 4; i++ {
		if im.ReplaceMatches(nessieWindow, replacement) {
			break
		}
		im.Rotate()
	}

	im.Flip()
	for i := 0; i < 4; i++ {
		if im.ReplaceMatches(nessieWindow, replacement) {
			break
		}
		im.Rotate()
	}
}

func matchesWindow(data [][]rune, window SearchWindow, x, y int) bool {
	for wy, windowRow := range window {
		for wx, windowRune := range windowRow {
			if windowRune == ' ' {
				continue
			}

			r := data[y+wy][x+wx]
			if r != windowRune {
				return false
			}
		}
	}

	return true
}

func replaceWindow(data [][]rune, window SearchWindow, x, y int, replacement rune) {
	for wy, windowRow := range window {
		for wx, windowRune := range windowRow {
			if windowRune != ' ' {
				data[y+wy][x+wx] = replacement
			}
		}
	}
}

func (im *Image) CountOccurances(want rune) int {
	sum := 0
	for _, row := range im.Data {
		for _, r := range row {
			if r == want {
				sum++
			}
		}
	}

	return sum
}

func initTcell() tcell.Screen {
	tcell.SetEncodingFallback(tcell.EncodingFallbackASCII)
	s, err := tcell.NewScreen()
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		os.Exit(1)
	}
	if err = s.Init(); err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		os.Exit(1)
	}

	s.SetStyle(tcell.StyleDefault.
		Foreground(tcell.ColorWhite).
		Background(tcell.ColorBlack))
	s.Clear()

	return s
}

func drawTiles(s tcell.Screen, tiles []*Tile) {
	s.Clear()
	for _, tile := range tiles {
		drawTile(s, tile)
	}
	s.Show()
}

func drawTile(s tcell.Screen, tile *Tile) {
	width := len(tile.Data) + 1
	height := width + 1

	writeString(s, fmt.Sprintf("Tile: %d", tile.ID), width*tile.Position.X, height*tile.Position.Y)
	for y, row := range tile.Data {
		for x, r := range row {
			s.SetContent(width*tile.Position.X+x, height*tile.Position.Y+y+1, r, nil, tcell.StyleDefault)
		}
	}
}

func writeString(s tcell.Screen, content string, x, y int) {
	for i, r := range content {
		s.SetContent(x+i, y, r, nil, tcell.StyleDefault)
	}
}

func waitForEsc(s tcell.Screen, do func(tcell.Screen, <-chan struct{})) {
	quit := make(chan struct{})
	go func() {
		for {
			ev := s.PollEvent()
			switch ev := ev.(type) {
			case *tcell.EventKey:
				switch ev.Key() {
				case tcell.KeyEscape, tcell.KeyEnter:
					close(quit)
					return
				case tcell.KeyCtrlL:
					s.Sync()
				}
			case *tcell.EventResize:
				s.Sync()
			}
		}
	}()

	do(s, quit)

	select {
	case <-quit:
	}
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
