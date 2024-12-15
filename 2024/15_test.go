package twentytwentyfour

import (
	"bytes"
	"fmt"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input15, part1Test15, part2Test15 = InputsForDay(15)

func parseWarehouseData(data string) (Matrix[GenericTile], []Direction) {
	parts := strings.Split(data, "\n\n")
	warehouse := ParseMatrix(bytes.NewBufferString(parts[0]))

	moves := []Direction{}
	for _, r := range parts[1] {
		switch r {
		case '^':
			moves = append(moves, North)
		case '>':
			moves = append(moves, East)
		case 'v':
			moves = append(moves, South)
		case '<':
			moves = append(moves, West)
		}
	}

	return warehouse, moves
}

func findRobot(warehouse Matrix[GenericTile]) Vec2 {
	tiles := warehouse.Filter(func(t GenericTile) bool {
		return t.symbol == '@'
	})
	return tiles[0].pos
}

func simulateWarehouseStep(warehouse Matrix[GenericTile], pos Vec2, move Direction) (Vec2, bool) {
	targetPos := pos.Add(Vec2(move))

	if !warehouse.WithinBounds(targetPos) {
		panic("leaving the warehouse")
	}
	targetTile := warehouse.Get(targetPos)

	switch targetTile.symbol {
	case '#':
		return pos, false
	case 'O':
		if _, didMove := simulateWarehouseStep(warehouse, targetPos, move); !didMove {
			// Do nothing if box did not move.
			return pos, false
		}
	}

	currentTile := warehouse.Get(pos)
	targetTile.symbol = currentTile.symbol
	warehouse.Set(targetPos, targetTile)
	warehouse.Set(pos, GenericTile{symbol: '.', pos: pos})
	return targetPos, true
}

func DrawMatrix(m Matrix[GenericTile]) {
	for _, row := range m {
		for _, tile := range row {
			fmt.Print(string(tile.symbol))
		}
		fmt.Println()
	}
}

func simulateWarehouse(warehouse Matrix[GenericTile], moves []Direction) {
	robot := findRobot(warehouse)

	for _, move := range moves {
		newPos, _ := simulateWarehouseStep(warehouse, robot, move)
		//DrawMatrix(warehouse)
		robot = newPos
	}
}

func calculateWarehouseGPS(warehouse Matrix[GenericTile]) int {
	boxes := warehouse.Filter(func(t GenericTile) bool {
		return t.symbol == 'O'
	})

	result := 0
	for _, b := range boxes {
		result += 100*b.pos.Y + b.pos.X
	}
	return result
}

func Test15Part1Test(t *testing.T) {
	data, err := InputToString(part1Test15)
	AssertNoError(t, err, "InputToString")

	warehouse, moves := parseWarehouseData(data)
	simulateWarehouse(warehouse, moves)
	result := calculateWarehouseGPS(warehouse)

	AssertEquals(t, 10092, result, "result")
}

func Test15Part1(t *testing.T) {
	data, err := InputToString(input15)
	AssertNoError(t, err, "InputToString")

	warehouse, moves := parseWarehouseData(data)
	simulateWarehouse(warehouse, moves)
	result := calculateWarehouseGPS(warehouse)

	AssertEquals(t, 10092, result, "result")
}

func Test15Part2Test(t *testing.T) {
}

func Test15Part2(t *testing.T) {
}
