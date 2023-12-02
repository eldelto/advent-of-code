package twentytwentythree

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input02, part1Test02, part2Test02 = InputsForDay(2)

type cubeSet struct {
	red   uint
	green uint
	blue  uint
}

type cubeGameTurn cubeSet

func parseCubeGameTurn(s string) (cubeGameTurn, error) {
	turn := cubeGameTurn{}
	rawCubes := strings.Split(strings.TrimSpace(s), ", ")
	for _, rawCube := range rawCubes {
		parts := strings.Split(rawCube, " ")
		rawCount := parts[0]
		color := parts[1]

		count, err := strconv.ParseUint(rawCount, 10, 64)
		if err != nil {
			return cubeGameTurn{}, fmt.Errorf("failed to parse cube number from '%s': %w", s, err)
		}

		switch color {
		case "red":
			turn.red = uint(count)
		case "green":
			turn.green = uint(count)
		case "blue":
			turn.blue = uint(count)
		default:
			return cubeGameTurn{}, fmt.Errorf("failed to parse color from '%s'", s)
		}
	}

	return turn, nil
}

func parseCubeGameTurns(s string) ([]cubeGameTurn, error) {
	rawTurns := strings.Split(s, ";")
	return MapWithErr(rawTurns, parseCubeGameTurn)
}

type cubeGame struct {
	number uint
	turns  []cubeGameTurn
}

func parseCubeGame(line string) (cubeGame, error) {
	gameAndTurns := strings.Split(line, ":")
	if len(gameAndTurns) != 2 {
		return cubeGame{}, fmt.Errorf("failed to parse cubeGame from '%s'", line)
	}

	rawGame := gameAndTurns[0]
	rawTurns := gameAndTurns[1]

	gameParts := strings.Split(rawGame, " ")
	if len(gameParts) != 2 {
		return cubeGame{}, fmt.Errorf("failed to parse game parts from '%s'", line)
	}

	number, err := strconv.ParseUint(gameParts[1], 10, 64)
	if err != nil {
		return cubeGame{}, fmt.Errorf("failed to parse game number from '%s': %w", rawGame, err)
	}
	game := cubeGame{number: uint(number)}
	game.turns, err = parseCubeGameTurns(rawTurns)

	return game, err
}

func (cg *cubeGame) maxCubes() cubeSet {
	maxCubes := cubeSet{}
	for _, turn := range cg.turns {
		maxCubes.red = max(maxCubes.red, turn.red)
		maxCubes.green = max(maxCubes.green, turn.green)
		maxCubes.blue = max(maxCubes.blue, turn.blue)
	}

	return maxCubes
}

func isGamePossible(cg cubeGame, set cubeSet) bool {
	max := cg.maxCubes()
	return max.red <= set.red &&
		max.green <= set.green &&
		max.blue <= set.blue
}

func numberOfGameIfPossible(set cubeSet) func(cg cubeGame) uint {
	return func(cg cubeGame) uint {
		if isGamePossible(cg, set) {
			return cg.number
		}

		return 0
	}
}

func powerOfGame(cg cubeGame) uint {
	max := cg.maxCubes()
	return max.red * max.green * max.blue
}

var testSet = cubeSet{
	red:   12,
	green: 13,
	blue:  14,
}

func Test02Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test02)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCubeGame)
	AssertNoError(t, err, "parseCubeGame")

	numbers := Map(games, numberOfGameIfPossible(testSet))
	sum := Sum(numbers)
	AssertEquals(t, uint(8), sum, "sum")
}

func Test02Part1(t *testing.T) {
	lines, err := InputToLines(input02)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCubeGame)
	AssertNoError(t, err, "parseCubeGame")

	numbers := Map(games, numberOfGameIfPossible(testSet))
	sum := Sum(numbers)
	AssertEquals(t, uint(3035), sum, "sum")
}

func Test02Part2Test(t *testing.T) {
	lines, err := InputToLines(part1Test02)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCubeGame)
	AssertNoError(t, err, "parseCubeGame")

	powers := Map(games, powerOfGame)
	sum := Sum(powers)
	AssertEquals(t, uint(2286), sum, "sum")
}

func Test02Part2(t *testing.T) {
	lines, err := InputToLines(input02)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCubeGame)
	AssertNoError(t, err, "parseCubeGame")

	powers := Map(games, powerOfGame)
	sum := Sum(powers)
	AssertEquals(t, uint(66027), sum, "sum")
}
