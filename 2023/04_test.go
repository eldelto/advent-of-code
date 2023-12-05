package twentytwentythree

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input04, part1Test04, part2Test04 = InputsForDay(4)

type cardGame struct {
	index          uint
	winningNumbers []uint
	playedNumbers  []uint
}

func (g *cardGame) MatchingNumbersCount() uint {
	var matches uint
	for _, playedNumber := range g.playedNumbers {
		for _, winningNumber := range g.winningNumbers {
			if playedNumber == winningNumber {
				matches++
			}
		}
	}

	return matches
}

func parseCardGame(line string) (*cardGame, error) {
	parts := strings.Split(line, ": ")
	rawGame := parts[0]
	rawNumbers := parts[1]

	gameNumber := ""
	for _, s := range strings.Split(rawGame, " ")[1:] {
		if s != "" {
			gameNumber = s
			break
		}
	}

	index, err := strconv.ParseUint(gameNumber, 10, 64)
	if err != nil {
		return nil, fmt.Errorf("failed to parse game number from '%s': %w", gameNumber, err)
	}

	numberParts := strings.Split(rawNumbers, "|")
	winningNumbers, err := StringsToUInts(strings.Split(numberParts[0], " "))
	if err != nil {
		return nil, err
	}
	playedNumbers, err := StringsToUInts(strings.Split(numberParts[1], " "))
	if err != nil {
		return nil, err
	}

	game := cardGame{
		index:          uint(index),
		winningNumbers: winningNumbers,
		playedNumbers:  playedNumbers,
	}

	return &game, nil
}

func pointsOfGame(g *cardGame) uint {
	matches := g.MatchingNumbersCount()
	if matches < 1 {
		return 0
	}

	return uint(math.Pow(2, float64(matches-1)))
}

var matchCache = map[uint]uint{}

func recursiveScratchCards(globalGames []*cardGame, cardGames []*cardGame) uint {
	var cards uint
	for _, game := range cardGames {
		cards++

		if cachedCards, ok := matchCache[game.index]; ok {
			cards += cachedCards
		} else {
			matches := game.MatchingNumbersCount()
			newCards := recursiveScratchCards(globalGames, globalGames[game.index:game.index+matches])
			matchCache[game.index] = newCards
			cards += newCards
		}
	}

	return cards
}

func Test04Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test04)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCardGame)
	AssertNoError(t, err, "parseCardGame")

	points := Map(games, pointsOfGame)
	totalPoints := Sum(points)
	AssertEquals(t, uint(13), totalPoints, "total points")
}

func Test04Part1(t *testing.T) {
	lines, err := InputToLines(input04)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCardGame)
	AssertNoError(t, err, "parseCardGame")

	points := Map(games, pointsOfGame)
	totalPoints := Sum(points)
	AssertEquals(t, uint(25004), totalPoints, "total points")
}

func Test04Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test04)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCardGame)
	AssertNoError(t, err, "parseCardGame")

	matchCache = map[uint]uint{}
	cards := recursiveScratchCards(games, games)
	AssertEquals(t, uint(30), cards, "total cards")
}

func Test04Part2(t *testing.T) {
	lines, err := InputToLines(input04)
	AssertNoError(t, err, "InputToLines")

	games, err := MapWithErr(lines, parseCardGame)
	AssertNoError(t, err, "parseCardGame")

	matchCache = map[uint]uint{}
	cards := recursiveScratchCards(games, games)
	AssertEquals(t, uint(14427616), cards, "total cards")
}
