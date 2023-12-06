package twentytwentythree

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input06, part1Test06, part2Test06 = InputsForDay(6)

type race struct {
	timeMs         uint
	recordDistance uint
}

func newRace(timeMs, recordDistance uint) race {
	return race{
		timeMs:         timeMs,
		recordDistance: recordDistance,
	}
}

func (r *race) calculateDistanceForCharge(chargeMs uint) uint {
	return (r.timeMs - chargeMs) * chargeMs
}

func (r *race) winningRuns() uint {
	var result uint

	for i := uint(1); i < r.timeMs; i++ {
		if r.calculateDistanceForCharge(i) > r.recordDistance {
			result++
		}
	}

	return result
}

func parseRaces(rawData []string) ([]race, error) {
	if len(rawData) != 2 {
		return nil, fmt.Errorf("failed to parse races from '%v'", rawData)
	}

	times, err := StringsToUInts(strings.Split(strings.Split(rawData[0], ":")[1], " "))
	if err != nil {
		return nil, err
	}

	records, err := StringsToUInts(strings.Split(strings.Split(rawData[1], ":")[1], " "))
	if err != nil {
		return nil, err
	}

	races := ZipMap(times, records, newRace)
	return races, nil
}

func parseSingleRace(rawData []string) (race, error) {
	if len(rawData) != 2 {
		return race{}, fmt.Errorf("failed to parse races from '%v'", rawData)
	}

	rawTime := strings.ReplaceAll(strings.Split(rawData[0], ":")[1], " ", "")
	time, err := strconv.ParseUint(rawTime, 10, 64)
	if err != nil {
		return race{}, fmt.Errorf("failed to parse time from '%s': %w", rawTime, err)
	}

	rawDistance := strings.ReplaceAll(strings.Split(rawData[1], ":")[1], " ", "")
	distance, err := strconv.ParseUint(rawDistance, 10, 64)
	if err != nil {
		return race{}, fmt.Errorf("failed to parse distance from '%s': %w", rawDistance, err)
	}

	return race{
		timeMs:         uint(time),
		recordDistance: uint(distance),
	}, nil
}

func Test06Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test06)
	AssertNoError(t, err, "InputToLines")

	races, err := parseRaces(lines)
	AssertNoError(t, err, "parseRaces")

	winningRuns := Map(races, func(r race) uint { return r.winningRuns() })
	product := Product(winningRuns)
	AssertEquals(t, uint(288), product, "product")
}

func Test06Part1(t *testing.T) {
	lines, err := InputToLines(input06)
	AssertNoError(t, err, "InputToLines")

	races, err := parseRaces(lines)
	AssertNoError(t, err, "parseRaces")

	winningRuns := Map(races, func(r race) uint { return r.winningRuns() })
	product := Product(winningRuns)
	AssertEquals(t, uint(625968), product, "product")
}

func Test06Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test06)
	AssertNoError(t, err, "InputToLines")

	race, err := parseSingleRace(lines)
	AssertNoError(t, err, "parseSingleRace")

	winningRuns := race.winningRuns()
	AssertEquals(t, uint(71503), winningRuns, "winning runs")
}

func Test06Part2(t *testing.T) {
	lines, err := InputToLines(input06)
	AssertNoError(t, err, "InputToLines")

	race, err := parseSingleRace(lines)
	AssertNoError(t, err, "parseSingleRace")

	winningRuns := race.winningRuns()
	AssertEquals(t, uint(43663323), winningRuns, "winning runs")
}
