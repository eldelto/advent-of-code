package twentytwentyfour

import (
	"strconv"
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input11, part1Test11, part2Test11 = InputsForDay(11)

type stonesResult struct {
	value int
	times int
}

var cache = map[stonesResult]int{}

func evolveStone(stone, times int) int {
	key := stonesResult{stone, times}
	if times == 0 {
		return 1
	}

	if result, ok := cache[key]; ok {
		return result
	}

	if stone == 0 {
		result := evolveStone(1, times-1)
		cache[key] = result
		return result
	}

	str := strconv.Itoa(stone)
	strLen := len(str)
	if strLen%2 != 0 {
		result := evolveStone(stone*2024, times-1)
		cache[key] = result
		return result
	}

	a, err := strconv.Atoi(str[:strLen/2])
	if err != nil {
		panic(err)
	}
	b, err := strconv.Atoi(str[strLen/2:])
	if err != nil {
		panic(err)
	}

	result := evolveStone(a, times-1) + evolveStone(b, times-1)
	cache[key] = result
	return result
}

func evolveStones(stones []int, times int) int {
	result := 0
	for _, s := range stones {
		result += evolveStone(s, times)
	}

	return result
}

func Test11Part1Test(t *testing.T) {
	data, err := InputToString(part1Test11)
	AssertNoError(t, err, "InputToString")

	stones, err := StringToInts(" ")(data)
	AssertNoError(t, err, "StringToInts")

	count := evolveStones(stones, 25)
	AssertEquals(t, 55312, count, "stone count")
}

func Test11Part1(t *testing.T) {
	data, err := InputToString(input11)
	AssertNoError(t, err, "InputToString")

	stones, err := StringToInts(" ")(data)
	AssertNoError(t, err, "StringToInts")

	count := evolveStones(stones, 25)
	AssertEquals(t, 217443, count, "stone count")
}

func Test11Part2(t *testing.T) {
	data, err := InputToString(input11)
	AssertNoError(t, err, "InputToString")

	stones, err := StringToInts(" ")(data)
	AssertNoError(t, err, "StringToInts")

	count := evolveStones(stones, 75)
	AssertEquals(t, 257246536026785, count, "stone count")
}
