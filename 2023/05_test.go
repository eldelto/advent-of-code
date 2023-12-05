package twentytwentythree

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
	"golang.org/x/exp/slices"
)

var input05, part1Test05, part2Test05 = InputsForDay(5)

type almanachMapper struct {
	srcStart  uint
	destStart uint
	width     uint
}

func (am *almanachMapper) translate(x uint) uint {
	if x >= am.srcStart && x < am.srcStart+am.width {
		return x + (am.destStart - am.srcStart)
	}

	return x
}

func (am *almanachMapper) reverseTranslate(x uint) uint {
	if x >= am.destStart && x < am.destStart+am.width {
		return x - (am.destStart - am.srcStart)
	}

	return x
}

type mapperGroup []almanachMapper

func (mg mapperGroup) translate(x uint) uint {
	for _, mapper := range mg {
		res := mapper.translate(x)
		if x != res {
			return res
		}
	}
	return x
}

func (mg mapperGroup) reverseTranslate(x uint) uint {
	for _, mapper := range mg {
		res := mapper.reverseTranslate(x)
		if x != res {
			return res
		}
	}
	return x
}

func (mg mapperGroup) minDestRange() mappingRange {
	minDestRange := mappingRange{start: mg[0].destStart, width: mg[0].width}
	for _, m := range mg {
		if m.destStart < minDestRange.start {
			minDestRange.start = m.destStart
			minDestRange.width = m.width
		}
	}

	if minDestRange.start == 0 {
		return minDestRange
	}

	return mappingRange{start: 0, width: minDestRange.start}
}

func chainTranslate(x uint, mapperGroups ...mapperGroup) uint {
	for _, group := range mapperGroups {
		x = group.translate(x)
	}

	return x
}

func chainReverseTranslate(x uint, mapperGroups ...mapperGroup) uint {
	for _, group := range mapperGroups {
		x = group.reverseTranslate(x)
	}

	return x
}

type mappingRange struct {
	start uint
	width uint
}

func (mr *mappingRange) contains(x uint) bool {
	return x >= mr.start && x < (mr.start+mr.width)
}

func parseMapper(rawMapper string) (almanachMapper, error) {
	parts := strings.Split(rawMapper, " ")
	if len(parts) != 3 {
		return almanachMapper{}, fmt.Errorf("failed to parse mapper from '%s'", rawMapper)
	}

	values, err := StringsToUInts(parts)
	if err != nil {
		return almanachMapper{}, err
	}

	return almanachMapper{
		srcStart:  values[1],
		destStart: values[0],
		width:     values[2],
	}, nil
}

func parseMapperGroup(rawGroup string) (mapperGroup, error) {
	parts := strings.Split(rawGroup, "\n")
	mappers, err := MapWithErr(parts[1:], parseMapper)
	if err != nil {
		return nil, err
	}

	return mapperGroup(mappers), nil
}

func parseSeeds(rawSeeds string) ([]uint, error) {
	parts := strings.Split(rawSeeds, ":")
	return StringsToUInts(strings.Split(parts[1], " "))
}

func parseAlmanach(rawData string) ([]uint, []mapperGroup, error) {
	parts := strings.Split(rawData, "\n\n")

	seeds, err := parseSeeds(parts[0])
	if err != nil {
		return nil, nil, err
	}

	mapperGroups, err := MapWithErr(parts[1:], parseMapperGroup)
	if err != nil {
		return nil, nil, err
	}

	return seeds, mapperGroups, nil
}

func parseSeedGroups(rawSeeds string) ([]mappingRange, error) {
	parts := strings.Split(rawSeeds, ": ")

	seedGroups := []mappingRange{}
	group := mappingRange{}
	for i, value := range strings.Split(parts[1], " ") {
		x, err := strconv.ParseUint(value, 10, 64)
		if err != nil {
			return nil, err
		}

		if (i+1)%2 == 1 {
			group = mappingRange{start: uint(x)}
		} else {
			group.width = uint(x)
			seedGroups = append(seedGroups, group)
		}
	}

	return seedGroups, nil
}

func parseAlmanach2(rawData string) ([]mappingRange, []mapperGroup, error) {
	parts := strings.Split(rawData, "\n\n")

	seedGroups, err := parseSeedGroups(parts[0])
	if err != nil {
		return nil, nil, err
	}

	mapperGroups, err := MapWithErr(parts[1:], parseMapperGroup)
	if err != nil {
		return nil, nil, err
	}

	return seedGroups, mapperGroups, nil
}

func seedsForRange(mr mappingRange, mapperGroups ...mapperGroup) []uint {
	seeds := []uint{}
	for i := mr.start; i < (mr.start + mr.width); i++ {
		s := chainReverseTranslate(i, mapperGroups...)
		seeds = append(seeds, s)
	}

	return seeds
}

func availableSeeds(seedGroups []mappingRange, possibleSeeds []uint) []uint {
	availableSeeds := []uint{}
	for _, s := range possibleSeeds {
		for _, r := range seedGroups {
			if r.contains(s) {
				availableSeeds = append(availableSeeds, s)
			}
		}
	}

	return availableSeeds
}

func Test05Part1Test(t *testing.T) {
	content, err := InputToString(part1Test05)
	AssertNoError(t, err, "InputToString")

	seeds, mapperGroups, err := parseAlmanach(content)
	AssertNoError(t, err, "parseAlmanach")

	locations := Map(seeds, func(seed uint) uint {
		return chainTranslate(seed, mapperGroups...)
	})

	minLocation := slices.Min(locations)
	AssertEquals(t, uint(35), minLocation, "closes location")
}

func Test05Part1(t *testing.T) {
	content, err := InputToString(input05)
	AssertNoError(t, err, "InputToString")

	seeds, mapperGroups, err := parseAlmanach(content)
	AssertNoError(t, err, "parseAlmanach")

	locations := Map(seeds, func(seed uint) uint {
		return chainTranslate(seed, mapperGroups...)
	})

	minLocation := slices.Min(locations)
	AssertEquals(t, uint(289863851), minLocation, "closes location")
}

func Test05Part2Test(t *testing.T) {
	content, err := InputToString(part2Test05)
	AssertNoError(t, err, "InputToString")

	seedGroups, mapperGroups, err := parseAlmanach2(content)
	AssertNoError(t, err, "parseAlmanach")

	minRange := mapperGroups[len(mapperGroups)-1].minDestRange()

	reverseMapperGroups := make([]mapperGroup, len(mapperGroups))
	copy(reverseMapperGroups, mapperGroups)
	slices.Reverse(reverseMapperGroups)

	possibleSeeds := seedsForRange(minRange, reverseMapperGroups...)
	availableSeeds := availableSeeds(seedGroups, possibleSeeds)

	locations := Map(availableSeeds, func(seed uint) uint {
		return chainTranslate(seed, mapperGroups...)
	})

	minLocation := slices.Min(locations)
	AssertEquals(t, uint(46), minLocation, "closes location")
}

func Test05Part2(t *testing.T) {
	content, err := InputToString(input05)
	AssertNoError(t, err, "InputToString")

	seedGroups, mapperGroups, err := parseAlmanach2(content)
	AssertNoError(t, err, "parseAlmanach")

	minRange := mapperGroups[len(mapperGroups)-1].minDestRange()

	reverseMapperGroups := make([]mapperGroup, len(mapperGroups))
	copy(reverseMapperGroups, mapperGroups)
	slices.Reverse(reverseMapperGroups)

	possibleSeeds := seedsForRange(minRange, reverseMapperGroups...)
	availableSeeds := availableSeeds(seedGroups, possibleSeeds)

	locations := Map(availableSeeds, func(seed uint) uint {
		return chainTranslate(seed, mapperGroups...)
	})

	minLocation := slices.Min(locations)
	AssertEquals(t, uint(60568880), minLocation, "closes location")
}
