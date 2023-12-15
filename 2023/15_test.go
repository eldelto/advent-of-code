package twentytwentythree

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input15, part1Test15, _ = InputsForDay(15)

func lavaHash(s string) int {
	hash := 0
	for _, b := range []byte(s) {
		hash += int(b)
		hash *= 17
		hash %= 256
	}

	return hash
}

type lavaLens struct {
	focalLength int
	label       string
}

type lavaLensOperation struct {
	isAddOperation bool
	lens           lavaLens
}

type lavaLensBox struct {
	SortedHashMap[string, lavaLens]
}

func (b *lavaLensBox) focalPower() int {
	power := 0
	for i, entry := range b.list {
		power += (i + 1) * entry.value.focalLength
	}

	return power
}

func executeLensOperation(boxes *[256]lavaLensBox, op lavaLensOperation) {
	lens := op.lens
	hash := lavaHash(lens.label)
	box := &boxes[hash]

	if op.isAddOperation {
		box.Update(lens.label, lens)
	} else {
		box.Remove(lens.label)
	}
}

func calculateFocalPower(boxes *[256]lavaLensBox) int {
	power := 0
	for i, box := range boxes {
		power += (i + 1) * box.focalPower()
	}

	return power
}

func parseLensOperation(data string) (lavaLensOperation, error) {
	if data[len(data)-1] == '-' {
		label := data[:len(data)-1]
		op := lavaLensOperation{lens: lavaLens{label: label}}
		return op, nil
	}

	parts := strings.Split(data, "=")
	op := lavaLensOperation{lens: lavaLens{label: parts[0]}}

	op.isAddOperation = true
	focalLength, err := strconv.ParseInt(parts[1], 10, 64)
	if err != nil {
		return lavaLensOperation{}, fmt.Errorf("failed to parse focal length from %q: %w", data, err)
	}
	op.lens.focalLength = int(focalLength)

	return op, nil
}

func Test15Part1Test(t *testing.T) {
	values, err := InputToLinesWithSeparator(part1Test15, ",")
	AssertNoError(t, err, "InputToLines")

	hashes := Map(values, lavaHash)
	sum := Sum(hashes)
	AssertEquals(t, 1320, sum, "sum of hashes")
}

func Test15Part1(t *testing.T) {
	values, err := InputToLinesWithSeparator(input15, ",")
	AssertNoError(t, err, "InputToLines")

	hashes := Map(values, lavaHash)
	sum := Sum(hashes)
	AssertEquals(t, 519041, sum, "sum of hashes")
}

func Test15Part2Test(t *testing.T) {
	lines, err := InputToLinesWithSeparator(part1Test15, ",")
	AssertNoError(t, err, "InputToLines")

	operations, err := MapWithErr(lines, parseLensOperation)
	AssertNoError(t, err, "parseLensOperation")

	boxes := [256]lavaLensBox{}
	for i := range boxes {
		boxes[i] = lavaLensBox{SortedHashMap: NewSortedHashMap[string, lavaLens]()}
	}

	ForEach(operations, func(op lavaLensOperation) {
		executeLensOperation(&boxes, op)
	})

	power := calculateFocalPower(&boxes)
	AssertEquals(t, 145, power, "total focal power")
}

func Test15Part2(t *testing.T) {
	lines, err := InputToLinesWithSeparator(input15, ",")
	AssertNoError(t, err, "InputToLines")

	operations, err := MapWithErr(lines, parseLensOperation)
	AssertNoError(t, err, "parseLensOperation")

	boxes := [256]lavaLensBox{}
	for i := range boxes {
		boxes[i] = lavaLensBox{SortedHashMap: NewSortedHashMap[string, lavaLens]()}
	}

	ForEach(operations, func(op lavaLensOperation) {
		executeLensOperation(&boxes, op)
	})

	power := calculateFocalPower(&boxes)
	AssertEquals(t, 260530, power, "total focal power")
}
