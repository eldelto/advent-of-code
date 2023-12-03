package twentytwentythree

import (
	"bufio"
	"fmt"
	"io"
	"path/filepath"
	"strconv"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input03, part1Test03, part2Test03 = InputsForDay(3)

type schematicPartType int

const (
	dot = schematicPartType(iota)
	symbol
	number
)

type schematicPart interface {
	partType() schematicPartType
	value() int
}

type schematicDot struct{}

func (d *schematicDot) partType() schematicPartType {
	return dot
}

func (d *schematicDot) value() int {
	return 0
}

type schematicSymbol string

func (s *schematicSymbol) partType() schematicPartType {
	return symbol
}

func (s *schematicSymbol) value() int {
	return 0
}

type schematicNumber int

func (n *schematicNumber) partType() schematicPartType {
	return number
}

func (n *schematicNumber) value() int {
	return int(*n)
}

func (n *schematicNumber) width() int {
	return len(fmt.Sprintf("%d", *n))
}

type schematic struct {
	symbols [][]schematicPart
}

type schematicFakeNumber int

func (n *schematicFakeNumber) partType() schematicPartType {
	return number
}

func (n *schematicFakeNumber) value() int {
	return int(*n)
}

func NewSchematic() schematic {
	symbols := make([][]schematicPart, 140)
	for i := range symbols {
		symbols[i] = make([]schematicPart, 140)
	}

	return schematic{
		symbols: symbols,
	}
}

func (s *schematic) asNumber(row, column int) (*schematicNumber, bool) {
	n, ok := s.symbols[row][column].(*schematicNumber)
	return n, ok
}

func (s *schematic) isSymbol(row, column int) bool {
	_, ok := s.symbols[row][column].(*schematicSymbol)
	return ok
}

func (s *schematic) isType(row, column int, partType schematicPartType) (schematicPart, bool) {
	entry := s.symbols[row][column]
	ok := entry != nil && entry.partType() == partType
	return entry, ok
}

func (s *schematic) adjacentParts(row, column, width int, partType schematicPartType) []schematicPart {
	matches := []schematicPart{}

	// Check above and below.
	for i := -1; i <= width; i++ {
		searchColumn := column + i
		if searchColumn < 0 || searchColumn >= len(s.symbols) {
			continue
		}

		above := row - 1
		if above >= 0 {
			if entry, ok := s.isType(above, searchColumn, partType); ok {
				matches = append(matches, entry)
			}
		}

		below := row + 1
		if below < len(s.symbols) {
			if entry, ok := s.isType(below, searchColumn, partType); ok {
				matches = append(matches, entry)
			}
		}
	}

	// Check left and right.
	left := column - 1
	if left >= 0 {
		if entry, ok := s.isType(row, left, partType); ok {
			matches = append(matches, entry)
		}
	}

	right := column + width
	if right < len(s.symbols) {
		if entry, ok := s.isType(row, right, partType); ok {
			matches = append(matches, entry)
		}
	}

	return matches
}

type schematicParser struct {
	schematic schematic
	scanner   *bufio.Scanner
	row       int
	column    int
}

func isNumber(s string) bool {
	_, err := strconv.Atoi(s)
	return err == nil
}

func isSchematicSymbol(s string) bool {
	return s != "." && s != "\n" && !isNumber(s)
}

func (sp *schematicParser) parseNewline() {
	if sp.scanner.Text() == "\n" {
		sp.row++
		sp.column = 0
	}
}

func (sp *schematicParser) parseSchematicDot() {
	token := sp.scanner.Text()
	if token != "." {
		return
	}

	sp.schematic.symbols[sp.row][sp.column] = &schematicDot{}
	sp.column++
}

func (sp *schematicParser) parseSchematicSymbol() {
	token := sp.scanner.Text()
	if !isSchematicSymbol(token) {
		return
	}

	s := schematicSymbol(token)
	sp.schematic.symbols[sp.row][sp.column] = &s
	sp.column += 1
}

func (sp *schematicParser) parseSchematicNumber() error {
	token := sp.scanner.Text()
	if !isNumber(token) {
		return nil
	}

	rawNumber := token
	for sp.scanner.Scan() {
		token = sp.scanner.Text()
		if !isNumber(token) {
			break
		}

		rawNumber += token
	}

	number, err := strconv.Atoi(rawNumber)
	if err != nil {
		return fmt.Errorf("failed to convert '%s' to int", rawNumber)
	}

	n := schematicNumber(number)
	sp.schematic.symbols[sp.row][sp.column] = &n

	sp.column++
	for i := 1; i < len(rawNumber); i++ {
		fn := schematicFakeNumber(number)
		sp.schematic.symbols[sp.row][sp.column] = &fn
		sp.column++
	}

	return nil
}

func parseSchematic(r io.Reader) (*schematic, error) {
	scanner := bufio.NewScanner(r)
	scanner.Split(bufio.ScanRunes)

	parser := schematicParser{schematic: NewSchematic(), scanner: scanner}
	for scanner.Scan() {
		if err := parser.parseSchematicNumber(); err != nil {
			return nil, err
		}
		parser.parseNewline()
		parser.parseSchematicDot()
		parser.parseSchematicSymbol()
	}

	return &parser.schematic, nil
}

func numbersWithSymbols(s *schematic) []schematicNumber {
	result := []schematicNumber{}
	for row := range s.symbols {
		for column := range s.symbols[row] {
			number, ok := s.asNumber(row, column)
			if ok && len(s.adjacentParts(row, column, number.width(), symbol)) > 0 {
				result = append(result, *number)
			}
		}
	}

	return result
}

func gearRatios(s *schematic) []int {
	result := []int{}
	for row := range s.symbols {
		for column := range s.symbols[row] {
			if s.isSymbol(row, column) {
				matches := s.adjacentParts(row, column, 1, number)
				// Forgive me father, for I have assumed that no two adjacent
				// numbers are equal.
				values := Unique(Map(matches, func(p schematicPart) int { return p.value() }))
				if len(values) == 2 {
					result = append(result, values[0]*values[1])
				}
			}
		}
	}

	return result
}

func Test03Part1Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part1Test03))
	AssertNoError(t, err, "open file")
	defer file.Close()

	schematic, err := parseSchematic(file)
	AssertNoError(t, err, "parse schematic")

	numbers := numbersWithSymbols(schematic)
	sum := int(Sum(numbers))
	AssertEquals(t, 4361, sum, "sum")
}

func Test03Part1(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input03))
	AssertNoError(t, err, "open file")
	defer file.Close()

	schematic, err := parseSchematic(file)
	AssertNoError(t, err, "parse schematic")

	numbers := numbersWithSymbols(schematic)
	sum := int(Sum(numbers))
	AssertEquals(t, 546563, sum, "sum")
}

func Test03Part2Test(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", part2Test03))
	AssertNoError(t, err, "open file")
	defer file.Close()

	schematic, err := parseSchematic(file)
	AssertNoError(t, err, "parse schematic")

	ratios := gearRatios(schematic)
	sum := Sum(ratios)
	AssertEquals(t, 467835, sum, "sum")
}

func Test03Part2(t *testing.T) {
	file, err := inputsFS.Open(filepath.Join("inputs", input03))
	AssertNoError(t, err, "open file")
	defer file.Close()

	schematic, err := parseSchematic(file)
	AssertNoError(t, err, "parse schematic")

	ratios := gearRatios(schematic)
	sum := Sum(ratios)
	AssertEquals(t, 91031374, sum, "sum")
}
