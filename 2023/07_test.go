package twentytwentythree

import (
	"fmt"
	"math"
	"slices"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input07, part1Test07, part2Test07 = InputsForDay(7)

type camelCard uint

const (
	camelCardJoker camelCard = iota
	camelCard2
	camelCard3
	camelCard4
	camelCard5
	camelCard6
	camelCard7
	camelCard8
	camelCard9
	camelCardT
	camelCardJ
	camelCardQ
	camelCardK
	camelCardA
)

type camelCardHandType uint

const (
	highCard camelCardHandType = iota
	onePair
	twoPair
	threeOfKind
	fullHouse
	fourOfKind
	fiveOfKind
)

type camelCardHand struct {
	cards    [5]camelCard
	bid      uint
	value    uint
	handType camelCardHandType
}

func newCamelCardHand(cards [5]camelCard, bid uint, withJoker bool) camelCardHand {
	return camelCardHand{
		cards:    cards,
		bid:      bid,
		value:    handBaseValue(cards),
		handType: handType(cards, withJoker),
	}
}

func (h *camelCardHand) compare(other camelCardHand) int {
	if h.handType == other.handType {
		return int(h.value) - int(other.value)
	}

	return int(h.handType) - int(other.handType)
}

func handType(cards [5]camelCard, withJoker bool) camelCardHandType {
	cardCount := map[camelCard]uint{}
	for _, card := range cards {
		count, ok := cardCount[card]
		if !ok {
			count = 0
		}
		cardCount[card] = count + 1
	}

	var highest uint
	var secondHighest uint
	for k, v := range cardCount {
		if k == camelCardJoker {
			continue
		}

		if v > highest {
			secondHighest = highest
			highest = v
			continue
		}
		if v > secondHighest {
			secondHighest = v
		}
	}

	if withJoker {
		highest += cardCount[camelCardJoker]
	}

	switch {
	case highest == 2 && secondHighest == 2:
		return twoPair
	case highest == 3 && secondHighest == 2:
		return fullHouse
	case highest == 1:
		return highCard
	case highest == 2:
		return onePair
	case highest == 3:
		return threeOfKind
	case highest == 4:
		return fourOfKind
	case highest == 5:
		return fiveOfKind
	}

	panic(fmt.Sprintf("invalid hand '%v'", cards))
}

func handBaseValue(cards [5]camelCard) uint {
	var baseValue uint
	for i, card := range cards {
		baseValue += uint(float64(card+1) * math.Pow10((len(cards)-i)*2))
	}

	return baseValue
}

func parseCamelCard(r rune, withJoker bool) (camelCard, error) {
	if r >= 50 && r <= 57 {
		return camelCard(r - 49), nil
	}

	switch r {
	case 'T':
		return camelCardT, nil
	case 'J':
		if withJoker {
			return camelCardJoker, nil
		}
		return camelCardJ, nil
	case 'Q':
		return camelCardQ, nil
	case 'K':
		return camelCardK, nil
	case 'A':
		return camelCardA, nil
	}

	return camelCard2, fmt.Errorf("failed to parse camel card from '%c'", r)
}

func parseCamelCardHandWithJoker(rawHand string, withJoker bool) (camelCardHand, error) {
	parts := strings.Split(rawHand, " ")

	cards := [5]camelCard{}
	for i, r := range parts[0] {
		card, err := parseCamelCard(r, withJoker)
		if err != nil {
			return camelCardHand{}, err
		}
		cards[i] = card
	}

	bid, err := strconv.ParseUint(parts[1], 10, 64)
	if err != nil {
		return camelCardHand{}, fmt.Errorf("failed to parse bid from '%s': %w", parts[1], err)
	}

	return newCamelCardHand(cards, uint(bid), withJoker), nil
}

func parseCamelCardHand(rawHand string) (camelCardHand, error) {
	return parseCamelCardHandWithJoker(rawHand, false)
}

func parseJokerCardHand(rawHand string) (camelCardHand, error) {
	return parseCamelCardHandWithJoker(rawHand, true)
}

func camelCardWinnings(hands []camelCardHand) []uint {
	slices.SortFunc(hands, func(a, b camelCardHand) int {
		return a.compare(b)
	})

	winnings := make([]uint, len(hands))
	for i, hand := range hands {
		winnings[i] = uint(i+1) * hand.bid
	}

	return winnings
}

func Test07Part1Test(t *testing.T) {
	lines, err := InputToLines(part1Test07)
	AssertNoError(t, err, "InputToLines")

	hands, err := MapWithErr(lines, parseCamelCardHand)
	AssertNoError(t, err, "parseCamelCardHand")

	winnings := camelCardWinnings(hands)
	totalWinnings := Sum(winnings)
	AssertEquals(t, uint(6440), totalWinnings, "total winnings")
}

func Test07Part1(t *testing.T) {
	lines, err := InputToLines(input07)
	AssertNoError(t, err, "InputToLines")

	hands, err := MapWithErr(lines, parseCamelCardHand)
	AssertNoError(t, err, "parseCamelCardHand")

	winnings := camelCardWinnings(hands)
	totalWinnings := Sum(winnings)
	AssertEquals(t, uint(246409899), totalWinnings, "total winnings")
}

func Test07Part2Test(t *testing.T) {
	lines, err := InputToLines(part2Test07)
	AssertNoError(t, err, "InputToLines")

	hands, err := MapWithErr(lines, parseJokerCardHand)
	AssertNoError(t, err, "parseCamelCardHand")

	winnings := camelCardWinnings(hands)
	totalWinnings := Sum(winnings)
	AssertEquals(t, uint(5905), totalWinnings, "total winnings")
}

func Test07Part2(t *testing.T) {
	lines, err := InputToLines(input07)
	AssertNoError(t, err, "InputToLines")

	hands, err := MapWithErr(lines, parseJokerCardHand)
	AssertNoError(t, err, "parseCamelCardHand")

	winnings := camelCardWinnings(hands)
	totalWinnings := Sum(winnings)
	AssertEquals(t, uint(244848487), totalWinnings, "total winnings")
}

func Test_camelCardHand_handType(t *testing.T) {
	tests := []struct {
		h    camelCardHand
		want camelCardHandType
	}{
		{newCamelCardHand([5]camelCard{camelCard2, camelCard3, camelCard4, camelCard5, camelCard6}, 0, false), highCard},
		{newCamelCardHand([5]camelCard{camelCardK, camelCardK, camelCard4, camelCard5, camelCard6}, 0, false), onePair},
		{newCamelCardHand([5]camelCard{camelCardK, camelCardK, camelCardQ, camelCardQ, camelCard6}, 0, false), twoPair},
		{newCamelCardHand([5]camelCard{camelCardK, camelCardK, camelCardK, camelCard5, camelCard6}, 0, false), threeOfKind},
		{newCamelCardHand([5]camelCard{camelCardK, camelCardK, camelCardK, camelCardQ, camelCardQ}, 0, false), fullHouse},
		{newCamelCardHand([5]camelCard{camelCardK, camelCardK, camelCardK, camelCardK, camelCard6}, 0, false), fourOfKind},
		{newCamelCardHand([5]camelCard{camelCardK, camelCardK, camelCardK, camelCardK, camelCardK}, 0, false), fiveOfKind},
	}
	for _, tt := range tests {
		t.Run(fmt.Sprintf("%v", tt.want), func(t *testing.T) {
			AssertEquals(t, tt.want, tt.h.handType, "handType")
		})
	}
}

func Test_camelCardHand_calcValue(t *testing.T) {
	fourOfKind := newCamelCardHand([5]camelCard{camelCardA, camelCardA, camelCardA, camelCardA, camelCardQ}, 0, false)
	fiveOfKind := newCamelCardHand([5]camelCard{camelCard2, camelCard2, camelCard2, camelCard2, camelCard2}, 0, false)
	highest := newCamelCardHand([5]camelCard{camelCardA, camelCardA, camelCardA, camelCardA, camelCardA}, 0, false)

	AssertEquals(t, true, fiveOfKind.compare(fourOfKind) > 0, "calcValue")
	AssertEquals(t, true, highest.compare(fiveOfKind) > 0, "highest value")

	AssertEquals(t, true, fourOfKind.compare(fiveOfKind) < 0, "calcValue")
	AssertEquals(t, true, fiveOfKind.compare(highest) < 0, "highest value")

	AssertEquals(t, true, fiveOfKind.compare(fiveOfKind) == 0, "equal")
}
