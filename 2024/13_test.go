package twentytwentyfour

import (
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input13, part1Test13, part2Test13 = InputsForDay(13)

func parseClawData(data string, offset int) (a, b, price Vec2) {
	parts := strings.Split(data, "\n")

	aParts := strings.Split(parts[0], "+")
	aX, err := strconv.Atoi(strings.Trim(aParts[1], ", Y"))
	if err != nil {
		panic(err)
	}
	aY, err := strconv.Atoi(strings.Trim(aParts[2], " ,"))
	if err != nil {
		panic(err)
	}
	a.X = aX
	a.Y = aY

	bParts := strings.Split(parts[1], "+")
	bX, err := strconv.Atoi(strings.Trim(bParts[1], ", Y"))
	if err != nil {
		panic(err)
	}
	bY, err := strconv.Atoi(strings.Trim(bParts[2], " ,"))
	if err != nil {
		panic(err)
	}
	b.X = bX
	b.Y = bY

	priceParts := strings.Split(parts[2], "=")
	priceX, err := strconv.Atoi(strings.Trim(priceParts[1], ", Y"))
	if err != nil {
		panic(err)
	}
	priceY, err := strconv.Atoi(strings.Trim(priceParts[2], " ,"))
	if err != nil {
		panic(err)
	}
	price.X = priceX + offset
	price.Y = priceY + offset

	return
}

func parseAllClawData(data string, offset int) (as, bs, prices []Vec2) {
	parts := strings.Split(data, "\n\n")
	for _, part := range parts {
		a, b, price := parseClawData(part, offset)
		as = append(as, a)
		bs = append(bs, b)
		prices = append(prices, price)
	}

	return
}

func playClaw(a, b, price Vec2, reversed bool) int {
	priceIncline := float64(price.X) / float64(price.Y)
	//reversed := b.X > a.X
	pos := Vec2{}
	cost := 0

	for pos.X < price.X && pos.Y < price.Y {
		if pos.Y == 0 || float64(pos.X)/float64(pos.Y) < priceIncline {
			if reversed {
				pos = pos.Add(b)
				cost += 1
			} else {
				pos = pos.Add(a)
				cost += 3
			}
		} else {
			if reversed {
				pos = pos.Add(a)
				cost += 3
			} else {
				pos = pos.Add(b)
				cost += 1
			}
		}
	}

	if pos == price {
		return cost
	}
	return 0
}

func playClawFast(a, b, price Vec2) int {
	// TODO: Calculate, don't simulate.
	return 0
}

func playClawGames(as, bs, prices []Vec2) int {
	costs := 0
	for i := range prices {
		cost := playClaw(as[i], bs[i], prices[i], false)
		if cost == 0 {
			cost = playClaw(as[i], bs[i], prices[i], true)
		}
		costs += cost
	}
	return costs
}

func playClawGamesFast(as, bs, prices []Vec2) int {
	costs := 0
	for i := range prices {
		costs += playClawFast(as[i], bs[i], prices[i])
	}
	return costs
}

func Test13Part1Test(t *testing.T) {
	data, err := InputToString(part1Test13)
	AssertNoError(t, err, "InputToString")

	as, bs, prices := parseAllClawData(data, 0)
	costs := playClawGames(as, bs, prices)
	AssertEquals(t, 480, costs, "total cost")
}

func Test13Part1(t *testing.T) {
	data, err := InputToString(input13)
	AssertNoError(t, err, "InputToString")

	as, bs, prices := parseAllClawData(data, 0)
	costs := playClawGames(as, bs, prices)
	AssertEquals(t, 29517, costs, "total cost")
}

func Test13Part2Test(t *testing.T) {
	t.Skip()
	data, err := InputToString(part1Test13)
	AssertNoError(t, err, "InputToString")

	as, bs, prices := parseAllClawData(data, 10000000000000)
	costs := playClawGamesFast(as, bs, prices)
	AssertEquals(t, 480, costs, "total cost")
}

func Test13Part2(t *testing.T) {
}
