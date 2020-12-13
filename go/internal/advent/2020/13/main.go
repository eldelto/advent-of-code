package main

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {

	// Example
	exampleTime, exampleBuses := parseInput(exampleInput)
	exampleResult := calculateResult(exampleTime, exampleBuses)
	fmt.Printf("Example result: %d\n", exampleResult)

	// Scenario 2
	time, buses := parseInput(input)
	result := calculateResult(time, buses)
	fmt.Printf("Scenario 1 result: %d\n", result)

	// Example 2
	exampleResult2 := calculateResult2(exampleBuses)
	fmt.Printf("Example 2 result: %d\n", exampleResult2)

	// Scenario 2
	result2 := calculateResult2(buses)
	fmt.Printf("Scenario 2 result: %d\n", result2)
}

func parseInput(input string) (int, []Bus) {
	parts := strings.Split(input, "\n")
	departure, err := strconv.Atoi(parts[0])
	if err != nil {
		panic(err)
	}

	buses := parseBuses(parts[1])

	return departure, buses
}

func parseBuses(input string) []Bus {
	rawBuses := strings.Split(input, ",")
	buses := []Bus{}
	for i, b := range rawBuses {
		id, err := strconv.Atoi(b)
		if err != nil {
			continue
		}

		bus := Bus{
			ID:       id,
			Position: i,
		}
		buses = append(buses, bus)
	}

	return buses
}

func findNextDepartingBus(timestamp int, buses []Bus) Bus {
	minDeparture := buses[0].NextDeparture(timestamp)
	bus := buses[0]
	for _, b := range buses {
		departure := b.NextDeparture(timestamp)
		if departure < minDeparture {
			minDeparture = departure
			bus = b
		}
	}

	return bus
}

func calculateResult(timestamp int, buses []Bus) int {
	bus := findNextDepartingBus(timestamp, buses)

	return bus.ID * (bus.NextDeparture(timestamp) - timestamp)
}

func findBusWithHighestID(buses []Bus) Bus {
	bus := buses[0]
	for _, b := range buses {
		if b.ID > bus.ID {
			bus = b
		}
	}

	return bus
}

func calculateResult2(buses []Bus) int {
	prod := 1
	for _, b := range buses {
		prod *= b.ID
	}

	x := 0
	for _, b := range buses {
		pp := prod / b.ID
		inv := inv(pp, b.ID)
		rem := b.ID - b.Position

		x += pp * inv * rem
	}

	return x % prod
}

func inv(pp, a int) int {
	for i := 0; true; i++ {
		if (pp*i)%a == 1 {
			return i
		}
	}

	return 0
}

type Bus struct {
	ID       int
	Position int
}

func (b *Bus) NextDeparture(timestamp int) int {
	departure := b.ID * (timestamp / b.ID)
	if timestamp%b.ID != 0 {
		departure += b.ID
	}

	return departure
}

const exampleInput = `939
7,13,x,x,59,x,31,19`

const input = `1008141
17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,23,x,x,x,x,x,x,x,787,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29`

/*
	a = 17, 13, 19
	m = 0, 2, 3

	prod = 17*13*19
	prod = 4199

	pp0 = 247
	pp1 = 323
	pp2 = 221

	inv0 = 2  // 247*2 % 17 = 1
	inv1 = 6  // 323*6 % 13 = 1
	inv2 = 8	// 221*8 % 19 = 1

	y0 = 0*247*2 = 0
	y1 = 2*323*6 = 3876
	y2 = 3*221*8 = 5304

	x = (0 + 3876 + 5304) % 4199 =
*/
