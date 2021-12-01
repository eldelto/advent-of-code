package main

import (
	"fmt"
)

func main() {

	// Example
	exampleGame := NewMemoryGame(0, 3, 6)
	exampleResult := findNumberAtTurn(exampleGame, 10)
	fmt.Printf("Example result: %d\n", exampleResult)

	// Scenario 1
	game := NewMemoryGame(11, 18, 0, 20, 1, 7, 16)
	result := findNumberAtTurn(game, 2020)
	fmt.Printf("Scenario 1 result: %d\n", result)

	// Scenario 2
	game2 := NewMemoryGame(11, 18, 0, 20, 1, 7, 16)
	result2 := findNumberAtTurn(game2, 30000000)
	fmt.Printf("Scenario 2 result: %d\n", result2)
}

func findNumberAtTurn(game *MemoryGame, turn int) int {
	for game.Turn < turn {
		game.NextRound()
	}

	return game.SpokenNumber
}

type MemoryGame struct {
	history      map[int]int
	SpokenNumber int
	Turn         int
}

func NewMemoryGame(seed ...int) *MemoryGame {
	history := map[int]int{}
	for i, v := range seed[:len(seed)-1] {
		history[v] = i + 1
	}

	spokenNumber := seed[len(seed)-1]

	return &MemoryGame{
		history:      history,
		SpokenNumber: spokenNumber,
		Turn:         len(history) + 1,
	}
}

func (mg *MemoryGame) NextRound() {
	nextNumber := 0
	turn, ok := mg.history[mg.SpokenNumber]
	if ok {
		nextNumber = mg.Turn - turn
	}

	mg.history[mg.SpokenNumber] = mg.Turn
	mg.SpokenNumber = nextNumber
	mg.Turn++
}
