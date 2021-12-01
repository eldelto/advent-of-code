package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

func main() {

	// Example
	exampleAdapters := parseInput(exampleInput)
	sortAdapters(exampleAdapters)
	exampleResult := calculateJoltGapResult(exampleAdapters)
	fmt.Printf("Example result: %d\n", exampleResult)

	// Scenario 1
	adapters := parseInput(input)
	sortAdapters(adapters)
	result := calculateJoltGapResult(adapters)
	fmt.Printf("Scenario 1 result: %d\n", result)

	// Example 2
	exampleResult2 := countDistinctConnections(exampleAdapters)
	fmt.Printf("Example 2 result: %d\n", exampleResult2)

	// Scenario 2
	result2 := countDistinctConnections(adapters)
	fmt.Printf("Scenario 2 result: %d\n", result2)
}

func parseInput(input string) []Adapter {
	rawNumbers := strings.Split(input, "\n")
	adapters := make([]Adapter, len(rawNumbers))
	max := 0
	for i := range rawNumbers {
		number, err := strconv.Atoi(rawNumbers[i])
		if err != nil {
			panic(err)
		}

		if number > max {
			max = number
		}

		adapters[i] = NewAdapter(number)
	}

	// Add outlet and device adapter
	adapters = append(adapters, NewAdapter(0))
	adapters = append(adapters, NewAdapter(max+3))

	return adapters
}

func sortAdapters(adapters []Adapter) {
	sort.Slice(adapters, func(i, j int) bool { return adapters[i].MinJolts < adapters[j].MinJolts })
}

func countJoltGaps(adapters []Adapter, gapSize int) int {
	count := 0
	prev := adapters[0]
	for _, adapter := range adapters {
		if adapter.OutputJolts-prev.OutputJolts == gapSize {
			count++
		}
		prev = adapter
	}

	return count
}

func calculateJoltGapResult(adapters []Adapter) int {
	oneJoltCount := countJoltGaps(adapters, 1)
	threeJoltCount := countJoltGaps(adapters, 3)

	return oneJoltCount * threeJoltCount
}

func countDistinctConnections(adapters []Adapter) int {
	start := adapters[0]
	end := adapters[len(adapters)-1]

	return countPathToAdapter(start, end, adapters, map[Adapter]int{})
}

func countPathToAdapter(start, end Adapter, adapters []Adapter, cache map[Adapter]int) int {
	result, ok := cache[start]
	if ok {
		return result
	}

	count := 0
	for _, a := range adapters {
		if a != start && a.CanConnect(start) {
			count += countPathToAdapter(a, end, adapters, cache)
		}
	}

	if start == end {
		result = 1
	} else {
		result = count
	}

	cache[start] = result
	return result
}

// Adapter represents a Joltage adapter
type Adapter struct {
	OutputJolts int
	MinJolts    int
}

// NewAdapter returns a new Adapter with the specified output joltage.
func NewAdapter(outputJolts int) Adapter {
	return Adapter{
		OutputJolts: outputJolts,
		MinJolts:    outputJolts - 3,
	}
}

// CanConnect returns true if the two adapter have compatible joltage ratings.
func (a *Adapter) CanConnect(other Adapter) bool {
	return other.OutputJolts <= a.OutputJolts && other.OutputJolts >= a.MinJolts
}

const exampleInput = `16
10
15
5
1
11
7
19
6
12
4`

const input = `99
151
61
134
112
70
75
41
119
137
158
50
167
60
116
117
62
82
31
3
72
88
165
34
8
14
27
108
166
71
51
42
135
122
140
109
1
101
2
77
85
76
143
100
127
7
107
13
148
118
56
159
133
21
154
152
130
78
54
104
160
153
95
49
19
69
142
63
11
12
29
98
84
28
17
146
161
115
4
94
24
126
136
91
57
30
155
79
66
141
48
125
162
37
40
147
18
20
45
55
83`
