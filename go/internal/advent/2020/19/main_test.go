package main

import (
	"fmt"
	"reflect"
	"testing"
)

func Test_parseInput(t *testing.T) {
	tests := []struct {
		rules string
		input string
		want  bool
	}{
		{`0: "a"`, "a", true},
		{`0: "a"`, "b", false},
		{`0: 1 2
		1: "a"
		2: "b"`, "ab", true},
		{`0: 1 2
		1: "a"
		2: "b"`, "ba", false},
		{`0: 1 2
		1: 2 3 | 3 2
		2: "a"
		3: "b"`, "aba", true},
		{`0: 1 2
		1: 2 3 | 3 2
		2: "a"
		3: "b"`, "baa", true},
		{`0: 1 2
		1: 2 3 | 3 2
		2: "a"
		3: "b"`, "bab", false},
		{`0: 1 2 | 2 1
		1: "a"
		2: "b"`, "ab", true},
		{`0: 1 2 | 2 1
		1: "a"
		2: "b"`, "ba", true},
		{`0: 1 2 | 2 1
		1: "a"
		2: "b"`, "aa", false},
		{`0: 1 2 | 2 1
		1: "a"
		2: "b"`, "bb", false},
		{`0: 4 1 5
		1: 2 3 | 3 2
		2: 4 4 | 5 5
		3: 4 5 | 5 4
		4: "a"
		5: "b"`, "ababbb", true},
		{`0: 4 1 5
		1: 2 3 | 3 2
		2: 4 4 | 5 5
		3: 4 5 | 5 4
		4: "a"
		5: "b"`, "abbbab", true},
		{`0: 4 1 5
		1: 2 3 | 3 2
		2: 4 4 | 5 5
		3: 4 5 | 5 4
		4: "a"
		5: "b"`, "aaabbb", false},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			fsm, _ := parseInput(tt.rules)
			fmt.Println(fsm)
			got := matchInput(fsm, tt.input)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("parseInput() got = %v, want %v", got, tt.want)
			}
		})
	}
}
