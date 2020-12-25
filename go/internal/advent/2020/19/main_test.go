package main

import (
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
		{`0: 1 2 1 | 2 1
		1: "a"
		2: "b"`, "ba", true},
		{`0: 1 2
		1: 2 3 2 | 2 3
		2: "a"
		3: "b"`, "aba", true},
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
		{`0: 1
		1: 3 | 2 1
		2: "a"
		3: "b"`, "b", true},
		{`0: 1
		1: 3 | 2 1
		2: "a"
		3: "b"`, "ab", true},
		{`0: 1
		1: 3 | 2 1
		2: "a"
		3: "b"`, "aaaaab", true},
		{`0: 1
		1: 3 2 | 3 1 2
		2: "a"
		3: "b"`, "ba", true},
		{`0: 1
		1: 3 2 | 3 1 2
		2: "a"
		3: "b"`, "bbaa", true},
		{`0: 1
		1: 3 2 | 3 1 2
		2: "a"
		3: "b"`, "bbbbaaaa", true},
		{`42: 9 14 | 10 1
		9: 14 27 | 1 26
		10: 23 14 | 28 1
		1: "a"
		11: 42 31 | 42 11 31
		5: 1 14 | 15 1
		19: 14 1 | 14 14
		12: 24 14 | 19 1
		16: 15 1 | 14 14
		31: 14 17 | 1 13
		6: 14 14 | 1 14
		2: 1 24 | 14 4
		0: 8 11
		13: 14 3 | 1 12
		15: 1 | 14
		17: 14 2 | 1 7
		23: 25 1 | 22 14
		28: 16 1
		4: 1 1
		20: 14 14 | 1 15
		3: 5 14 | 16 1
		27: 1 6 | 14 18
		14: "b"
		21: 14 1 | 1 14
		25: 1 1 | 1 14
		22: 14 14
		8: 42 | 42 8
		26: 14 22 | 1 20
		18: 15 15
		7: 14 5 | 1 21
		24: 14 1`, "bbbbbbbaaaabbbbaaabbabaaa", true},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			fsm, _ := parseInput(tt.rules)
			got := matchInput(fsm, tt.input)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("parseInput() got = %v, want %v", got, tt.want)
			}
		})
	}
}
