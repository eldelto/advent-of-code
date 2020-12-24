package main

import (
	"fmt"
	"strings"
)

func main() {

	// Example
	exampleMachine, exampleInputs := parseInput(exampleInput)
	exampleResult := countMatches(exampleMachine, exampleInputs)
	fmt.Printf("Example result: %d\n", exampleResult)

	// Scenario 1
	machine, inputs := parseInput(input)
	result := countMatches(machine, inputs)
	fmt.Printf("Scenario 1 result: %d\n", result)

	// Example 2
	exampleMachine2, exampleInputs2 := parseInput(exampleInput2)
	exampleResult2 := countMatches(exampleMachine2, exampleInputs2)
	fmt.Printf("Example 2 result: %d\n", exampleResult2)
}

func countMatches(machine FSM, inputs []string) int {
	sum := 0
	for _, v := range inputs {
		machine.Reset()
		if matchInput(machine, v) {
			sum++
		}
	}

	return sum
}

func parseInput(input string) (FSM, []string) {
	parts := strings.Split(input, "\n\n")
	fsm := parseFSM(parts[0])
	inputs := []string{}
	if len(parts) == 2 {
		inputs = parseInputs(parts[1])
	}

	return fsm, inputs
}

func parseInputs(input string) []string {
	return strings.Split(input, "\n")
}

func parseFSM(input string) FSM {
	rules := map[string]string{}
	for _, row := range strings.Split(input, "\n") {
		parts := strings.Split(row, ":")
		id := strings.TrimSpace(parts[0])
		rule := strings.TrimSpace(parts[1])

		rules[id] = rule
	}

	return parseRecursiveFSM(rules, "0")
}

func parseRecursiveFSM(rules map[string]string, id string) FSM {
	return NewLazyStateMachine(func() FSM {
		rule, ok := rules[id]
		if !ok {
			panic(fmt.Errorf("rule with ID %s does not exist", id))
		}

		if strings.ContainsRune(rule, '"') {
			return parseRune(rule)
		} else if strings.ContainsRune(rule, '|') {
			parts := strings.Split(rule, "|")
			return NewAlternateStateMachine(
				parseConcat(rules, parts[0]),
				parseConcat(rules, parts[1]),
			)
		}

		return parseConcat(rules, rule)
	})
}

func parseRune(rule string) FSM {
	r := []rune(strings.Trim(rule, "\""))[0]
	return NewRuneMachine(r)
}

func parseConcat(rules map[string]string, rule string) FSM {
	rule = strings.TrimSpace(rule)
	ids := strings.Split(rule, " ")

	machines := []FSM{}
	for _, id := range ids {
		fsm := parseRecursiveFSM(rules, id)
		machines = append(machines, fsm)
	}

	return NewConcatStateMachine(machines...)
}

func matchInput(machine FSM, input string) bool {
	for _, r := range input {
		if machine.Transition(Event(r)) != nil {
			return false
		}
	}

	return machine.IsAccepting()
}

type Event rune

type Transition struct {
	Event       Event
	Origin      State
	Destination State
}

type State struct {
	Name      string
	Accepting bool
}

type FSM interface {
	IsAccepting() bool
	Transition(Event) error
	Reset()
	String() string
}

type FSMBuilder func() FSM

type LazyStateMachine struct {
	builder FSMBuilder
	machine FSM
}

func NewLazyStateMachine(builder FSMBuilder) *LazyStateMachine {
	return &LazyStateMachine{
		builder: builder,
		machine: nil,
	}
}

func (sm *LazyStateMachine) IsAccepting() bool {
	if sm.machine == nil {
		sm.machine = sm.builder()
	}

	return sm.machine.IsAccepting()
}

func (sm *LazyStateMachine) Transition(event Event) error {
	if sm.machine == nil {
		sm.machine = sm.builder()
	}

	return sm.machine.Transition(event)
}

func (sm *LazyStateMachine) Reset() {
	sm.machine = nil
}

func (sm *LazyStateMachine) String() string {
	if sm.machine == nil {
		sm.machine = sm.builder()
	}

	return sm.machine.String()
}

type StateMachine struct {
	initialState State
	currentState State
	states       []State
	transitions  []Transition
}

func NewStateMachine(initialState State, states []State, transitions []Transition) *StateMachine {
	return &StateMachine{
		initialState: initialState,
		currentState: initialState,
		states:       states,
		transitions:  transitions,
	}
}

func (sm *StateMachine) IsAccepting() bool {
	return sm.currentState.Accepting
}

func (sm *StateMachine) Transition(event Event) error {
	for _, t := range sm.transitions {
		if t.Event == event && t.Origin == sm.currentState {
			sm.currentState = t.Destination
			return nil
		}
	}

	sm.currentState = State{"too long", false}
	return fmt.Errorf("no matching transition found for event: %c", event)
}

func (sm *StateMachine) Reset() {
	sm.currentState = sm.initialState
}

func (sm *StateMachine) String() string {
	return fmt.Sprintf("%c", sm.transitions[0].Event)
}

func NewRuneMachine(r rune) *StateMachine {
	noMatch := State{"no match", false}
	match := State{"match", true}

	transitions := []Transition{
		{Event(r), noMatch, match},
	}

	return NewStateMachine(noMatch, []State{noMatch, match}, transitions)
}

type ConcatStateMachine struct {
	stateMachines []FSM
	machineCursor int
}

func NewConcatStateMachine(stateMachines ...FSM) *ConcatStateMachine {
	return &ConcatStateMachine{
		stateMachines: stateMachines,
		machineCursor: 0,
	}
}

func (sm *ConcatStateMachine) IsAccepting() bool {
	len := len(sm.stateMachines)
	return sm.stateMachines[len-1].IsAccepting()
}

func (sm *ConcatStateMachine) Transition(event Event) error {
	machine := sm.stateMachines[sm.machineCursor]

	if err := machine.Transition(event); err != nil {
		return err
	}

	if machine.IsAccepting() && sm.machineCursor+1 < len(sm.stateMachines) {
		sm.machineCursor++
	}

	return nil
}

func (sm *ConcatStateMachine) Reset() {
	for i := sm.machineCursor; i >= 0; i-- {
		sm.stateMachines[i].Reset()
	}

	sm.machineCursor = 0
}

func (sm *ConcatStateMachine) String() string {
	builder := strings.Builder{}
	builder.WriteRune('(')
	for i, machine := range sm.stateMachines {
		if i > 0 {
			builder.WriteString(" & ")
		}
		builder.WriteString(machine.String())
	}
	builder.WriteRune(')')

	return builder.String()
}

type AlternateStateMachine struct {
	stateMachines []FSM
	events        []Event
	machineCursor int
}

func NewAlternateStateMachine(stateMachines ...FSM) *AlternateStateMachine {
	return &AlternateStateMachine{
		stateMachines: stateMachines,
		events:        []Event{},
		machineCursor: 0,
	}
}

func (sm *AlternateStateMachine) IsAccepting() bool {
	// for _, machine := range sm.stateMachines {
	// 	if machine.IsAccepting() {
	// 		return true
	// 	}
	// }

	// return false

	return sm.stateMachines[sm.machineCursor].IsAccepting()
}

func (sm *AlternateStateMachine) Transition(event Event) error {
	// success := false
	// for _, machine := range sm.stateMachines {
	// 	if machine.Transition(event) == nil {
	// 		success = true
	// 	}
	// }

	// if !success {
	// 	return fmt.Errorf("no machting transition found for event: %c", event)
	// }

	// return nil

	sm.events = append(sm.events, event)
	machine := sm.stateMachines[sm.machineCursor]

	err := machine.Transition(event)
	if err == nil {
		return nil
	}

Outer:
	for i := sm.machineCursor + 1; i < len(sm.stateMachines); i++ {
		for _, e := range sm.events {
			if sm.stateMachines[i].Transition(e) != nil {
				continue Outer
			}
		}

		sm.machineCursor = i
		return nil
	}

	return err
}

func (sm *AlternateStateMachine) Reset() {
	for i := sm.machineCursor; i >= 0; i-- {
		sm.stateMachines[i].Reset()
	}

	sm.machineCursor = 0
	sm.events = []Event{}
}

func (sm *AlternateStateMachine) String() string {
	builder := strings.Builder{}
	builder.WriteRune('(')
	for i, machine := range sm.stateMachines {
		if i > 0 {
			builder.WriteString(" | ")
		}
		builder.WriteString(machine.String())
	}
	builder.WriteRune(')')

	return builder.String()
}

const exampleInput = `0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb`

const exampleInput2 = `42: 9 14 | 10 1
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
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba`

const input = `123: 39 86 | 127 32
8: 42 | 42 8
11: 42 31 | 42 11 31
131: 29 32 | 93 86
69: 76 86 | 79 32
39: 32 105 | 86 120
59: 86 117 | 32 112
81: 134 32 | 22 86
17: 86 55 | 32 50
3: 86 60 | 32 87
78: 132 86 | 41 32
133: 86 6 | 32 132
115: 36 86 | 40 32
87: 38 86 | 49 32
129: 32 94 | 86 61
126: 137 86 | 67 32
64: 124 32 | 119 86
127: 86 83 | 32 47
84: 2 86 | 5 32
57: 96 32 | 131 86
16: 28 32 | 6 86
21: 86 91 | 32 41
44: 32 88 | 86 92
99: 86 112
4: 135 32 | 59 86
6: 86 86 | 86 32
60: 32 110 | 86 66
20: 70 32 | 29 86
35: 32 23 | 86 136
28: 32 86
51: 9 86
121: 98 32 | 97 86
47: 41 86 | 117 32
27: 86 34 | 32 52
107: 128 32 | 57 86
95: 9 32 | 6 86
1: 26 86 | 114 32
56: 93 86 | 28 32
24: 32 117 | 86 6
134: 86 70 | 32 6
109: 9 32 | 9 86
91: 86 32 | 32 86
128: 33 32 | 46 86
7: 99 32 | 24 86
31: 53 86 | 102 32
72: 86 120 | 32 133
75: 28 32 | 9 86
108: 124 32 | 130 86
53: 89 86 | 12 32
76: 91 86 | 130 32
12: 101 86 | 111 32
111: 32 1 | 86 85
41: 86 86 | 113 32
136: 91 32 | 70 86
93: 32 86 | 113 32
43: 115 86 | 58 32
2: 86 109 | 32 54
118: 86 13 | 32 134
0: 8 11
49: 86 81 | 32 17
46: 117 32
112: 86 32 | 32 32
114: 86 70 | 32 68
106: 32 100 | 86 131
83: 32 112 | 86 130
89: 123 86 | 107 32
25: 86 119 | 32 70
132: 86 86 | 32 86
74: 86 45 | 32 56
119: 32 86 | 86 113
97: 32 63 | 86 106
10: 86 6 | 32 130
45: 124 32 | 6 86
102: 43 86 | 103 32
61: 117 32 | 124 86
96: 32 117 | 86 68
82: 130 86 | 117 32
117: 86 32
116: 86 41 | 32 93
62: 32 75 | 86 10
29: 113 113
37: 86 28 | 32 91
94: 41 32 | 6 86
18: 86 9 | 32 70
15: 86 37 | 32 21
122: 6 32 | 124 86
40: 104 86 | 61 32
85: 80 32 | 25 86
30: 54 86 | 108 32
52: 122 86 | 116 32
50: 32 91 | 86 119
113: 86 | 32
58: 35 86 | 74 32
67: 86 82 | 32 77
68: 86 86 | 32 113
104: 112 32 | 9 86
90: 32 121 | 86 73
130: 32 86 | 32 32
14: 78 32 | 92 86
73: 126 32 | 27 86
32: "a"
38: 129 86 | 4 32
70: 86 86 | 32 32
19: 32 62 | 86 14
100: 9 32 | 41 86
86: "b"
63: 125 86 | 95 32
135: 28 32 | 29 86
9: 32 32
55: 130 32 | 9 86
77: 6 86 | 70 32
34: 32 20 | 86 16
137: 71 86 | 64 32
124: 86 86
120: 32 93 | 86 124
79: 86 124 | 32 124
125: 86 70 | 32 130
22: 93 32 | 41 86
105: 41 32 | 112 86
80: 32 68 | 86 41
110: 72 32 | 7 86
65: 41 86 | 91 32
103: 32 19 | 86 84
5: 65 32 | 10 86
26: 28 86 | 6 32
33: 113 41
23: 113 130
48: 32 134 | 86 51
54: 86 130 | 32 93
42: 90 32 | 3 86
92: 86 119 | 32 112
98: 15 32 | 30 86
66: 48 32 | 118 86
101: 86 44 | 32 69
71: 124 32 | 29 86
36: 18 86 | 65 32
13: 6 32 | 93 86
88: 132 32 | 117 86

aaabbaabbaaababaaaaabbbb
babaaaabaabbbababbbabaaa
abbaababababababbbbbabaaaaaabbbabbbbabaababbaabbbaaaabbb
ababaabbabaabbabababbbabbaaababaababaaaabbbbbaabbbababbbbabaabaabaaabaabbabbaaabbbbbabbbaaabbaba
babbbabaabaaabababbbabbaabaaababbaaababb
bbaababbabbbbabababaaababbaabbbaabbaaaabbbaaabbaabbbaaabbbabaaaa
abbaabaabaaabababaaababb
bbbabbaabbaababbabaaabaa
bbaabaababbbaababbbbaaab
babbabbbbbbbbbbaabbaabaaaaabbaabbbbabbbbaabbbbaaabababbbaaababbbbbaaabbb
bbbbaabbbaababbbbbbbabbbbbbabaabaabaabaa
baaaababaababbbbabbbabab
babbbabaababababaabbabaaaabbabaaabababbb
bbabbbabaabaabbabbbbabaabaababbbaabaabbbaabbbaababbbabbb
abaaaabbbaaabbbbbaababaaabbaaaabbaabbbaaababaabaabaaaaabbaaaabbaababaaaabbbabbaaabbabbbbaabbbbaa
abbbbbbbbbaabbbbabbbabbabbababab
bbaabbbaaaaabaabbabababb
abbbabbaaabababbbbabaaabbaabbbbb
babbbbabbabbaaaaaabbaaaabaaabbaa
abbababbaabbabbbbbaabaaababaaabaabbbababaabbbaaa
abbbbbabaababaabaababaabababaabbbbabaaaa
babaabbbaabababbbbaabaaaabbbbaaabbabbaabaaaababbaaaabaaaaaaaaaabbaabaaaaabbbabaababbabba
abababbaabbaabaaababbbbb
aaaabaabaaaaaaababaababb
bbbaabaaaabababbbabaaababbabaabbbabaabbaaabbbbabbbbaaaba
abbbabbaabbbaabaaaabaaab
babaaababbbbaaaabbbabbbabaabbaaaababbbab
bbaabbbbaabbbbbbaaababaabbbaabba
ababbbaaaababaabbaaaabbb
aabaaaabaabbabaababaabaaaababbbabbababbbabbaaaab
abbbbaabaaaababbbbbbaaaa
aabbaaaaabbababbaabbabaaababaaba
bbbababababaabababaaaaab
bbaabaaabbbbabbbababbbaabbabbbbbaabbbaab
aabababaabbababbaabbbaaa
abbaababbabbbbabbbbabbab
baaabbababbbbbabaabbbabb
aabaabbbbaabaababaababaa
bbaabaaabbbbbbaaabababaa
bbaabbabaaaaababbbbbabab
aabbbbbaababbbbabbbbbbaabbbaabaaaaabbbbb
abbabbababaabbabbaaabbba
babaaabbabbaaaabbaabaabb
babbbbaaabbbabbaaaabbabb
aaaaabbaaabbaaaabbbaaaaaaaababbb
aaababaababbbabaabaaabbb
abbababbbbababbabbbbbbbb
abbabbabbaaabbabbbaabbaa
aaaaaabababbbbbbbaabaabaaaaaaabaabaababa
babaabbabbbaaaaabbbbbbaaaabbabaaaaabbbaababbbaab
aaabababbaaaaaaaababbbbb
babaabbaaaaaaabaabaabaab
baabbaabbaababbbaaaaaabb
bbababaaabbbaabbabaabbaabbbbaabbbbbaabbaaaaabbabbaabbbbb
babaabaaaabbbbaabaabbaaaaaabbbba
abababababbabbabbbaababbbbabbbaababbbaab
bababbbbbbababbaabbbbbba
abbbaababbbaabaaabbaaaaa
abbaabbabaabbbabbabbbbbbabbbabaabaababba
aaaabbaabbaaabbaaaabaabbaaaabbaabababbba
aabaabbaaaabaaaaaabbbaab
bbaabbabaaabbbabbabaabaa
bbaabbbaaaaabaabbbabbabaabbbbaabbaaabbaa
aabaaabbaaabbaabaaabbaabbbaababbbbababbaaaaaabaa
aabbabaaaaaaaabababaabbb
bbbbbaabbabbabaaabbbabbbbabaaaaaabbabbbbbbaaabbbbbabaaaa
babbbababbabbaaaaababbbbabbbabbb
babbbbbabababaaabbbababaaaaababbbbaabbaababbaaab
aabbaaaaabbaabaababababbbabaaabbaabaabaaaabaababbababbaabbbabaab
bbaaaabaaabaaaabababaaba
bbbbabbbaaaaabbabaabaabb
bbbbaabbbbbbbbbaabaabbbb
aaaababbbabaaaabbabbbabaaaaabbbabaabbaaa
abbabababbbbbabbbaaaaaaabaaabbabbbbbaaaa
aaaabaabaaaabaabbaaaaababaabaaab
ababaabbbbabaabbbbbaabaabbbbbaaaababbbab
abaaababbbaabaabaaabbbaabbbababaaababbbabaababab
bbbbabbabbbbabaababbbabaabaababb
aaabbaabaabaaaabbbbbaaba
babbbbabbaabbaabbaaababb
abbaabaabaaababaabbbbbba
baaababababbbabababbabbbbaababba
babbabaabaaaaabaabaabaab
aaabaaaaaaabbbababbababbbbababaabbaabaaaaabbaaab
bbbbabbbabbaababbbaaabbabbabbbbbaaababababbbababaaabbabbababbbbb
aababababbbbabbaabbaabbabbbbbbaabbabaaabaaaaaaababbaaaaa
abaabaaabaaaaababaabbbaabbabaaaa
baaabbabbabbabaaaaaaaaab
baabbbabbaabbaabaabaaaabaabbaaaabababbab
abbabbbaaabbbbbbaaabaaab
abbbbbabaabbabbbbaaaababaababbbbaabababaaababaabaabbbbaabaaaabba
ababbbbabaaaaabaabbaaabb
abbbabbaabaaabbaabbaaaaa
aabaababbbbbbbbabaababaa
bbbaababbbaababbbbbababababaababababbbabbabaabbbabbbbbba
babbbbabaabbbababbabbabb
aababbaaaaabbaaaaabbabaabbabaaaabaabbaaa
bbbabbaaaaaaabaabbabbaab
babbbbbaaabbbbbaababbbbb
bbaaaababbbbabbabaaaaabb
abbaabbbbaabbbabbabababa
baabbbaabbbabbaabbbaababbabbabbbbabaababaaabaaabaaaababaabaabaab
abbbabbabbbaabbbbaababaa
aababaabaaaababbbbbaababababbaabbbbbaaba
aabbaaaabaaaaaaabaababbbbabbbbabaaabbaba
bbaaaabaaaaaabbaabbabbbabababbba
baabaababbaaaababaaaaaab
bbabbbbbbbaabbbabbababbb
bbaabbbaabbbbbaaaaabaabbbbbbbaaabbaaabbbbbaabbab
bbbaabbbbbbbbbaaabaaaabb
bbaabbbbaababbaababbabaabbbbbbabbabbbaab
baaaaaaabbaaabbaabaaaaaa
abbaabababbabaaabbbabaab
babbbabaaabaaabbbbbaababaaababaaaaabaaaababbaabaababaaba
aaabababaaaaabbabbaababa
aabbabaabbabbbbbbaabaabaababbabb
aaabbaaababbaabbbbbbbaab
aabaabbababbbbbaabbbabbb
bbbbabbaabaaaabaababababaabbbbbbbbbbbbbaabbbabbb
abbababbbbbaaabbbbbbaabbbbaaaabaabaaaababbaabbaa
bbbaaaaababbabbaaaabaaaaabaabaaabaabaabbbbaaabaababaabaa
ababbbbaabbaaaabbbbbabab
babbaabababbbababaababaa
aabbbababbabaaabbaaaabaa
bbabbbabbbbbbbaabbababaaaabbbabb
baabbbaabababaaaababbbbaabaaaaaa
baabaabaabbaabaaaabbbbaa
abbaaaabbbaaaabaabaaababbabbabbaabbbabbabaababbaaaaabbbbbbbaaaba
bbaaabbabbaababbbaababbbbbaaabbababbaaab
abbbbababbbabbaaabbbbbaa
bbaaabbaababaaaababbaaaaaaabbaabaabaabbbbbabaaababababbbbaaaabaa
aabbaabbbaabbbaabbaabbbaabbabaababababaa
bbabbbbbbbbabbaabbbbbbabaaaaaaabababbbabbbbabaab
baababbbaaaabbbbaabaabaaaabaaaaa
baaaaabaaaabbaabbabbbaaabbabbaaabbabaaaa
aaababaaaabaababaabbaaba
aabbabbbabbbabbabbabbaaababaaaababbbaaaabaaaabaa
baabbaabbbabbabaabbbbbabaabbaabbbbaabababaaabbbbbbaababa
bbabbababababaaaabbabaaabbabaaaa
abababbababbaababaaaabba
babbaabaaabbbbbbaaababba
bbaabaabbbbaabbaababaaabbabbaababbbbbbbbababaabb
bbbababaabbbbababbaaabbaabbbaabbaaabbabb
abbaabbabaabbbaaabaabaaababbbaaabaabbabb
aababbababaaaaaaaabbbbaa
bbbbaaaabbaaabbbabbaaabbababaabaaaabbabbabaaabbaaaabaaaa
babbabbbbbbabbaaaaabaabbbaaabaaaaababbab
aaaaaaaaabbabababaaaababaaabaaabbaaabbaa
bbbbabbaaabbaaaababbbabaabaaaaaaabaabaab
babbbbbbaaaaaababbabaaaa
bbbbbbbaabbbbbbbabbabaabbaaabbababbbbaaaabaabaabaababbba
abbbaabaabbbbbbbbabaabbb
aabbbbbbaabababbbaaaaabb
babaaaababbbbbbbaabbaaaaabbaababbababbab
babbaabaaaabbbaaaabababbabbaaabababaabababbbbbbaaabaaabaaaabbbba
aaabbaabbabbabbabababaaaaababaabbbbaaaabbbaaaaaa
babbabaaaaaabbbaaaaababa
baabbbaaaaabbaabbbabbabb
aaababaaabbbbbabababaaba
baaababaabbabbbaabbbaabaabaabaab
abbbaabbaaaaabbbaaabbbba
aaaababbbaaaabababbbbbabbbaaaaabbbabbabb
abbaababbaabbbabbbbbaaba
bbbbabbaaababbaabbbaaaab
abbaabbabbabbbababbabbbb
abaabaababaaaabbaaabbabbbbbbaabaaaaaaabb
bbbbabaababaababbbaabaaaabaaabaa
babbabbaabbaababbabaabbaaabababbaaaabbab
babaaabaabbabbbaabaaabbabaababaa
baabbbababbbbbbbbabbbbbaaaaabaabbbaababbaabbabaabaababab
baabaababaaabbababbaabab
bbabbbbbaaaabbbaabbaabaabbaabaabbbbbbbab
aaaaabbaaababaaaabbbabab
aabababbbbaabbbabbaabaabbaabbabbaabbbaab
babbaabaaaaaaababbbbbbbb
aabbabbbaaabababbbabbbbbbbaabbaa
aaabbbaaabbababbabbaaabaaabbaabbbabaabaabbbaabbaaaababbb
abbaabbabbbaabaababaabaa
babaaaabababbaabbababbba
bbaababbaaabbaabbbabaaba
bbbbbaabaababbaabaaaaababbbbabaabbabaabaabbbbbbabaabbbbbaaabaaab
aababababbaabaababababababbbabaabaaaabaa
abaabbaaaaaabaabbaaabbabaaabbaabbbaaaaaa
aabaabbaaabbaabbabaabbba
bbaabaaaabbaababbaabaaab
babbaaaabaababbbabaaabaa
bbabaaababbbbaabaabbaaaabbabbaab
babbaababbbbaabbabaaaabaabbabaaaaaabbaaabbbaabbaaabbbaababaababa
baababbbabaabbaaabaaaabb
baaaaaaaabababababaaabbb
baabbbababbaabbbaababaabbbaababbaaabbbba
baabaabaabaaaabaabbabbbabababbab
bababaabaabaaabbababbaba
bbbbabbbaaaabbaaabbbbaaa
babaaabbbabaabbaaaabababbbbbaabbaaabaaab
aababaaabbbbabbaaababaabbbbaabbaabbbabaa
baaabababaaaaababbbaababaabbabbabaaaaabb
aabbabbbbaaaaaaabbbbbbbb
aababbaaabbbbabababbaabbabaabaaaaabbabab
ababbbaaaabbabbbabbabaaaabababbb
abbbaabbabbaabbaabbababbbabbabbaaaaaabbaabaababb
bbbabababbabaaabaaaabbbaabaaaaaa
bbaabaabbabbaabaabaaabbababbbaab
aaabbaabaaabaabbbbbabbba
bbababaabaabbbabbabbabab
bbbbbabbbbbaaaaabbaabaabbababaabaaaababbaabaabbaabaabbbaaaaababaaabbbaababbbbaaaaabaaaaa
aaaabaabbabbbaaaaaaaaaab
bababaaaaabaaabbaabaabbbbbaaaabaaababaaaaaaaaabbaabbbaaa
bbbbbbbaabbaababbababbbb
baabaaaabbbabaaaababaaaabababababbababba
abaabbaababbaabbabbbbabababbaaaaaabaabbbbaabaaaa
bbbaabbbaaabaabbaaabbaababbbaababaaabaab
aabbbababaabbbababbaaabb
aaabbaaaababbbaaaaaaaabb
abbaabbabbbbabbabbaaaaab
babbbaaaaaaaabababbbbbbbbabbaabababaabbaabaabbaaaaaaabaabaabbabb
ababbbbabbbbaabbbababaaabbbbaabababbbaab
babbabbaaaaabbaaababbabb
babbabbbaababaaabbbbbaba
aaabbaababbbbabababbbbbbaaaaaabbabaaabbbbabaaaaa
bbbabbbbabbbaabbaaaabaab
bbaababbbbaababbaabaabbbbbbbbbbb
aabbabaaabbbaabbbbbbabbbbbbbbbbb
abbaabbbababbbaaabbabaaabbbaabbaaabbbaab
bbabbababbaabaabbbbaaabb
bbbbbbbaababaabbbbaababbbaabbbabbaaaaabb
bbbbaabbabbabaaabbbbbaba
abbbabbbbaabaaabbbbbbabbaaabaaaababbabbabbabbaaabbabbaabaabaaaaa
babaabbabbbaabbbbabbabaabbbbaaba
aaaaabbababaabbabaaabaab
babaababaaaababbabbbaaab
baabbbaaaabaaaababbababbabbaabbbaaaabaaa
bbbbbabbbaabaaabababbababbabbabb
babaaaabaaaaabbbaaababaaaaaaabbbbaabbaaa
abbaabbbbabbaabbababbbaababbbaab
aaaabbbaababababaabaaabbbabaaababbaababbabbbbaba
babbabaaaaaabbbaabbaaabb
bbbaababababbaabbaabbabb
bbaababbbbaaabbbbbbbbbbbbbaabbabbababaaaaabbbbaaaaababba
bbababbaaabbaabbbabbabaababbbaabbbaaabbb
aabbbababbababbabbaabbaa
babbbbababbbabababababbbaabbabbaabbbbabbbbbabbba
babbabaaabbbbbabbbbbabbbaabbbbbabbaabaaabbaaaaaababbababbabaaaaa
baabbaababbaaababbabaaaa
ababababbabbbbaaaaabbabb
bbbbbbbaababbbaabaaaabbb
abbaabaabbaabaabbbbbaaaa
ababbbaabababaabbaabbabb
aaaaabbaabbababaaaabaaaabbbbabbbabbaabaabaabbabb
aaaaabbabbaaaabaaabbabab
aaaabaabbbbbaaababbbababbbaaabaaaabbabab
bababaabaabbbbbbbbaabbbaabbbbbabbbbaaaba
baababaaaabaabbbbabaabbbbabaaaabbbbaaabbbbbbabba
aaaaabbabbbbabaaaaabbaabaababbbbaabbbbaabbbbaaba
babbabbbbbababbaabaabaab
bbbbabbabbaabaaabbbaababbaaaaabbbbbbaaab
abaaababbababaabaaabbaabbabbaababaaaabbb
babaabbabababaabaabbbaaa
baaababaabbabaaababbabbbbbbabbba
abbabbbabbaabbbaabaabaaabbaaaabb
bbbbaabbaababbaaabababbb
aabbbbbbbbbabbbbaabaaaba
babbaabaabbabbabbabaabbb
baabbbabaaabbaaabbaabbbbbaaaabba
babbabbaabbbaabbbabaababbaabaababaababab
aababbbbaababaaaababbbbb
aababaabaaabbbabaaaabaaa
abbbaababbbabababaabbaba
abaabbaaaababaaabaaaaaaa
ababaabbbbaabbbaaabbbbbbababaabbabbaaabbaabaabaabbbbbaaa
bbbbabbaabaabaaaaabbaaab
baababbbaabbbbbabbabaaaa
babbbbaaaaaaababaabbbababbbbbbaababababaabbbaaaaaaaabaaa
aaaabaabbabbbaaabbbbbaba
bbbaabbbabbabaaaaabababaababbaabaaaaababaaababbaaaabaabaaaaabaaaabaaaabb
baaaaaaabaaabbabaababbba
abbaaaababaaababbaabaaaa
babbbababbabbabaaabbabbbabbaabababbaaabaabbbbabb
bbabaaabbbaabbbabaabaababbaaaaaa
aabbabbbabbaabbabbabaaababababbb
aaaabaabbabaaaabbaaaaaaaabbaaaaaaabbbabb
aaaaabbaababaabbbaaaaaaabbabbabaabaaaaaabbaaabaabbbbaaba
aaaabaabbbbaabbbababbbbaaaabaabbbabbbaaaaaabaabaabbabaab
baabbaabbabbbbbbabbabbbb
bbaabbbabaaaaaaaaaaababa
babaaabbbaabbbababbabbbabbbbabab
bbbaababbbabbbaabbaaaaab
bbaabbababaabaaaaaaababbbaababab
aabbbababbaaaabaabaaabbaababbbab
abbbbaabaaaabbbabbbababaaaaabbaabaabbaaaabaaabaa
aabbbbabbaababbaabbbabab
aabbbabaabbbaabaabbaaaaa
babbaabbabaabaaababaabbabbbaaaaaababaabbbabbabab
abbbbbabbbabbaaabbaabaaabbabbbaabaaaabaaababbbabababbbbbbaaaaabb
bbaabbbabbbabbaaaabbbbaa
abbaaabaaabbbbbabbaaaaaa
baaaaaaaaaaaababbaaababb
aaabbbbbbaababaabbbbbaaa
aaaaaaaabbabbbbbabbbbbbbbbbbaabbbabbabaaababbabbbbababbbaabbbbaa
bbaabaaabbbabbaaaaabbaaabaababbbabbaabaababababa
bbabbbabbbababaababbbaaaaabbbbab
bbbabbbabbbabababbbbbabbbbabaaaaabbaaaabbbbbabababbbaabbbabaaaaaabbabbabbababaaa
bbabaabbaaabaabbaaababaabaaabbaa
bbabaaabbbabbbbabaaabaab
bbbbbbbaabbababbaaaaabaa
bbaaabbaaaaabbaabababbba
aababaaaaabaaabbbabbaabaaaabbaaaabbbaabaabbbbbaabbbabaaa
bbabaabbaaaaaababbbbbaaa
abaabbaaabbbabbabbbbabab
aaababaaaabaababaababbba
abababababababababbaabaabbaabaabbbbbbbbbbbabaaaa
bbababbababaaabbaaaaabbaabaabbba
aabaabbaaababbbbaaabbabb
aaababaaaaabbbaabbbabbbbbbaabbbaaabbbaab
baaaaababbabbababaabbbaababbaabbbbabaabbaababaabbbbabaababaaaaab
aabaabbbaaaabbbababaaabbababaabaaaababba
bbabaabbbabbbbbabbaabaaabaabbbbaaabaaaaa
ababbbaabbabbbaaaaabbbaababaabaa
bbbbaabbaaaaaaaabbaabbbbaaabaababaaaabbb
bbaabbaaaaabbabbbaabbbbababaabbbababaaabbbaaabaababbbaaa
abbbbbbbabbbbabaabaaaaaa
bbbbabbbbabbaabbbabaabaa
abbbaabbbabaabbabbbaabaabbbbabaabbababbaaababbbbbbbaabbaaabbbbab
aabaababaabaaabbbbababaabbabaaabaaaaaaabbabbbabb
aaabbbabbbbbabbbaaabbbba
aabaababbbaaabbababbabbbaabbabba
bbabbaaaaababbbbbabaabbb
ababaaabbbaabaabbbabaaaa
abbaabababbaababbabaabaa
bbaabaaaaabbaabbbbabbbab
abbbbbabbbabbbaaaabaaabbabbaababbababbba
ababaaabaababaabbbbababb
abaabbaaaaabbaabbababbbb
aaabbbaabbbabbaababaaaaa
aabababbaababababaabaabaababbbbb
baabbaabaaaaabbaaabaaaabaaaaaabaabbbbabb
baaababaabbabaaabbbabbab
abbabbabababaaababbbabaa
abbbaabaababaaabbbaabaaaababbaabbabbabbabbaaabaa
baababbbbbabbababbbabbbbbbbbabbbabbaababbbaababa
babaabababbbabbaaaabaaab
aabaabbaaaabaabbbbbbaaaa
ababaaabbaaaaaaababbabab
bbaabbabbaabbbabbbbbbbbb
bbbbbabbbabbbbabbbababbb
aaaabaabbbabbaaabbaaaaab
bbababbabaaaababbabbbbbabbbabbaaabababbbaabbbbab
ababaaabaabbabbbaaaaabbbbabbaaab
abbbbabababaaaabbabbaabaaaabbbabbbaaaababababbaaababbbbbbbaaaabb
bbbbabbbbbaabbbbaabaabaa
bbababaaaababbbbbbbaabbbaabbaababaaabbaa
bbaabaabababbbaaababbaaa
abbbbbababaaaabaaaababababbbabbabaabbaabaabaabbbbaaaabba
ababababaababbbbaabaaaba
aaaababbbbbabbbbaabbbbbaabaababa
aaaabbbaaababbbbbaababab
bbaabbbbabbbbbbbaaaaaaaaabbaababbbaaaababbbbbaab
bbaabaabaabbaabbabaababa
abbaabaaaaaababbbbabaaaa
aaaaaaaababbbbbbbbbbbabbaababbaabaaaabaabaaaaaabaabbabab
abaaaaabaabbbbabbaaaaaabaababbaabbbababbbbababba
aaabababbaaabababaabbbaaaabbaaab
baaaababbbaabbabbbbbbbbabababaaaaaaabaaa
bbbabbbbaaaaaababaabbabb
aabaaaabaababbbbabaaaaaa
aaabbbaaaabbbabbaaaaaabbbbabbabb
babbabbabbaabbabaabbbbbbaaaabaaa
aabbbbbbaabaaabbaabaababbababbbbbaaaaaab
aaaaabbbaaababaabbaabaabaaaabaabbbbaaabb
bbbabbaabaababbbbaaabbbb
babaaabaaababaaabaaaaabaaabbbaaa
babbbbaabaaaaabaaaababaaabbbbaaa
ababaaaabaaaaabaaaabbbababbaaaaababaabaa
bbaabbabbbbbbabbaaaabbaabbbaaaba
aaaaabbaabaaaabaabbabbbbababbbababababaaaabaaaba
abaabbaaabaaababaaababba
abbabaaaababaabbbbaabbbbabbabbbaabbbbbaaaabbabaabababababbbababa
baabbbabaaabbbabbbabbbbbbaabbbbabaaaaabb
baaabbbababbbbaaaaabababbabaabaaaaabaaabbaaaabababbaabab
bbbababaabbbaabbbbbabbab
bbabaabbabaabaaaaaaaaaab
baaaaaaaabababbabbbabbba
ababaaaabaabbbabbbbbaabbababaaabababbabb
aaabbaaaaababaaabbaabbbbbbababbb
abbabaaaabbbbbababbbbbbbaabaabbbbbbaaaaaaaaabbbbabbabbaaaaabbbbaabaaaaaa
babaaabaaabbabbbaabbbbab
babbabbabbaabbbaabaaabbaaabbbaaa
aabbaaaaabbababbbabaababababbbbaabbabbaa
abbababaaaaaaaaabbbaabbbbbababababbabbaa
aabaaaabbbaaaabaababbbab
abbbaabbabbabaaaabbabbbababbaaaabaababbbbbbbaaabbbbababbbbbbbbbbbbabbabb
aaaabbaabbbababaaabaaabbbabaabbabbbababb
baaaaaaababbabbababbabab
bbbabbaabbaabbbbbbabaabaaaabbabbaabbabba
bbababbababbbaaabababbaa
aabaabbabbabbabaaabbbbaa
abbaaababbbbabbbaaabbbbb
aabababbaaaaaababaaaabbb
bbababaabbbababaaaaaababababaabbabbaaabb
ababaaaaaababaababaaaaab
abaabaaaaaabbbaaabaaabbb
aabbbabaaababbbbabbbaababaaababb
bbbababaabaaabbabbaaabaa
babbabbbabbabbabaaabaaab
baaaaaaaababbbbabbbbaaab
aababaabbbabbbabbaaabaab
abbababbbbbbbbbaaabbbbaa
bbbabbaaabaaaabaabaaabaa
ababababaabbbbbabbbabbba
aabababbabbababaabbabaaabbaabbbbbbaababa
babbabbabababaabbbabbbaababbbbabbbaabbaabaabaabb
aababaaaaabaabbaaaaaaabb
babaababbaaaaabababbabab
bbbaabaabbbbbabbaabbabaabaabbaabbaaaabaa
aaaaaabababaabbabbabbaaabbbabaaabbababab
ababbbbabaaabbabbabbbbabaaabababbabbbabaaabbaaabababbabbabababbb
baaababaabbaabaaabababaa
abbbbbbbaaaaaaaaaabaaabbbababababbbabbab
bbbaaaaabaaababaaaabaaba
bbababbababbaabbbbabbbaaaabaabbaababababbaabaabaaaabbbba
bbabbbbaabbbbaabababbbaabbabaaaaababaaba
bbbaabaaaababaabbbabbbababbabbbabaabbabaabbabbaa
abbbaabababbaaaaaababbaabbababaabaabaabababababaaabbbabbbbbbaaabbbaaaabb
abbbaabaaabbaabbababbbbaaabbbabb
aaabbbaababbababbabaabaaaaabaaababbabbbabbbbbabb
bbabbbababbabbbaaabaaaaa
ababababbbaabbbaaaabaaab
abbaabaabbabbbababbbbababbbbaaaa
aaabbababbaabbaaabbabbabaabbabaababbaababbbabaaaabbabaaaabbabbabbbabbbaa
bbaaabbaabbabbbababaaabaabababbaaaabbbaabbaabbbbbbbaaabbbaabbaba
aaabaaaababbabbbabbbabbabbbaabaaaabbbaab
aaaaabbbabbabbbabbbabbba
abababbabaaaababbabbabab
bbbbaabbaaabbbababbbbabb
abaabaaabbaabaababaabbaaabbabababaaaaababbabbbabaaabbbba
bbaaabbaaaabbaababaaabaa
babbbbbabbbbbbaaabaabaaaabbabaab
bbbbbaaaaaababbbaaabaabaabbbbabbbbbbaabbabbbbabbbaabaabb
bbaabbbbbabbbbbabbaaabbb
aababaaaabbaababbbbbbbab
bbaaaabaaabbbabaaabbabab
abbbaabbabbabbabbababbab
babbabbaababaaabbabababb
aababbbbaabbaabbabbbbababbbbabaaabbbabaaaabbbabbaabbaaba
aaabbbaaaaabaabbbabaaabaaabbbaabbaabbabb
aaaaabbbaaabaaaabbbbaabbabbbbbaaabaababa
aabbabbbabaaabbababaabbb
bababaabbabaaabbaaaaabaa
aababaaababbabbababaabbaabbbbbbaaabbbbab
aaaabbbabaabbaababbaaaaa
bbaabaabaabbabbaaaaaaabaabaaaabbbbaabaabbabbaabbabbbabaa
aabbbabaabbbaabababbbaab
bbabbbabaaababaaaabbaabbbabbbbabbabbbabbabaababb
bbbbaabbabaabaaabbbbaaba
bbbbabbababaabbaabbaaababbbbaaaa
aaaaaaaaababbbaaaaaaaabb
abbaababbaabbaababbaaaabbabbbaabbbaaaaab
ababababbbabaababbaaaabbbbababbbbbabbbbbaababbaabababababbbbaabbabbbbbbbbabbbbab
aaabaababbabaabbaaababbabbaaababbbabaabbbbaaabaabbababbabbbbabbbbababbbb
aaabbbabbbabbbbbaabaaaba
babbaabbababaaaababbbaaaabbbbabababaabbbaabbababbaaabaabaabbbabbbaaaaabb
abbaabababbaabbbbbbabaab
babaaabbabaaabbabbaaabbb
babbaabbbbabaaabbbbababb
abbaabbaaaabababaababaabbaabbbababaaaaaa
bababaaabbaabaabbabbabbbababababbaabbaaa
aaabbaaaaabbaaaaabbbaababababbbababbabab
bbaabaabbbbabbbbaababbaabbbbaaba
aabbabaaaaabababaabbaaba
abbaaaabbaabbbabababaaabaabbaabbbabbaaababababbb
bbbbbabbbbabbbbbbbbaabbbbababaaabbbababb
abaabbabbaabbbaaabaaabaa
bbaaaabaababbbaabaabbbba
bbbbbbaababbabbaabbbbbababaaabaa
abbbbaabaababaaabbbabbaaaaaabaabbaabbabb
bbbbabbbaabbaaaaabbbbbaa
abbaaaababaabaaaabbabaab
aabaabbaaaabbaaabbbababb
babbbaaaaaabaabbbbabaaba
abbaabaabbbaababbbbaaaba
bbabbbababaabbaabbabbbabaaaabbbabbaaabaabaabbabbbababbba
bbbbbbbaabbbbbbbabbabaab
babbaaaaaaaabaabbabaabbaabaabbabbabbbbbabbbbaabaabaaabbbabbbaaab
bbababaabbababaaabaaaaaa
bbabbababbababbaaabbaaab
aabbbbbabbbabbaabababbbb
abababbababbaabbbabaaaabbbbababbaabbbbaa
bbabaaabbabaabbabbaabbbaaabaababaabbabbbaababbba
aaabbaaabbaabbabaabbabba
aabbbabbabbbabbbabbbbbaa
aabaabbabbababaaaaaaababbaabababbababbaa
aabaabbbabbaaaabaabbabaabaaaabba`
