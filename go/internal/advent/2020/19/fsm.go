package main

import (
	"fmt"
)

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
}

type Appender interface {
	Append(FSM)
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

func (sm *ConcatStateMachine) Append(fsm FSM) {
	if len(sm.stateMachines) == 0 {
		sm.stateMachines = append(sm.stateMachines, fsm)
		return
	}

	machine := sm.stateMachines[len(sm.stateMachines)-1]
	switch machine.(type) {
	case Appender:
		machine.(Appender).Append(fsm)
	default:
		sm.stateMachines = append(sm.stateMachines, fsm)
	}
}

type AlternateStateMachine struct {
	stateMachines []FSM
}

func NewAlternateStateMachine(stateMachines ...FSM) *AlternateStateMachine {
	return &AlternateStateMachine{
		stateMachines: stateMachines,
	}
}

func (sm *AlternateStateMachine) IsAccepting() bool {
	for _, machine := range sm.stateMachines {
		if machine.IsAccepting() {
			return true
		}
	}

	return false

	// 	if sm.stateMachines[sm.machineCursor].IsAccepting() {
	// 		return true
	// 	}

	// Outer:
	// 	for i := sm.machineCursor + 1; i < len(sm.stateMachines); i++ {
	// 		machine := sm.stateMachines[i]
	// 		for _, e := range sm.events {
	// 			if err := machine.Transition(e); err != nil {
	// 				continue Outer
	// 			}
	// 		}

	// 		sm.machineCursor = i
	// 		return machine.IsAccepting()
	// 	}

	// return false
}

func (sm *AlternateStateMachine) Transition(event Event) error {
	success := false
	for _, machine := range sm.stateMachines {
		if machine.Transition(event) == nil {
			success = true
		}
	}

	if !success {
		return fmt.Errorf("no machting transition found for event: %c", event)
	}

	return nil

	// 	sm.events = append(sm.events, event)
	// 	machine := sm.stateMachines[sm.machineCursor]

	// 	err := machine.Transition(event)
	// 	if err == nil {
	// 		return nil
	// 	}

	// Outer:
	// 	for i := sm.machineCursor + 1; i < len(sm.stateMachines); i++ {
	// 		for _, e := range sm.events {
	// 			machine = sm.stateMachines[i]
	// 			if err := machine.Transition(e); err != nil {
	// 				continue Outer
	// 			}
	// 		}

	// 		sm.machineCursor = i
	// 		return nil
	// 	}

	// 	return err
}

func (sm *AlternateStateMachine) Reset() {
	for i := range sm.stateMachines {
		sm.stateMachines[i].Reset()
	}
}

func (sm *AlternateStateMachine) Append(fsm FSM) {
	for _, machine := range sm.stateMachines {
		switch machine.(type) {
		case Appender:
			machine.(Appender).Append(fsm)
		default:
			machine = NewConcatStateMachine(machine, fsm)
		}
	}
}
