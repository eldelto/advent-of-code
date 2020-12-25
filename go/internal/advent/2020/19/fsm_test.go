package main

import (
	"testing"
)

func TestRuneMachine(t *testing.T) {
	tests := []struct {
		name          string
		fsm           *StateMachine
		events        []Event
		wantAccepting bool
		wantErr       bool
	}{
		{"empty", NewRuneMachine('a'), []Event{}, false, false},
		{"matching", NewRuneMachine('a'), []Event{Event('a')}, true, false},
		{"non-matching", NewRuneMachine('a'), []Event{Event('b')}, false, true},
		{"too long", NewRuneMachine('a'), []Event{Event('a'), Event('a')}, false, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var err error
			for _, e := range tt.events {
				err = tt.fsm.Transition(e)
			}
			if gotAccepting := tt.fsm.IsAccepting(); gotAccepting != tt.wantAccepting {
				t.Errorf("fsm.IsAccepting() = %v, want %v", gotAccepting, tt.wantAccepting)
			}
			gotErr := err != nil
			if gotErr != tt.wantErr {
				t.Errorf("err = %v, want %v", gotErr, tt.wantAccepting)
			}
		})
	}
}

func TestConcatStateMachine(t *testing.T) {
	tests := []struct {
		name          string
		fsm           *ConcatStateMachine
		events        []Event
		wantAccepting bool
		wantErr       bool
	}{
		{"empty", NewConcatStateMachine(NewRuneMachine('a')), []Event{}, false, false},
		{"matching", NewConcatStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{Event('a'), Event('b')}, true, false},
		{"non-matching", NewConcatStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{Event('a'), Event('a')}, false, true},
		{"too long", NewConcatStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{Event('a'), Event('b'), Event('b')}, false, true},
		{"nested matching", NewConcatStateMachine(NewConcatStateMachine(NewRuneMachine('a')), NewRuneMachine('b')), []Event{Event('a'), Event('b')}, true, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var err error
			for _, e := range tt.events {
				err = tt.fsm.Transition(e)
			}
			if gotAccepting := tt.fsm.IsAccepting(); gotAccepting != tt.wantAccepting {
				t.Errorf("fsm.IsAccepting() = %v, want %v", gotAccepting, tt.wantAccepting)
			}
			gotErr := err != nil
			if gotErr != tt.wantErr {
				t.Errorf("err = %v, want %v", gotErr, tt.wantAccepting)
			}
		})
	}
}

func TestAlternateStateMachine(t *testing.T) {
	tests := []struct {
		name          string
		fsm           *AlternateStateMachine
		events        []Event
		wantAccepting bool
		wantErr       bool
	}{
		{"empty", NewAlternateStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{}, false, false},
		{"matching", NewAlternateStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{Event('a')}, true, false},
		{"matching", NewAlternateStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{Event('b')}, true, false},
		{"non-matching", NewAlternateStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{Event('x'), Event('a')}, false, true},
		{"too long", NewAlternateStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), []Event{Event('a'), Event('b'), Event('b')}, false, true},
		{"nested matching", NewAlternateStateMachine(NewConcatStateMachine(NewRuneMachine('a'), NewRuneMachine('b')), NewRuneMachine('a')), []Event{Event('a')}, true, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var err error
			for _, e := range tt.events {
				err = tt.fsm.Transition(e)
			}
			if gotAccepting := tt.fsm.IsAccepting(); gotAccepting != tt.wantAccepting {
				t.Errorf("fsm.IsAccepting() = %v, want %v", gotAccepting, tt.wantAccepting)
			}
			gotErr := err != nil
			if gotErr != tt.wantErr {
				t.Errorf("err = %v, want %v", gotErr, tt.wantAccepting)
			}
		})
	}
}
