package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

func main() {

	// Example
	exampleOps := parseInput(exampleInput)
	exampleFerry := NewFerry()
	runOperations(exampleFerry, exampleOps)
	fmt.Printf("Example result: %d\n", exampleFerry.ManhattenDistance())

	// Scenario 1
	ops := parseInput(input)
	ferry := NewFerry()
	runOperations(ferry, ops)
	fmt.Printf("Scenario 1 result: %d\n", ferry.ManhattenDistance())

	// Example 2
	exampleFerry2 := NewFerry()
	runWaypointOperations(exampleFerry2, exampleOps)
	fmt.Printf("Example 2 result: %d\n", exampleFerry2.ManhattenDistance())

	// Scenario 2
	ferry2 := NewFerry()
	runWaypointOperations(ferry2, ops)
	fmt.Printf("Scenario 2 result: %d\n", ferry2.ManhattenDistance())
}

func runOperations(ferry *Ferry, operations []Operation) {
	for _, op := range operations {
		switch op.Cmd {
		case NorthCmd:
			ferry.Move(North, op.Arg)
		case EastCmd:
			ferry.Move(East, op.Arg)
		case SouthCmd:
			ferry.Move(South, op.Arg)
		case WestCmd:
			ferry.Move(West, op.Arg)
		case RightCmd:
			ferry.Rotate(op.Arg)
		case LeftCmd:
			ferry.Rotate(-op.Arg)
		case ForwardCmd:
			ferry.Forward(op.Arg)
		}
	}
}

func runWaypointOperations(ferry *Ferry, operations []Operation) {
	for _, op := range operations {
		switch op.Cmd {
		case NorthCmd:
			ferry.MoveWaypoint(North, op.Arg)
		case EastCmd:
			ferry.MoveWaypoint(East, op.Arg)
		case SouthCmd:
			ferry.MoveWaypoint(South, op.Arg)
		case WestCmd:
			ferry.MoveWaypoint(West, op.Arg)
		case RightCmd:
			ferry.RotateWaypoint(op.Arg)
		case LeftCmd:
			ferry.RotateWaypoint(-op.Arg)
		case ForwardCmd:
			ferry.MoveTowardsWaypoint(op.Arg)
		}
	}
}

func parseInput(input string) []Operation {
	rows := strings.Split(input, "\n")
	operations := []Operation{}
	for _, r := range rows {
		op, err := parseOperation(r)
		if err != nil {
			panic(err)
		}

		operations = append(operations, op)
	}

	return operations
}

func parseCommand(rawCommand string) (Command, error) {
	switch rawCommand {
	case "N":
		return NorthCmd, nil
	case "S":
		return SouthCmd, nil
	case "E":
		return EastCmd, nil
	case "W":
		return WestCmd, nil
	case "L":
		return LeftCmd, nil
	case "R":
		return RightCmd, nil
	case "F":
		return ForwardCmd, nil
	default:
		return 0, fmt.Errorf("invalid command: %s", rawCommand)
	}
}

func parseOperation(rawOperation string) (Operation, error) {
	parts := strings.SplitN(rawOperation, "", 2)
	if len(parts) != 2 {
		return Operation{}, fmt.Errorf("invalid operation: %s", rawOperation)
	}

	command, err := parseCommand(parts[0])
	if err != nil {
		return Operation{}, err
	}

	argument, err := strconv.Atoi(parts[1])
	if err != nil {
		return Operation{}, fmt.Errorf("invalid operation: %s", rawOperation)
	}

	operation := Operation{
		Cmd: command,
		Arg: argument,
	}

	return operation, nil
}

type Command int

const (
	NorthCmd = Command(iota)
	EastCmd
	SouthCmd
	WestCmd
	RightCmd
	LeftCmd
	ForwardCmd
)

type Operation struct {
	Cmd Command
	Arg int
}

type Direction Vec

var (
	North = Direction(Vec{0, -1})
	East  = Direction(Vec{1, 0})
	South = Direction(Vec{0, 1})
	West  = Direction(Vec{-1, 0})
)

type Vec struct {
	X int
	Y int
}

func (v *Vec) Add(other Vec) {
	v.X += other.X
	v.Y += other.Y
}

func (v *Vec) Scale(factor int) {
	v.X *= factor
	v.Y *= factor
}

func (v *Vec) Rotate(angle int) {
	rad := degreeToRadians(angle)
	fX := float64(v.X)
	fY := float64(v.Y)

	sin := math.Sin(rad)
	cos := math.Cos(rad)
	newX := fX*cos - fY*sin
	newY := fX*sin + fY*cos

	v.X = int(math.Round(newX))
	v.Y = int(math.Round(newY))
}

func degreeToRadians(value int) float64 {
	return float64(value) * (math.Pi / float64(180))
}

type Waypoint struct {
	Position Vec
}

type Ferry struct {
	Position  Vec
	Direction Vec
	Waypoint  *Waypoint
}

func NewFerry() *Ferry {
	wp := Waypoint{Vec{10, -1}}
	return &Ferry{
		Position:  Vec{0, 0},
		Direction: Vec{1, 0},
		Waypoint:  &wp,
	}
}

func (f *Ferry) Move(dir Direction, distance int) {
	v := Vec(dir)
	v.Scale(distance)
	f.Position.Add(v)
}

func (f *Ferry) MoveWaypoint(dir Direction, distance int) {
	v := Vec(dir)
	v.Scale(distance)
	f.Waypoint.Position.Add(v)
}

func (f *Ferry) MoveTowardsWaypoint(moveCount int) {
	delta := f.Waypoint.Position
	delta.Scale(moveCount)
	f.Position.Add(delta)
}

func (f *Ferry) Forward(distance int) {
	v := f.Direction
	v.Scale(distance)
	f.Position.Add(v)
}

func (f *Ferry) Rotate(angle int) {
	f.Direction.Rotate(angle)
}

func (f *Ferry) RotateWaypoint(angle int) {
	f.Waypoint.Position.Rotate(angle)
}

func (f *Ferry) ManhattenDistance() int {
	return int(math.Abs(float64(f.Position.X)) + math.Abs(float64(f.Position.Y)))
}

const exampleInput = `F10
N3
F7
L270
F11`

const input = `S3
R90
E1
S3
E5
S4
E5
N3
W1
N3
F91
W1
F49
W1
F7
R90
F13
S1
F87
L90
N5
R90
E1
F69
N4
F80
E2
F15
N5
F15
N4
E3
N2
R90
E4
N5
F71
W3
S4
R270
F39
N1
F100
R90
F52
S2
W3
L90
S1
F79
E5
R90
F94
E3
F87
N5
R90
F50
W5
N4
W4
S1
W2
R180
W4
F6
W2
N1
W4
F89
L90
E3
R90
F73
W1
N4
F77
E4
F42
N3
E5
R180
F51
E2
F22
N4
F95
L90
E2
F41
L90
F61
L180
E1
R90
N2
F32
E5
L90
L180
E3
F86
S3
L180
E3
F76
W1
F62
S2
F23
R90
F60
E2
F47
L90
S1
L270
N3
N2
W1
S4
S1
E5
F43
S4
R90
S5
N3
F100
W5
R90
S2
F30
E1
R180
R90
F1
R90
F32
N1
W3
R180
F9
N1
L90
E1
R90
W5
L180
N4
F57
F53
N4
E5
R90
E1
F32
R90
E5
L180
E1
L90
W4
S2
F77
N2
R90
S2
F68
S4
R90
E3
F66
R90
F85
S1
F47
F25
R90
N1
F65
R270
L270
F90
S3
F33
W5
N1
F19
E1
L90
F72
L90
F67
E3
R90
E4
R90
F6
S2
W5
F43
W1
F3
L270
N3
L90
E5
N3
F29
E4
R90
E4
L90
F32
E5
F52
N4
L270
N1
W5
R90
W5
N5
E3
N5
W2
L180
N2
E1
S4
N1
W4
F8
N2
W2
F80
N3
F98
L90
W3
L90
S2
L90
F98
L180
F12
E3
N2
F60
N5
W1
F54
W3
F74
E4
S3
R90
F94
L90
S2
L90
N4
E2
F83
R90
N2
E1
F86
E5
N1
R90
E5
R180
F72
L180
W5
L90
E4
L90
E1
F26
W4
F4
L90
W4
L90
E5
F92
R90
N1
L90
N5
F21
L90
L180
W3
F76
L90
F14
L90
E5
F50
S5
F18
W5
F27
S5
F52
L180
N3
L180
W2
N5
W3
S2
F48
W2
F57
R90
E3
R180
E4
F42
W2
F21
E2
S4
E2
N4
W4
S4
E3
F23
S1
W5
F70
S5
F77
W5
S3
L180
E4
L90
W2
L90
E1
F15
S4
F38
N1
L90
F27
N1
F34
L90
E1
S2
E2
L90
N3
F71
W4
S4
F14
R90
F58
R90
F8
R90
E1
L270
F12
E2
L90
F9
R90
S5
F48
S5
L90
E1
F7
W2
F97
S1
R90
N5
E2
N4
F20
R90
N1
R90
F84
R90
E3
S3
F34
S3
F41
R90
F15
L180
N1
F4
L180
W3
F50
S4
W3
F45
W1
S4
E2
L90
E5
R270
S3
L90
S4
F59
W4
R180
F10
E1
R90
S4
W5
L90
F45
S4
F92
R90
E5
F36
S1
L90
W1
S5
F30
W5
F80
N4
E2
R90
F98
F7
S5
W2
F1
N3
L180
N2
F49
E3
S5
S5
W4
E5
E3
S1
W1
F10
L90
F40
E4
N4
F25
R180
W5
N5
W1
S4
L90
F79
L180
S1
F100
S1
F34
S2
E4
L90
N2
F36
E1
N3
L90
F42
R90
W5
L270
F45
E4
F95
N5
F22
L90
F67
R180
S5
E5
R90
N1
R180
W3
F80
E4
F19
W5
N4
R90
N5
F28
F86
R180
R90
S2
E1
N5
W1
S3
W5
S3
F51
W5
S3
R90
R90
F35
W2
F51
N1
R180
S2
F37
R270
W2
S2
L90
L180
E4
N1
L180
E2
S1
F38
R90
N2
W1
R90
L180
E4
S3
R180
E3
S4
F65
E2
R90
N1
E4
L180
E5
R90
W2
F22
W5
N1
R90
E1
F34
S1
R90
N3
F62
F48
E5
F85
E5
S4
R90
E3
R90
E1
F54
S4
W3
W2
R90
N5
W4
N3
R180
E1
F88
W5
F49
E5
N4
W3
F81
R90
F58
N4
L90
F88
N3
F87
E2
N4
L90
N5
N1
F87
E1
L90
N3
W1
F13
F3
N4
L90
E3
S1
F35
S4
F1
L90
E1
N4
R90
F60
N2
W1
N1
R180
L180
S3
R90
E4
R180
W3
N1
W1
F3
L90
F8
W1
R90
S1
F84
L90
S3
F63
F11
E1
F38
R90
S3
R270
F50
R90
F15
L270
E1
S1
E5
L180
N4
L90
F37
N3
E5
F13
L180
E5
F88
E2
F9
W3
F6
N5
F75
N5
W5
S5
E3
R90
N5
E4
N3
L180
F1
R90
S3
E1
S4
E5
N3
R270
E3
L90
N3
S2
L90
S5
F90
R90
N1
R90
F50
F33
W3
F23
E3
R90
F81
E1
N3
R180
F16
S2
F35
R90
W5
F31
R90
S2
L180
E5
R180
F89
N1
L90
F68
W3
R90
W3
N1
L90
N4
L180
S5
R180
N4
R90
W5
N5
L180
N2
E4
N1
W4
F98
S5
F70
W5
F76`
