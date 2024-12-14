package twentytwentyfour

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input14, part1Test14, part2Test14 = InputsForDay(14)

type robot struct {
	pos Vec2
	velocity Vec2
}

func parseVec2(data string) (Vec2, error) {
	parts := strings.Split(data, ",")

	x, err := strconv.Atoi(strings.TrimSpace(parts[0]))
		if err != nil {
			return Vec2{}, err
		}
	y, err := strconv.Atoi(strings.TrimSpace(parts[1]))
		if err != nil {
			return Vec2{}, err
		}

	return Vec2{X:x, Y:y}, nil
}

func parseRobot(data string) robot {
	parts := strings.Split(data, " ")
	pos, err := parseVec2(parts[0][2:])
	if err != nil {
		panic(err)
	}
	velocity, err := parseVec2(parts[1][2:])
	if err != nil {
		panic(err)
	}

	return robot{pos:pos, velocity:velocity}
}

func simulateRobot(r robot, bounds Vec2, seconds int) Vec2 {
	pos := r.pos.Add(r.velocity.Scale(seconds))
	pos.X = pos.X % bounds.X
	if pos.X < 0 {
		pos.X += bounds.X
	}
	pos.Y = pos.Y % bounds.Y
	if pos.Y < 0 {
		pos.Y += bounds.Y
	}
	return pos
}

func countRobotsPerQuadrant(positions []Vec2, bounds Vec2) [4]int {
	half := bounds.Div(2)
	quadrants := [4]int{}

	for _, p := range positions {
		if p.X < half.X && p.Y < half.Y {
			quadrants[0]++
		} else if p.X < half.X && p.Y > half.Y {
			quadrants[1]++
		} else if p.X > half.X && p.Y > half.Y {
			quadrants[2]++
		} else if p.X > half.X && p.Y < half.Y {
			quadrants[3]++
		}
	}

	return quadrants
}

func drawRobots(positions []Vec2, bounds Vec2) {
	m := Matrix[rune]{}
	for y := 0; y < bounds.Y; y++ {
		m = append(m, []rune{})
		for x := 0; x < bounds.X; x++ {
			m[y] = append(m[y], '.')
		}
	}

	for _, p := range positions {
		m.Set(p, 'X')
	}

	for _, row := range m {
		for _, tile := range row {
			fmt.Print(string(tile))
		}
		fmt.Println()
	}
}

func Test14Part1Test(t *testing.T) {
	data, err := InputToLines (part1Test14)
	AssertNoError(t, err, "InputToLines")

	bounds := Vec2{X: 11, Y: 7}
	robots := Map(data, parseRobot)
	positions := Map(robots, func(r robot) Vec2 {
		return simulateRobot(r, bounds, 100)
	})
	quadrants := countRobotsPerQuadrant(positions, bounds)

	safetyScore := Product(quadrants[:])
	AssertEquals(t, 12, safetyScore, "safety score")
}

func Test14Part1(t *testing.T) {
	data, err := InputToLines (input14)
	AssertNoError(t, err, "InputToLines")

	bounds := Vec2{X: 101, Y: 103}
	robots := Map(data, parseRobot)
	positions := Map(robots, func(r robot) Vec2 {
		return simulateRobot(r, bounds, 100)
	})
	quadrants := countRobotsPerQuadrant(positions, bounds)

	safetyScore := Product(quadrants[:])
	AssertEquals(t, 229868730, safetyScore, "safety score")
}

func Test14Part2(t *testing.T) {
	data, err := InputToLines (input14)
	AssertNoError(t, err, "InputToLines")

	bounds := Vec2{X: 101, Y: 103}
	robots := Map(data, parseRobot)

	// flat 33, 136, 239
	// heigh 84, 185, 286

	for i := 0; i < 10000; i+=101 {
	positions := Map(robots, func(r robot) Vec2 {
		return simulateRobot(r, bounds, 84+i)
	})
		fmt.Println(i)
		drawRobots(positions, bounds)
		fmt.Println()
	}

	AssertEquals(t, 7861, 7861, "christmas tree at second")
}
