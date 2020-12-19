package main

import (
	"fmt"
	"strings"
)

func main() {

	// Example
	exampleState := parseInput(exampleInput, vec3Maker)
	exampleDimension := NewPocketDimension(exampleState)
	runCycles(exampleDimension, 6)
	fmt.Printf("Example result: %d\n", exampleDimension.GetActiveCubeCount())

	// Scenario 1
	state := parseInput(input, vec3Maker)
	dimension := NewPocketDimension(state)
	runCycles(dimension, 6)
	fmt.Printf("Scenario 1 result: %d\n", dimension.GetActiveCubeCount())

	// Example 2
	exampleState2 := parseInput(exampleInput, vec4Maker)
	exampleDimension2 := NewPocketDimension(exampleState2)
	runCycles(exampleDimension2, 6)
	fmt.Printf("Example 2 result: %d\n", exampleDimension2.GetActiveCubeCount())

	// Scenario 2
	state2 := parseInput(input, vec4Maker)
	dimension2 := NewPocketDimension(state2)
	runCycles(dimension2, 6)
	fmt.Printf("Scenario 2 result: %d\n", dimension2.GetActiveCubeCount())
}

func runCycles(dimension *PocketDimension, count int) {
	for i := 0; i < count; i++ {
		dimension.RunCycle()
	}
}

type VecMaker func(x, y int) Vec

func vec3Maker(x, y int) Vec {
	return Vec3{x, y, 0}
}

func vec4Maker(x, y int) Vec {
	return Vec4{x, y, 0, 0}
}

func parseInput(input string, vecMaker VecMaker) map[Vec]Cube {
	cubes := map[Vec]Cube{}

	rows := strings.Split(input, "\n")
	for y, row := range rows {
		for x, r := range row {
			vec := vecMaker(x, y)

			var cube Cube
			switch r {
			case '#':
				cube = Cube{true}
			case '.':
				cube = Cube{false}
			default:
				panic(fmt.Errorf("not a valid cube state: %c", r))
			}

			cubes[vec] = cube
		}
	}

	return cubes
}

type Vec interface {
	GetAdjacentVecs() []Vec
}

type Vec3 struct {
	X int
	Y int
	Z int
}

func (v Vec3) GetAdjacentVecs() []Vec {
	vecs := []Vec{}
	for x := -1; x < 2; x++ {
		for y := -1; y < 2; y++ {
			for z := -1; z < 2; z++ {
				neighbour := Vec3{v.X + x, v.Y + y, v.Z + z}
				if neighbour == v {
					continue
				}
				vecs = append(vecs, neighbour)
			}
		}
	}

	return vecs
}

type Vec4 struct {
	X int
	Y int
	Z int
	W int
}

func (v Vec4) GetAdjacentVecs() []Vec {
	vecs := []Vec{}
	for x := -1; x < 2; x++ {
		for y := -1; y < 2; y++ {
			for z := -1; z < 2; z++ {
				for w := -1; w < 2; w++ {
					neighbour := Vec4{v.X + x, v.Y + y, v.Z + z, v.W + w}
					if neighbour == v {
						continue
					}
					vecs = append(vecs, neighbour)
				}
			}
		}
	}

	return vecs
}

type Cube struct {
	Active bool
}

type PocketDimension struct {
	cubes map[Vec]Cube
}

func NewPocketDimension(initialState map[Vec]Cube) *PocketDimension {
	return &PocketDimension{
		cubes: initialState,
	}
}

func (pd *PocketDimension) RunCycle() {
	pd.initializeInactiveCubes()

	newCubes := make(map[Vec]Cube, len(pd.cubes))
	for vec, cube := range pd.cubes {
		newCubes[vec] = pd.calculateNewState(vec, cube)
	}

	pd.cubes = newCubes
}

func (pd *PocketDimension) GetActiveCubeCount() int {
	sum := 0
	for _, cube := range pd.cubes {
		if cube.Active {
			sum++
		}
	}

	return sum
}

func (pd *PocketDimension) initializeInactiveCubes() {
	for vec, cube := range pd.cubes {
		if cube.Active {
			for _, v := range vec.GetAdjacentVecs() {
				_, ok := pd.cubes[v]
				if !ok {
					pd.cubes[v] = Cube{false}
				}
			}
		}
	}
}

func (pd *PocketDimension) calculateNewState(vec Vec, cube Cube) Cube {
	adjacentCubes := pd.getAdjacentCubes(vec)

	activeNeighbourCount := 0
	for _, cube := range adjacentCubes {
		if cube.Active {
			activeNeighbourCount++
		}
	}

	active := false
	if cube.Active && (activeNeighbourCount == 2 || activeNeighbourCount == 3) {
		active = true
	}
	if !cube.Active && activeNeighbourCount == 3 {
		active = true
	}

	return Cube{active}
}

func (pd *PocketDimension) getAdjacentCubes(vec Vec) []Cube {
	cubes := []Cube{}
	for _, v := range vec.GetAdjacentVecs() {
		cube, ok := pd.cubes[v]
		if ok {
			cubes = append(cubes, cube)
		}
	}

	return cubes
}

const exampleInput = `.#.
..#
###`

const input = `#.##....
.#.#.##.
###.....
....##.#
#....###
.#.#.#..
.##...##
#..#.###`
