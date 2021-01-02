package main

import (
	"fmt"
	"testing"
)

func TestNewTile(t *testing.T) {
	tests := []struct {
		name              string
		data              [][]rune
		wantPossibleEdges [8]Edge
	}{
		{
			"1",
			[][]rune{
				[]rune("#.."),
				[]rune("..#"),
				[]rune("..#"),
			},
			[8]Edge{
				Edge("#.."),
				Edge(".##"),
				Edge("#.."),
				Edge("..#"),

				Edge("..#"),
				Edge("##."),
				Edge("..#"),
				Edge("#.."),
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tile := NewTile(0, tt.data)

			for i := range tile.Edges {
				want := tt.wantPossibleEdges[i]
				got := tile.Edges[i]

				if !want.Equals(got) {
					t.Errorf("(index %d) wanted edge %s but got %s", i, string(want), string(got))
				}
			}

			for i := range tile.PossibleEdges {
				want := tt.wantPossibleEdges[i]
				got := tile.PossibleEdges[i]

				if !want.Equals(got) {
					t.Errorf("(index %d) wanted possible edge %s but got %s", i, string(want), string(got))
				}
			}
		})
	}
}

func TestGrid_FindNeighbourEdges(t *testing.T) {
	tile := NewTile(0, [][]rune{
		[]rune("#.."),
		[]rune("..#"),
		[]rune("..#"),
	})

	grid := NewGrid(9)
	grid.Tiles[0][1] = tile
	grid.Tiles[1][2] = tile
	grid.Tiles[2][1] = tile
	grid.Tiles[1][0] = tile

	wantEdges := [4]Edge{
		Edge("..#"),
		Edge("#.."),
		Edge("..#"),
		Edge("##."),
	}

	gotEdges := grid.FindNeighbourEdges(Vec2{1, 1})

	for i := range gotEdges {
		want := wantEdges[i]
		got := gotEdges[i]

		if !want.Equals(got) {
			t.Errorf("(index %d) wanted neighbour edge %s but got %s", i, string(want), string(got))
		}
	}
}

func Test_canAttach_Full(t *testing.T) {
	neighbour := NewTile(0, [][]rune{
		[]rune("#.."),
		[]rune("..#"),
		[]rune("..#"),
	})

	grid := NewGrid(9)
	grid.Tiles[0][1] = neighbour
	grid.Tiles[1][2] = neighbour
	grid.Tiles[2][1] = neighbour
	grid.Tiles[1][0] = neighbour

	tile := NewTile(0, [][]rune{
		[]rune("..#"),
		[]rune("#.."),
		[]rune("#.."),
	})

	if !canAttach(tile, grid.FindNeighbourEdges(Vec2{1, 1})) {
		t.Errorf("tile should be able to attach")
	}
}

func Test_canAttach_Partial(t *testing.T) {
	neighbour := NewTile(0, [][]rune{
		[]rune("#.."),
		[]rune("..#"),
		[]rune("..#"),
	})

	grid := NewGrid(9)
	grid.Tiles[0][1] = neighbour
	grid.Tiles[1][0] = neighbour

	tile := NewTile(0, [][]rune{
		[]rune("..#"),
		[]rune("#.#"),
		[]rune("###"),
	})

	if !canAttach(tile, grid.FindNeighbourEdges(Vec2{1, 1})) {
		t.Errorf("tile should be able to attach")
	}
}

func Test_canAttach_Single(t *testing.T) {
	neighbour := NewTile(0, [][]rune{
		[]rune("#.."),
		[]rune("..#"),
		[]rune("..#"),
	})

	grid := NewGrid(9)
	grid.Tiles[1][0] = neighbour

	tile := NewTile(0, [][]rune{
		[]rune(".##"),
		[]rune("###"),
		[]rune("###"),
	})

	if !canAttach(tile, grid.FindNeighbourEdges(Vec2{1, 1})) {
		t.Errorf("tile should be able to attach")
	}
}

func TestTile_Rotate(t *testing.T) {
	tests := []struct {
		count     int
		wantEdges [4]Edge
	}{
		{
			0,
			[4]Edge{
				Edge("#.."),
				Edge(".##"),
				Edge("#.."),
				Edge("..#"),
			},
		},
		{
			1,
			[4]Edge{
				Edge("..#"),
				Edge("#.."),
				Edge(".##"),
				Edge("#.."),
			},
		},
		{
			2,
			[4]Edge{
				Edge("#.."),
				Edge("..#"),
				Edge("#.."),
				Edge(".##"),
			},
		},
		{
			3,
			[4]Edge{
				Edge(".##"),
				Edge("#.."),
				Edge("..#"),
				Edge("#.."),
			},
		},
		{
			4,
			[4]Edge{
				Edge("#.."),
				Edge(".##"),
				Edge("#.."),
				Edge("..#"),
			},
		},
	}
	for _, tt := range tests {
		t.Run(fmt.Sprintf("%d time(s)", tt.count), func(t *testing.T) {

			tile := NewTile(0, [][]rune{
				[]rune("#.."),
				[]rune("..#"),
				[]rune("..#"),
			})

			for i := 0; i < tt.count; i++ {
				tile.Rotate()
			}

			for i := range tile.Edges {
				want := tt.wantEdges[i]
				got := tile.Edges[i]

				if !want.Equals(got) {
					t.Errorf("(index %d) wanted edge %s but got %s", i, string(want), string(got))
				}
			}
		})
	}
}
