package twentytwentythree

import (
	"container/heap"
	"fmt"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input17, part1Test17, part2Test17 = InputsForDay(17)

type crucible struct {
	pos       Vec2
	direction Direction
	heatLoss  int
	stepCount int
	parent    *crucible
}

func (c *crucible) canGoStraight() bool {
	return c.stepCount%3 != 0
}

type path struct {
	pos       Vec2
	direction Direction
}

func possiblePaths(m Matrix[int], c crucible) []path {
	var paths []path
	if c.canGoStraight() {
		paths = []path{
			{c.pos.Add(Vec2(c.direction)), c.direction},
			{c.pos.Add(Vec2(c.direction.Left())), c.direction.Left()},
			{c.pos.Add(Vec2(c.direction.Right())), c.direction.Right()},
		}
	} else {
		paths = []path{
			{c.pos.Add(Vec2(c.direction.Left())), c.direction.Left()},
			{c.pos.Add(Vec2(c.direction.Right())), c.direction.Right()},
		}
	}

	return Filter(paths, func(p path) bool {
		return m.WithinBounds(p.pos)
	})
}

type crucibleItem struct {
	crucible      crucible
	estimatedCost int
	index         int
}

func (n *crucibleItem) Priority() int {
	// return n.crucible.heatLoss
	return n.estimatedCost
}

func (n *crucibleItem) Index() int {
	return n.index
}

func (n *crucibleItem) SetIndex(i int) {
	n.index = i
}

type nodeKey struct {
	pos       Vec2
	direction Direction
	stepCount int
}

func aStarHeuristic(current, target Vec2) uint {
	// return current.ManhattenDistance(target)
	return target.Sub(current).Len()
}

func aStar(matrix Matrix[int]) crucible {
	current := crucible{
		pos:       Vec2{0, 0},
		direction: East,
		stepCount: 1,
	}
	target := Vec2{len(matrix) - 1, len(matrix[0]) - 1}

	open := PriorityQueue[*crucibleItem]{}
	closed := map[nodeKey]crucible{}

	for current.pos != target {
		k := nodeKey{
			pos:       current.pos,
			direction: current.direction,
			stepCount: current.stepCount,
		}
		closed[k] = current

		possiblePaths := possiblePaths(matrix, current)
		// fmt.Println(current)
		// fmt.Println(possiblePaths)
		// fmt.Println()
		for _, path := range possiblePaths {
			traversalCost := current.heatLoss + matrix.Get(path.pos)
			stepCount := 1
			if path.direction == current.direction {
				stepCount = current.stepCount + 1
			}

			estimatedCost := int(aStarHeuristic(path.pos, target)) + traversalCost
			// estimatedCost := traversalCost

			// parent := current
			newOpenNode := crucible{
				pos:       path.pos,
				direction: path.direction,
				heatLoss:  traversalCost,
				stepCount: stepCount,
				// parent:    &parent,
			}

			k := nodeKey{
				pos:       newOpenNode.pos,
				direction: newOpenNode.direction,
				stepCount: newOpenNode.stepCount,
			}
			if _, ok := closed[k]; ok {
				// fmt.Println("skip")
				continue
			}

			heap.Push(&open, &crucibleItem{crucible: newOpenNode, estimatedCost: estimatedCost})
		}

		for {
			newCurrentItem, _ := heap.Pop(&open).(*crucibleItem)
			c := newCurrentItem.crucible
			k := nodeKey{
				pos:       c.pos,
				direction: c.direction,
				stepCount: c.stepCount,
			}
			if _, ok := closed[k]; ok {
				continue
			}

			current = newCurrentItem.crucible
			break
		}
	}

	return current
}

func crucibleTraversedPos(node ultraCrucible, pos Vec2) bool {
	if node.pos == pos {
		return true
	}

	if node.parent != nil {
		return crucibleTraversedPos(*node.parent, pos)
	}

	return false
}

func printCruciblePath(m Matrix[int], node ultraCrucible) {
	for ri, row := range m {
		for ci := range row {
			pos := Vec2{ci, ri}
			value := m.Get(pos)
			if crucibleTraversedPos(node, pos) {
				fmt.Print("*")
			} else {
				fmt.Print(value)
			}
		}
		fmt.Println()
	}
}

type ultraCrucible struct {
	pos       Vec2
	direction Direction
	heatLoss  int
	stepCount int
	parent    *ultraCrucible
}

func (c *ultraCrucible) canGoStraight() bool {
	return c.stepCount%10 != 0
}

func (c *ultraCrucible) canTurn() bool {
	return c.stepCount >= 4
}

func possibleUltraCruciblePaths(m Matrix[int], c ultraCrucible) []path {
	paths := []path{}
	if c.canGoStraight() {
		paths = append(paths, path{c.pos.Add(Vec2(c.direction)), c.direction})
	}
	if c.canTurn() {
		paths = append(paths, path{c.pos.Add(Vec2(c.direction.Left())), c.direction.Left()})
		paths = append(paths, path{c.pos.Add(Vec2(c.direction.Right())), c.direction.Right()})
	}

	return Filter(paths, func(p path) bool {
		return m.WithinBounds(p.pos)
	})
}

type ultraCrucibleItem struct {
	crucible      ultraCrucible
	estimatedCost int
	index         int
}

func (n *ultraCrucibleItem) Priority() int {
	// return n.crucible.heatLoss
	return n.estimatedCost
}

func (n *ultraCrucibleItem) Index() int {
	return n.index
}

func (n *ultraCrucibleItem) SetIndex(i int) {
	n.index = i
}

func ultraAStar(matrix Matrix[int]) ultraCrucible {
	current := ultraCrucible{
		pos:       Vec2{0, 0},
		direction: East,
		stepCount: 1,
	}
	target := Vec2{len(matrix[0]) - 1, len(matrix) - 1}

	open := PriorityQueue[*ultraCrucibleItem]{}
	closed := map[nodeKey]ultraCrucible{}

	// Setup downwards search
	downwards := current
	downwards.direction = South
	heap.Push(&open, &ultraCrucibleItem{crucible: downwards, estimatedCost: 0})

	for current.pos != target || current.stepCount < 4 {
		if current.stepCount >= 4 {
			k := nodeKey{
				pos:       current.pos,
				direction: current.direction,
				stepCount: current.stepCount,
			}
			closed[k] = current
		}

		possiblePaths := possibleUltraCruciblePaths(matrix, current)
		// fmt.Println(current)
		// fmt.Println(possiblePaths)
		// fmt.Println()
		for _, path := range possiblePaths {
			traversalCost := current.heatLoss + matrix.Get(path.pos)
			stepCount := 1
			if path.direction == current.direction {
				stepCount = current.stepCount + 1
			}

			// estimatedCost := int(aStarHeuristic(path.pos, target)) + traversalCost
			estimatedCost := traversalCost

			parent := current
			newOpenNode := ultraCrucible{
				pos:       path.pos,
				direction: path.direction,
				heatLoss:  traversalCost,
				stepCount: stepCount,
				parent:    &parent,
			}

			k := nodeKey{
				pos:       newOpenNode.pos,
				direction: newOpenNode.direction,
				stepCount: newOpenNode.stepCount,
			}
			if _, ok := closed[k]; ok {
				// fmt.Println("skip")
				continue
			}

			heap.Push(&open, &ultraCrucibleItem{crucible: newOpenNode, estimatedCost: estimatedCost})
		}

		for {
			newCurrentItem, _ := heap.Pop(&open).(*ultraCrucibleItem)
			c := newCurrentItem.crucible
			k := nodeKey{
				pos:       c.pos,
				direction: c.direction,
				stepCount: c.stepCount,
			}
			if _, ok := closed[k]; ok {
				continue
			}

			current = newCurrentItem.crucible
			break
		}
	}

	return current
}

func Test17Part1Test(t *testing.T) {
	matrix, err := InputIntoMatrix(part1Test17, IntParser)
	AssertNoError(t, err, "InputIntoMatrix")

	node := aStar(matrix)
	// printPath(matrix, node)
	// fmt.Println(matrix.String())
	// printCruciblePath(matrix, node)
	AssertEquals(t, 102, node.heatLoss, "node")
}

func Test17Part1(t *testing.T) {
	matrix, err := InputIntoMatrix(input17, IntParser)
	AssertNoError(t, err, "InputIntoMatrix")

	node := aStar(matrix)
	// printPath(matrix, node)
	AssertEquals(t, 1155, node.heatLoss, "node")
}

func Test17Part2Test(t *testing.T) {
	matrix, err := InputIntoMatrix(part1Test17, IntParser)
	AssertNoError(t, err, "InputIntoMatrix")

	node := ultraAStar(matrix)
	fmt.Println(matrix.String())
	printCruciblePath(matrix, node)
	AssertEquals(t, 94, node.heatLoss, "node")
}

func Test17Part2Test2(t *testing.T) {
	matrix, err := InputIntoMatrix(part2Test17, IntParser)
	AssertNoError(t, err, "InputIntoMatrix")

	node := ultraAStar(matrix)
	fmt.Println(matrix.String())
	printCruciblePath(matrix, node)
	AssertEquals(t, 71, node.heatLoss, "node")
}

func Test17Part2(t *testing.T) {
	matrix, err := InputIntoMatrix(input17, IntParser)
	AssertNoError(t, err, "InputIntoMatrix")

	node := ultraAStar(matrix)
	fmt.Println(matrix.String())
	printCruciblePath(matrix, node)
	AssertEquals(t, 1283, node.heatLoss, "node")
	// 1005 < answer < 1286
}
