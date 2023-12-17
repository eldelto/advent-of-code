package twentytwentythree

import (
	"container/heap"
	"fmt"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input17, part1Test17, _ = InputsForDay(17)

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
}

func aStar2(matrix Matrix[int]) crucible {
	current := crucible{
		pos:       Vec2{0, 0},
		direction: East,
		stepCount: 1,
	}
	target := Vec2{len(matrix) - 1, len(matrix[0]) - 1}

	open := PriorityQueue[*crucibleItem]{}
	closed := map[nodeKey]struct{}{}

	for current.pos != target {
		closed[nodeKey{current.pos, current.direction}] = struct{}{}

		possiblePaths := possiblePaths(matrix, current)
		fmt.Println(current)
		// fmt.Println(possiblePaths)
		// fmt.Println()
		for _, path := range possiblePaths {
			k := nodeKey{pos: path.pos, direction: path.direction}
			if _, ok := closed[k]; ok {
				// fmt.Println("skip")
				continue
			}

			traversalCost := current.heatLoss + matrix.Get(path.pos)
			// estimatedCost := int(aStarHeuristic(path.pos, target)) + traversalCost
			estimatedCost := traversalCost
			stepCount := 1
			if path.direction == current.direction {
				stepCount = current.stepCount + 1
			}
			// parent := current
			newOpenNode := crucible{
				pos:       path.pos,
				direction: path.direction,
				heatLoss:  traversalCost,
				stepCount: stepCount,
				// parent:    &parent,
			}

			heap.Push(&open, &crucibleItem{crucible: newOpenNode, estimatedCost: estimatedCost})
		}

		newCurrentItem, _ := heap.Pop(&open).(*crucibleItem)
		current = newCurrentItem.crucible
		// for {
		// 	newCurrentItem, _ := heap.Pop(&open).(*crucibleItem)
		// 	k := nodeKey{newCurrentItem.crucible.pos, newCurrentItem.crucible.direction}
		// 	if _, ok := closed[k]; !ok {
		// 		current = newCurrentItem.crucible
		// 		break
		// 	}
		// }
	}

	return current
}

type aStarNode struct {
	pos           Vec2
	traversalCost int
	estimatedCost int
	parent        *aStarNode
	index         int
}

func (n aStarNode) Priority() int {
	return n.estimatedCost
}

func (n aStarNode) Index() int {
	return n.index
}

func (n aStarNode) SetIndex(i int) {
	n.index = i
}

func aStarHeuristic(current, target Vec2) uint {
	// return current.ManhattenDistance(target)
	return target.Sub(current).Len()
}

func possibleNodes(matrix Matrix[int], current aStarNode) []Vec2 {
	positions := []Vec2{
		current.pos.Add(Vec2(North)),
		current.pos.Add(Vec2(East)),
		current.pos.Add(Vec2(South)),
		current.pos.Add(Vec2(West)),
	}

	if current.parent != nil {
		dst := &current
		src := dst.parent
		previousDirection := DirectionFromPositions(src.pos, dst.pos)
		straightCount := 1
		for i := 0; i < 2; i++ {
			if src.parent == nil {
				break
			}

			dst = src
			src = dst.parent
			direction := DirectionFromPositions(src.pos, dst.pos)
			if direction != previousDirection {
				break
			}
			previousDirection = direction
			straightCount++
		}

		// Need to turn
		if straightCount >= 3 {
			positions = []Vec2{
				current.pos.Add(Vec2(previousDirection.Left())),
				current.pos.Add(Vec2(previousDirection.Right())),
			}
		}
	}

	return Filter(positions, func(pos Vec2) bool {
		return matrix.WithinBounds(pos)
	})
}

type key struct {
	pos Vec2
	// parent Vec2
}

func aStar(matrix Matrix[int]) aStarNode {
	current := aStarNode{pos: Vec2{0, 0}}
	target := Vec2{len(matrix) - 1, len(matrix[0]) - 1}

	// open := []aStarNode{}
	open := PriorityQueue[aStarNode]{}
	closed := map[key]struct{}{} //map[Vec2]aStarNode{current.pos: current}

	for current.pos != target {
		// fmt.Println(current.pos)
		k := key{pos: current.pos}
		// if current.parent != nil {
		// 	k.parent = current.parent.pos
		// }
		closed[k] = struct{}{} //current

		possibleNodes := possibleNodes(matrix, current)
		for _, possibleNode := range possibleNodes {
			k := key{pos: possibleNode} //, parent: current.pos}
			if _, ok := closed[k]; ok {
				// fmt.Println("skip")
				continue
			}

			traversalCost := current.traversalCost + matrix.Get(possibleNode)
			estimatedCost := int(aStarHeuristic(possibleNode, target)) + traversalCost
			parent := current
			newOpenNode := aStarNode{
				pos:           possibleNode,
				traversalCost: traversalCost,
				estimatedCost: int(estimatedCost),
				parent:        &parent}

			// open = append(open, newOpenNode)
			heap.Push(&open, newOpenNode)
			// slices.SortFunc(open, func(a, b aStarNode) int {
			// 	return a.estimatedCost - b.estimatedCost
			// })
		}

		newCurrent, _ := heap.Pop(&open).(aStarNode)
		current = newCurrent
		// open = open[1:]
		// for i := 0; i < len(open); i++ {
		// 	next := open[i]
		// 	if _, ok := closed[next.pos]; !ok {
		// 		current = next
		// 		break
		// 	}

		// 	if i == len(open)-1 {
		// 		panic("no possible node found")
		// 	}
		// }
	}

	return current
}

func nodeContainsPos(node aStarNode, pos Vec2) bool {
	if node.pos == pos {
		return true
	}

	if node.parent != nil {
		return nodeContainsPos(*node.parent, pos)
	}

	return false
}

func printPath(m Matrix[int], node aStarNode) {
	for ri, row := range m {
		for ci := range row {
			pos := Vec2{ci, ri}
			value := m.Get(pos)
			if nodeContainsPos(node, pos) {
				fmt.Print("*")
			} else {
				fmt.Print(value)
			}
		}
		fmt.Println()
	}
}

func crucibleTraversedPos(node crucible, pos Vec2) bool {
	if node.pos == pos {
		return true
	}

	if node.parent != nil {
		return crucibleTraversedPos(*node.parent, pos)
	}

	return false
}

func printCruciblePath(m Matrix[int], node crucible) {
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

func Test17Part1Test(t *testing.T) {
	matrix, err := InputIntoMatrix(part1Test17, IntParser)
	AssertNoError(t, err, "InputIntoMatrix")

	node := aStar2(matrix)
	// printPath(matrix, node)
	// fmt.Println(matrix.String())
	// printCruciblePath(matrix, node)
	AssertEquals(t, 102, node.heatLoss, "node")
}

func Test17Part1(t *testing.T) {
	matrix, err := InputIntoMatrix(input17, IntParser)
	AssertNoError(t, err, "InputIntoMatrix")

	node := aStar2(matrix)
	// printPath(matrix, node)
	AssertEquals(t, 102, node.heatLoss, "node")
}
