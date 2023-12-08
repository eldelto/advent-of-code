package twentytwentythree

import (
	"fmt"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input08, part1Test08, part2Test08 = InputsForDay(8)

type directions struct {
	turns  string
	offset uint
}

func (d *directions) next() byte {
	turn := d.turns[d.offset]
	d.offset = (d.offset + 1) % uint(len(d.turns))
	return turn
}

func (d *directions) at(offset uint) byte {
	return d.turns[offset%uint(len(d.turns))]
}

type directionNode struct {
	id      string
	isANode bool
	isZNode bool
	left    *directionNode
	right   *directionNode
}

func newDirectionNode(id string) *directionNode {
	return &directionNode{
		id:      id,
		isANode: id[2] == 'A',
		isZNode: id[2] == 'Z',
	}
}

type directionNodeCache map[string]*directionNode

func (nc directionNodeCache) get(id string) *directionNode {
	node, ok := nc[id]
	if !ok {
		node = newDirectionNode(id)
		nc[id] = node
	}

	return node
}

func parseDirectionNode(rawData string, cache directionNodeCache) {
	parts := strings.Split(rawData, " = ")
	node := cache.get(parts[0])

	rawChildren := parts[1]
	rawChildren = rawChildren[1 : len(rawChildren)-1]
	children := strings.Split(rawChildren, ", ")

	left := cache.get(children[0])
	right := cache.get(children[1])
	node.left = left
	node.right = right
}

func findANodes(cache directionNodeCache) []*directionNode {
	aNodes := []*directionNode{}
	for _, v := range cache {
		if v.isANode && v.left != nil && v.right != nil {
			aNodes = append(aNodes, v)
		}
	}

	return aNodes
}

func parseDirectionMap(data string) (directions, *directionNode, []*directionNode) {
	parts := strings.Split(data, "\n\n")

	directions := directions{turns: parts[0]}

	rawNodes := strings.Split(parts[1], "\n")
	cache := directionNodeCache{}
	ForEach(rawNodes, func(s string) { parseDirectionNode(s, cache) })

	return directions, cache.get("AAA"), findANodes(cache)
}

func takeStep(node **directionNode, turn byte) {
	n := *node
	switch turn {
	case 'L':
		*node = n.left
	case 'R':
		*node = n.right
	default:
		panic(fmt.Sprintf("invalid turn %q", turn))
	}
}

func followDirections(directions directions, node *directionNode) uint {
	var steps uint
	for {
		if node.id == "ZZZ" {
			return steps
		}

		steps++
		takeStep(&node, directions.next())
	}
}

type ghostWalker struct {
	node   *directionNode
	offset uint
}

func (gw *ghostWalker) walkTo(directions directions, offset uint) {
	for i := gw.offset + 1; i <= offset; i++ {
		takeStep(&gw.node, directions.at(i))
	}
}

func takeParStep(nodes []*directionNode, turn byte) {
	for i := range nodes {
		takeStep(&nodes[i], turn)
	}
}

func areAllZNodes(nodes []*directionNode) bool {
	for _, node := range nodes {
		if !node.isZNode {
			return false
		}
	}

	return true
}

func followGhostDirections(directions directions, nodes []*directionNode) uint {
	var steps uint
	for {
		if areAllZNodes(nodes) {
			return steps
		}

		steps++
		takeParStep(nodes, directions.next())
	}
}

func Test08Part1Test(t *testing.T) {
	content, err := InputToString(part1Test08)
	AssertNoError(t, err, "InputToString")

	directions, node, _ := parseDirectionMap(content)
	steps := followDirections(directions, node)
	AssertEquals(t, uint(2), steps, "steps")
}

func Test08Part1(t *testing.T) {
	content, err := InputToString(input08)
	AssertNoError(t, err, "InputToString")

	directions, node, _ := parseDirectionMap(content)
	steps := followDirections(directions, node)
	AssertEquals(t, uint(19637), steps, "steps")
}

func Test08Part2Test(t *testing.T) {
	content, err := InputToString(part2Test08)
	AssertNoError(t, err, "InputToString")

	directions, _, aNodes := parseDirectionMap(content)
	steps := followGhostDirections(directions, aNodes)
	AssertEquals(t, uint(6), steps, "steps")
}

func Test08Part2(t *testing.T) {
	content, err := InputToString(input08)
	AssertNoError(t, err, "InputToString")

	directions, _, aNodes := parseDirectionMap(content)
	steps := followGhostDirections(directions, aNodes)
	AssertEquals(t, uint(6), steps, "steps")
}
