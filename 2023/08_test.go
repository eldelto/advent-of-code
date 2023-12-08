package twentytwentythree

import (
	"fmt"
	"strings"
	"testing"

	. "github.com/eldelto/advent-of-code/2023/testutils"
)

var input08, part1Test08, part2Test08 = InputsForDay(8)

type directions struct {
	turns string
}

func (d directions) at(offset uint) byte {
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
		takeStep(&node, directions.at(steps))
		steps++
		if node.id == "ZZZ" {
			return steps
		}
	}
}

type cachedDirectionNode struct {
	zNode *directionNode
	steps uint
}

type ghostWalker struct {
	node   *directionNode
	offset uint
}

func newGhostWalker(node *directionNode) ghostWalker {
	return ghostWalker{node: node}
}

func (gw *ghostWalker) walkToNextZNode(directions directions, cache map[string]cachedDirectionNode) {
	cacheEntry, ok := cache[gw.node.id]
	if ok {
		gw.node = cacheEntry.zNode
		gw.offset += cacheEntry.steps
		return
	}

	initialNode := gw.node
	initialOffset := gw.offset
	for {
		takeStep(&gw.node, directions.at(gw.offset))
		gw.offset++
		if gw.node.isZNode {
			cache[initialNode.id] = cachedDirectionNode{zNode: gw.node, steps: gw.offset - initialOffset}
			return
		}
	}
}

func areAtSameOffset(walkers []ghostWalker) (bool, uint) {
	offsetMax := walkers[0].offset
	allMatch := true
	for _, w := range walkers {
		if w.offset != offsetMax {
			allMatch = false
		}

		if w.offset > offsetMax {
			offsetMax = w.offset
		}
	}

	return allMatch, offsetMax
}

func catchUpToOffset(walkers []ghostWalker, directions directions, offsetMax uint, cache map[string]cachedDirectionNode) {
	for i, w := range walkers {
		if w.offset < offsetMax {
			w.walkToNextZNode(directions, cache)
			walkers[i] = w
		}
	}
}

func followGhostDirections(directions directions, walkers []ghostWalker) uint {
	cache := map[string]cachedDirectionNode{}

	var steps uint = 1
	for {
		catchUpToOffset(walkers, directions, steps, cache)
		atSameOffset, offsetMax := areAtSameOffset(walkers)
		steps = offsetMax
		if atSameOffset {
			return steps
		}
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
	walkers := Map(aNodes, newGhostWalker)
	steps := followGhostDirections(directions, walkers)
	AssertEquals(t, uint(6), steps, "steps")
}

func Test08Part2(t *testing.T) {
	t.Skipf("Needs > 1 minute to run")

	content, err := InputToString(input08)
	AssertNoError(t, err, "InputToString")

	directions, _, aNodes := parseDirectionMap(content)
	walkers := Map(aNodes, newGhostWalker)
	steps := followGhostDirections(directions, walkers)
	AssertEquals(t, uint(8811050362409), steps, "steps")
}
