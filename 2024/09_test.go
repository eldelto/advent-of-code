package twentytwentyfour

import (
	"errors"
	"fmt"
	"strconv"
	"testing"

	"iter"

	. "github.com/eldelto/advent-of-code/2024/testutils"
)

var input09, part1Test09, part2Test09 = InputsForDay(9)

type diskBlock struct {
	free bool
	id int
	size uint
}

func parseFileIDs(input string) []diskBlock {
	result := []diskBlock{}
	var fileID int = 0
	for i := range input {
		str := input[i]

		b, err := strconv.Atoi(string(str))
		if err != nil {
			continue
		}

		if i % 2 == 0 {
			// File
			for i := 0; i < int(b); i++ {
				result = append(result, diskBlock{
					free: false,
					id: fileID,
				})
			}
			fileID++
		} else {
			// Empty space
			for i := 0; i < int(b); i++ {
				result = append(result, diskBlock{
					free: true,
				})
			}
		}
	}

	return result
}

func parseFileIDsIntoFreeList(input string) *freeList {
	result := newFreeList()
	var fileID int = 0
	for i := range input {
		str := input[i]

		b, err := strconv.Atoi(string(str))
		if err != nil {
			continue
		}

		if i % 2 == 0 {
			// File
			if int(b) > 0 {
			result.Append(diskBlock{
				free: false,
				id: fileID,
				size: uint(b),
			})
			}
			fileID++
		} else {
			// Empty space
			if int(b) > 0 {
			result.Append(diskBlock{
				free: true,
				size: uint(b),
			})
			}
		}
	}

	return result
}

func defragDiskBlocks(blocks []diskBlock) {
	head := 0
	tail := len(blocks) - 1
	free := diskBlock{free:true}
	for head < tail {
		for !blocks[head].free {
			head++
		}
		for blocks[tail].free {
			tail--
		}

		if head >= tail {
			break
		}

		blocks[head] = blocks[tail]
		blocks[tail] = free
	}
}

type linkedList[T any] struct {
	next *linkedList[T]
	previous *linkedList[T]
	value T
}

func NewLinkedList[T any](value T) *linkedList[T] {
	return &linkedList[T]{
		value: value,
	}
}

func (l *linkedList[T]) NodeAt(index int) *linkedList[T] {
	node := l
	for i := 0; i < index; i++ {
		node = node.next
	}

	return node
}

func (l *linkedList[T]) At(index int) T {
	return l.NodeAt(index).value
}

func (l *linkedList[T]) Last() *linkedList[T] {
	node := l
	for ;node.next != nil; node = node.next {
	}

	return node
}

func (l *linkedList[T]) Len() int {
	i := 0
	node := l
	for ;node.next != nil; node = node.next {
		i++
	}

	return i
}

func (l *linkedList[T]) InsertAt(value T, index int) {
	before := l.NodeAt(index-1)
	after := before.next
	newNode := &linkedList[T]{
		next : after,
		previous: before,
		value: value,
	}
	before.next = newNode
	after.previous = newNode
}

func (l *linkedList[T]) Append(value T) *linkedList[T] {
	if l == nil {
		return &linkedList[T]{
			value: value,
		}
	}

	before := l.Last()
	newNode := linkedList[T]{
		previous: before,
		value: value,
	}
	before.next = &newNode

	return l
}

func (l *linkedList[T]) All() iter.Seq[T] {
    return func(yield func(T) bool) {
node := l
        for ; node.next != nil; node = node.next {
            if !yield(node.value) {
                return
            }
        }
		yield(node.value)
    }
}

type freeList struct {
	head *linkedList[diskBlock]
	nextFreeBlock *linkedList[diskBlock]
}

func newFreeList() *freeList {
	return &freeList{}
}

func (l *freeList) Append(block diskBlock) {
	l.head = l.head.Append(block)
}

func allocateAndSplit(node *linkedList[diskBlock], block diskBlock) error {
	if !node.value.free {
		return errors.New("disk block is already allocated")
	}

	if node.value.size < block.size {
		return errors.New("free disk block is too small")
	}
	
	if node.value.size == block.size {
		node.value = block
		return nil
	}

	remainingSpace := diskBlock {
		free: true,
			size: node.value.size - block.size,
		}
	remainingSpaceNode := &linkedList[diskBlock]{
		previous: node,
		next: node.next,
		value: remainingSpace,
	}
	node.next = remainingSpaceNode
	node.value = block

	nextNode :=remainingSpaceNode.next 
	if nextNode!= nil {
		nextNode.previous = remainingSpaceNode
	}

	return nil
}

func (l *freeList) Allocate(block diskBlock) (bool, error) {
	node := l.nextFreeBlock
	if node == nil {
		node = l.head
	}
	for ; node.next != nil; node = node.next {
		if node.value == block {
			// Already defragmented.
			return false, nil
		}

		if l.nextFreeBlock == nil && node.value.free {
			l.nextFreeBlock = node
		}

		if node.value.free && node.value.size >= block.size {
			err := allocateAndSplit(node, block)
			return true, err
		}
	}

	return false, errors.New("no free space for allocation")
}


func (l *freeList) Checksum() int {
	checksum := 0
	pos := 0
	for block := range l.head.All() {
		for i := 0; i < int(block.size); i++ {
			if pos % 80 == 0 {
				fmt.Println()
			}
			//fmt.Print(block.id)
			if block.free {
			fmt.Print(".")
			} else {
			fmt.Print("X")
			}

			checksum += pos*block.id
			pos++
		}
	}

	return checksum
}

func defragWholeDiskBlocks(l *freeList) error {
	lastAllocation := l.head.Last()
	movedFiles := map[int]struct{}{}

	for i := 0; i < 100000; i++{
		if lastAllocation == nil {
			return nil
		}

		if lastAllocation.value.free {
			lastAllocation = lastAllocation.previous
			continue
		}

		if _, ok := movedFiles[lastAllocation.value.id]; ok {
			// Reached the beginning.
			if lastAllocation.previous == nil {
				return nil
			}

			lastAllocation = lastAllocation.previous
			continue
		}

		moved, err := l.Allocate(lastAllocation.value)
		if err != nil {
			return err
		}

		// Set free
		if moved {
			movedFiles[lastAllocation.value.id] = struct{}{}
			lastAllocation.value.free = true
			lastAllocation.value.id = 0
		} else {
			lastAllocation = lastAllocation.previous
		}
	}

	return nil
}

func diskChecksum(blocks []diskBlock) int {
	checksum := 0
	for i, b := range blocks {
		checksum += i * b.id
	}
	return checksum
}

func Test09Part1Test(t *testing.T) {
	input, err := InputToString(part1Test09)
	AssertNoError(t, err, "InputToString")

	blocks := parseFileIDs(input)
	defragDiskBlocks(blocks)
	checksum := diskChecksum(blocks)

	AssertEquals(t, 1928, checksum, "checksum")
}

func Test09Part1(t *testing.T) {
	input, err := InputToString(input09)
	AssertNoError(t, err, "InputToString")

	blocks := parseFileIDs(input)
	defragDiskBlocks(blocks)
	checksum := diskChecksum(blocks)

	AssertEquals(t, 6200294120911, checksum, "checksum")
}

func Test09Part2Test(t *testing.T) {
	input, err := InputToString(part1Test09)
	AssertNoError(t, err, "InputToString")

	freeList := parseFileIDsIntoFreeList(input)

	defragWholeDiskBlocks(freeList)
	checksum := freeList.Checksum()

	AssertEquals(t, 2858, checksum, "checksum")
}

func Test09Part2(t *testing.T) {
	input, err := InputToString(input09)
	AssertNoError(t, err, "InputToString")

	freeList := parseFileIDsIntoFreeList(input)

	defragWholeDiskBlocks(freeList)
	checksum := freeList.Checksum()

	AssertEquals(t, 6227018762750, checksum, "checksum")
}
