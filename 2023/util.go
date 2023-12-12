package twentytwentythree

import (
	"bufio"
	"embed"
	"fmt"
	"io"
	"math"
	"path/filepath"
	"strconv"
	"strings"
	"unicode/utf8"

	"golang.org/x/exp/constraints"
)

//go:embed inputs
var inputsFS embed.FS

func InputsForDay(day uint) (string, string, string) {
	return fmt.Sprintf("%02d.txt", day),
		fmt.Sprintf("%02d-part1-test.txt", day),
		fmt.Sprintf("%02d-part2-test.txt", day)
}

func InputToString(name string) (string, error) {
	content, err := inputsFS.ReadFile(filepath.Join("inputs", name))
	if err != nil {
		return "", fmt.Errorf("failed to read file '%s': %w", name, err)
	}

	return string(content), nil
}

func InputToLines(name string) ([]string, error) {
	content, err := InputToString(name)
	if err != nil {
		return nil, err
	}

	return strings.Split(string(content), "\n"), nil
}

func ForEach[A any](a []A, f func(a A)) {
	for i := range a {
		f(a[i])
	}
}

func Map[A, B any](a []A, f func(a A) B) []B {
	result := make([]B, len(a))
	for i := range a {
		result[i] = f(a[i])
	}

	return result
}

func MapWithErr[A, B any](a []A, f func(a A) (B, error)) ([]B, error) {
	result := make([]B, len(a))
	for i := range a {
		b, err := f(a[i])
		if err != nil {
			return nil, err
		}
		result[i] = b
	}

	return result, nil
}

func Filter[A any](a []A, f func(a A) bool) []A {
	result := []A{}
	for _, element := range a {
		if f(element) {
			result = append(result, element)
		}
	}

	return result
}

func Sum[T constraints.Integer](l []T) T {
	var sum T
	for _, n := range l {
		sum += n
	}

	return sum
}

func Product[T constraints.Integer](l []T) T {
	var sum T = 1
	for _, n := range l {
		sum *= n
	}

	return sum
}

func Max[T constraints.Integer](l []T) T {
	max := l[0]
	for _, n := range l {
		if n > max {
			max = n
		}
	}

	return max
}

func Unique[T comparable](l []T) []T {
	set := map[T]struct{}{}
	for _, entry := range l {
		set[entry] = struct{}{}
	}

	result := []T{}
	for k := range set {
		result = append(result, k)
	}

	return result
}

func ZipMap[A, B, C any](a []A, b []B, f func(a A, b B) C) []C {
	result := make([]C, min(len(a), len(b)))
	for i := range a {
		result[i] = f(a[i], b[i])
	}

	return result
}

func Repeat[T any](l []T, count int) []T {
	res := []T{}
	for i := 0; i < count; i++ {
		res = append(res, l...)
	}

	return res
}

func StringToUInts(str, separator string) ([]uint, error) {
	return StringsToUInts(strings.Split(str, separator))
}

func StringsToUInts(strs []string) ([]uint, error) {
	result := []uint{}
	for i, str := range strs {
		str = strings.TrimSpace(str)
		if str == "" {
			continue
		}

		x, err := strconv.ParseUint(str, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("failed to parse uint at index %d '%s': %w", i, str, err)
		}

		result = append(result, uint(x))
	}

	return result, nil
}

func StringToInts(str, separator string) ([]int, error) {
	return StringsToInts(strings.Split(str, separator))
}

func StringsToInts(strs []string) ([]int, error) {
	result := []int{}
	for i, str := range strs {
		str = strings.TrimSpace(str)
		if str == "" {
			continue
		}

		x, err := strconv.ParseInt(str, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("failed to parse int at index %d '%s': %w", i, str, err)
		}

		result = append(result, int(x))
	}

	return result, nil
}

func DegreeToRadians(value int) float64 {
	return float64(value) * (math.Pi / float64(180))
}

type Vec2 struct {
	X int
	Y int
}

func (v Vec2) Add(o Vec2) Vec2 {
	return Vec2{X: v.X + o.X, Y: v.Y + o.Y}
}

func (v Vec2) Scale(factor int) Vec2 {
	return Vec2{v.X * factor, v.Y * factor}
}

func (v Vec2) Rotate(angle int) Vec2 {
	rad := DegreeToRadians(angle)
	fX := float64(v.X)
	fY := float64(v.Y)

	sin := math.Sin(rad)
	cos := math.Cos(rad)
	newX := fX*cos - fY*sin
	newY := fX*sin + fY*cos

	return Vec2{int(math.Round(newX)), int(math.Round(newY))}
}

type Direction Vec2

var (
	North = Direction{Y: -1}
	East  = Direction{X: 1}
	South = Direction{Y: 1}
	West  = Direction{X: -1}
)

func DirectionFromPositions(src, dst Vec2) Direction {
	switch {
	case src.Y < dst.Y:
		return North
	case src.Y > dst.Y:
		return South
	case src.X < dst.X:
		return West
	case src.X > dst.X:
		return East
	}

	panic("treading in place!")
}

func (d Direction) Right() Direction {
	return Direction(Vec2(d).Rotate(90))
}

func (d Direction) Left() Direction {
	return Direction(Vec2(d).Rotate(-90))
}

func color(msg, colorCode string) string {
	return "\033[" + colorCode + msg + "\033[0m"
}

func Magenta(msg string) string {
	return color(msg, "95m")
}

func Green(msg string) string {
	return color(msg, "92m")
}

func ParseIntoMatrix[T any](r io.Reader,
	f func(r rune, row, column int) (T, error)) ([][]T, error) {
	scanner := bufio.NewScanner(r)
	scanner.Split(bufio.ScanRunes)

	matrix := [][]T{}
	matrix = append(matrix, []T{})

	column := 0
	row := 0
	for scanner.Scan() {
		token := scanner.Text()
		if token == "\n" {
			matrix = append(matrix, []T{})
			row++
			column = 0
			continue
		}

		r, _ := utf8.DecodeRuneInString(token)
		value, err := f(r, row, column)
		if err != nil {
			return nil, fmt.Errorf("failed to parse %q - column=%d row=%d", r, column, row)
		}

		matrix[row] = append(matrix[row], value)
		column++
	}

	return matrix, nil
}

func Abs[T constraints.Integer](x T) T {
	if x < 0 {
		return -x
	}

	return x
}
