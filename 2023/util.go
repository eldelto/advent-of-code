package twentytwentythree

import (
	"embed"
	"fmt"
	"path/filepath"
	"strconv"
	"strings"

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
		return nil, nil
	}

	return strings.Split(string(content), "\n"), nil
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

func Sum[T constraints.Integer](l []T) T {
	var sum T
	for _, n := range l {
		sum += n
	}

	return sum
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
