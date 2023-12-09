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
