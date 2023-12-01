package twentytwentythree

import (
	"embed"
	"fmt"
	"path/filepath"
	"strings"
)

//go:embed inputs
var inputsFS embed.FS

func InputsForDay(day uint) (string, string, string, string) {
	return fmt.Sprintf("%02d-part1-test.txt", day),
		fmt.Sprintf("%02d-part1.txt", day),
		fmt.Sprintf("%02d-part2-test.txt", day),
		fmt.Sprintf("%02d-part2.txt", day)
}

func InputToLines(name string) ([]string, error) {
	content, err := inputsFS.ReadFile(filepath.Join("inputs", name))
	if err != nil {
		return nil, fmt.Errorf("failed to read file '%s': %w", name, err)
	}

	return strings.Split(string(content), "\n"), nil
}
