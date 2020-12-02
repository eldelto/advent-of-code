package advent2020

import (
	"strconv"
	"strings"
)

func parseInput(input string) []string {
	return strings.Split(input, "\n")
}

type pwdPolicy interface {
	isValid(string) bool
}

type policyProvider func(string) (pwdPolicy, error)

type passwordPolicy struct {
	char string
	min  int
	max  int
}

func parsePolicy(rawPolicy string) (pwdPolicy, error) {
	parts := strings.Split(rawPolicy, " ")

	rawRange := strings.Split(parts[0], "-")
	min, err := strconv.Atoi(rawRange[0])
	if err != nil {
		return nil, err
	}
	max, err := strconv.Atoi(rawRange[1])
	if err != nil {
		return nil, err
	}

	policy := passwordPolicy{
		char: parts[1],
		min:  min,
		max:  max,
	}

	return &policy, nil
}

func (p *passwordPolicy) isValid(pwd string) bool {
	count := strings.Count(pwd, p.char)
	return count >= p.min && count <= p.max
}

type passwordPolicy2 struct {
	char string
	pos0 int
	pos1 int
}

func parsePolicy2(rawPolicy string) (pwdPolicy, error) {
	parts := strings.Split(rawPolicy, " ")

	rawRange := strings.Split(parts[0], "-")
	min, err := strconv.Atoi(rawRange[0])
	if err != nil {
		return nil, err
	}
	max, err := strconv.Atoi(rawRange[1])
	if err != nil {
		return nil, err
	}

	policy := passwordPolicy2{
		char: parts[1],
		pos0: min - 1,
		pos1: max - 1,
	}

	return &policy, nil
}

func (p *passwordPolicy2) isValid(pwd string) bool {
	if len(pwd) < p.pos0 || len(pwd) < p.pos1 {
		return false
	}

	char0 := string(pwd[p.pos0])
	char1 := string(pwd[p.pos1])

	return (char0 == p.char || char1 == p.char) && char0 != char1
}

func findValidPasswords(input []string, provider policyProvider) (int, error) {
	count := 0
	for _, x := range input {
		parts := strings.Split(x, ":")
		policy, err := provider(parts[0])
		if err != nil {
			return 0, err
		}
		pwd := strings.TrimSpace(parts[1])

		if policy.isValid(pwd) {
			count++
		}
	}

	return count, nil
}
