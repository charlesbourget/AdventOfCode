package main

import (
	"fmt"
	"unicode"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day03/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	result := 0

	for _, v := range input {
		v1 := []rune(v[0 : len(v)/2])
		v2 := []rune(v[len(v)/2:])
		intersect := lib.Intersect(v1, v2)
		for _, v := range intersect {
			result += toCodePoint(v)
			break
		}
	}

	return result
}

func Part2(input []string) int {
	result := 0

	for i := 0; i < len(input)-2; i += 3 {
		v1 := []rune(input[i])
		v2 := []rune(input[i+1])
		v3 := []rune(input[i+2])

		intersect := lib.Intersect(lib.Intersect(v1, v2), v3)
		for _, v := range intersect {
			result += toCodePoint(v)
			break
		}
	}

	return result
}

func toCodePoint(r rune) int {
	if unicode.IsLower(r) {
		return int(r-'a') + 1
	} else {
		return int(r-'A') + 27
	}
}
