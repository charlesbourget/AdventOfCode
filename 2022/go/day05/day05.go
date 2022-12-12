package main

import (
	"fmt"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day01/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %s\n", Part1(input))
	fmt.Printf("Part 2: %s\n", Part2(input))
}

func Part1(input []string) string {
	result := ""

	return result
}

func Part2(input []string) string {
	result := ""

	return result
}

func ParseStacks(input []string) [][]rune {
	return [][]rune{}
}
