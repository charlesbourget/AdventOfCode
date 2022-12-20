package main

import (
	"fmt"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day06/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	return findMarker(input[0], 4)
}

func Part2(input []string) int {
	return findMarker(input[0], 14)
}

func findMarker(input string, length int) int {
	chars := []rune(input)

	for i := 0; i < len(chars)-1; i++ {
		set := createSet(chars[i : i+length])

		if len(set) == length {
			return i + length
		}
	}

	return 0
}

func createSet(chars []rune) map[rune]bool {
	set := make(map[rune]bool)

	for _, v := range chars {
		set[v] = true
	}

	return set
}
