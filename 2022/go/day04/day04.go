package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day04/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	result := 0

	for _, v := range input {
		elf1, elf2 := parseAssignements(v)

		if lib.Contains(elf1, elf2) {
			result += 1
		}
	}

	return result
}

func Part2(input []string) int {
	result := 0

	for _, v := range input {
		elf1, elf2 := parseAssignements(v)

		if lib.Overlaps(elf1, elf2) {
			result += 1
		}
	}

	return result
}

func parseAssignements(s string) ([]int, []int) {
	assignements := strings.Split(s, ",")
	min1, max1 := lib.UnpackInt(strings.Split(assignements[0], "-"))
	elf1 := lib.Seq(min1, max1)
	min2, max2 := lib.UnpackInt(strings.Split(assignements[1], "-"))
	elf2 := lib.Seq(min2, max2)

	return elf1, elf2
}
