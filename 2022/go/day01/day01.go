package main

import (
	"fmt"
	"sort"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day01/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	result := calculate_cals_per_elf(input)[0]

	return result
}

func Part2(input []string) int {
	result := lib.Sum(calculate_cals_per_elf(input)[:3])

	return result
}

func calculate_cals_per_elf(input []string) []int {
	elfs := []int{}
	current := 0

	for _, v := range input {
		if v == "" {
			elfs = append(elfs, current)
			current = 0
		} else {
			current += lib.ToInt(v)
		}
	}

	elfs = append(elfs, current)
	sort.Sort(sort.Reverse(sort.IntSlice(elfs)))

	return elfs
}
