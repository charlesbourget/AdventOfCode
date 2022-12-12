package main

import (
	"fmt"
	"math"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.ReadInt("../../inputs/day01/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []int) int {
	count := 0
	for i := 0; i < len(input)-1; i++ {
		if input[i] < input[i+1] {
			count++
		}
	}
	return count
}

func Part2(input []int) int {
	count := 0
	lastSum := math.MaxInt
	for i := 0; i < len(input)-2; i++ {
		if input[i+2] > lastSum {
			count++
		}
		lastSum = input[i]
	}

	return count
}
