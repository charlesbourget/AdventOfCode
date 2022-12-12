package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day02/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	x := 0
	z := 0
	for _, line := range input {
		instruction, valueStr := lib.Unpack(strings.Split(line, " "))
		value := lib.ToInt(valueStr)
		switch instruction {
		case "forward":
			x += value
		case "down":
			z += value
		case "up":
			z -= value
		}
	}
	return z * x
}

func Part2(input []string) int {
	aim := 0
	x := 0
	z := 0
	for _, line := range input {
		instruction, valueStr := lib.Unpack(strings.Split(line, " "))
		value := lib.ToInt(valueStr)
		switch instruction {
		case "forward":
			x += value
			z += aim * value
		case "down":
			aim += value
		case "up":
			aim -= value
		}
	}
	return z * x
}
