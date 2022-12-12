package main

import (
	"fmt"
	"math"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day07/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	values := parse(input[0])
	min, max := lib.MinMax(values)

	minFuel := math.MaxInt
	for _, v := range lib.Seq(min, max) {
		fuel := calculateFuelConstant(values, v)
		if fuel < minFuel {
			minFuel = fuel
		}
	}

	return minFuel
}

func Part2(input []string) int {
	values := parse(input[0])
	min, max := lib.MinMax(values)

	minFuel := math.MaxInt
	for _, v := range lib.Seq(min, max) {
		fuel := calculateFuelLinear(values, v)
		if fuel < minFuel {
			minFuel = fuel
		}
	}

	return minFuel
}

func parse(input string) []int {
	return lib.ToIntSlice(strings.Split(input, ","))
}

func calculateFuelConstant(values []int, pos int) (fuel int) {
	for _, v := range values {
		fuel += lib.Abs(v - pos)
	}

	return
}

func calculateFuelLinear(values []int, pos int) (fuel int) {
	for _, v := range values {
		fuel += FuelToMove(lib.Abs(v - pos))
	}

	return
}

func FuelToMove(v int) int {
	return v * (v + 1) / 2
}
