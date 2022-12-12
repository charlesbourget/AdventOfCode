package main

import (
	"fmt"
	"math"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day14/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	pairs, letters, instructions := parse(input)
	for i := 0; i < 10; i++ {
		newPairs := make(map[string]int)
		for k, v := range pairs {
			value, exists := instructions[k]
			if exists {
				for j := 0; j < v; j++ {
					letters[value] += 1
					newPair := strings.Split(k, "")[0] + value
					newPairs[newPair] += 1
					newPair = value + strings.Split(k, "")[1]
					newPairs[newPair] += 1
				}
			}
		}
		pairs = newPairs
	}
	return findResult(letters)
}

func Part2(input []string) int {
	pairs, letters, instructions := parse(input)
	for i := 0; i < 40; i++ {
		newPairs := make(map[string]int)
		for k, v := range pairs {
			value, exists := instructions[k]
			if exists {
				letters[value] += v
				newPair := strings.Split(k, "")[0] + value
				newPairs[newPair] += v
				newPair = value + strings.Split(k, "")[1]
				newPairs[newPair] += v
			}
		}
		pairs = newPairs
	}
	return findResult(letters)
}

func parse(input []string) (map[string]int, map[string]int, map[string]string) {
	pairs, letters := findPair(input[0])
	instructions := createInstructions(input[2:])
	return pairs, letters, instructions

}

func createInstructions(input []string) map[string]string {
	instructions := make(map[string]string)
	for _, v := range input {
		pair, add := lib.Unpack(strings.Split(v, " -> "))
		instructions[pair] = add
	}

	return instructions
}

func findPair(str string) (map[string]int, map[string]int) {
	pairs := make(map[string]int)
	letters := make(map[string]int)
	chars := strings.Split(str, "")

	for i := 0; i < len(chars); i++ {
		if i > 0 {
			pairs[chars[i-1]+chars[i]] += 1
		}
		letters[chars[i]] += 1
	}

	return pairs, letters
}

func findResult(letters map[string]int) int {
	max := 0
	min := math.MaxInt
	for _, v := range letters {
		if v > max {
			max = v
		}
		if v < min {
			min = v
		}
	}

	return max - min
}
