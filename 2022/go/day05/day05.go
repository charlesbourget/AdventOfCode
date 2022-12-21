package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day05/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %s\n", Part1(input, 9))
	fmt.Printf("Part 2: %s\n", Part2(input, 9))
}

func Part1(input []string, numStacks int) string {
	stacks, startingLine := parseStacks(input, numStacks)

	for i := startingLine; i < len(input); i++ {
		instruction := parseInstruction(input[i])

		for j := 0; j < instruction.number; j++ {
			newStacks, value := Dequeue(stacks[instruction.from])
			stacks[instruction.from] = newStacks

			stacks[instruction.to] = append(stacks[instruction.to], value)
		}
	}

	result := ""
	for _, v := range stacks {
		result += string(v[len(v)-1])
	}

	return result
}

func Part2(input []string, numStacks int) string {
	stacks, startingLine := parseStacks(input, numStacks)

	for i := startingLine; i < len(input); i++ {
		instruction := parseInstruction(input[i])

		newStacks, value := DequeueNum(stacks[instruction.from], instruction.number)
		stacks[instruction.from] = newStacks

		stacks[instruction.to] = append(stacks[instruction.to], value...)
	}

	result := ""
	for _, v := range stacks {
		result += string(v[len(v)-1])
	}

	return result
}

type Instruction struct {
	number int
	from   int
	to     int
}

func parseStacks(input []string, numStacks int) ([][]rune, int) {
	stacks := make([][]rune, numStacks)
	for i := range stacks {
		stacks[i] = make([]rune, 0)
	}

	for i, v := range input {
		if strings.HasPrefix(v, " 1") {
			return stacks, i + 2
		}

		chars := []rune(v)

		for column := 0; column < numStacks; column++ {
			index := column*4 + 1
			data := chars[index]

			if data != ' ' {
				stacks[column] = append([]rune{data}, stacks[column]...)
			}
		}
	}

	return stacks, 0
}

func parseInstruction(line string) *Instruction {
	instructions := strings.Split(line, " ")

	number := lib.ToInt(instructions[1])
	from := lib.ToInt(instructions[3]) - 1
	to := lib.ToInt(instructions[5]) - 1

	return &Instruction{number: number, from: from, to: to}
}

func Dequeue[T any](queue []T) ([]T, T) {
	v := queue[len(queue)-1]
	return queue[:len(queue)-1], v
}

func DequeueNum[T any](queue []T, num int) ([]T, []T) {
	v := queue[len(queue)-num:]
	return queue[:len(queue)-num], v
}
