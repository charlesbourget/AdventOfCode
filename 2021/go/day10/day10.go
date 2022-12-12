package main

import (
	"fmt"
	"sort"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day10/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	score, validLines := Part1(input)
	fmt.Printf("Part 1: %d\n", score)
	fmt.Printf("Part 2: %d\n", Part2(validLines))
}

func Part1(input []string) (score int, validLines []string) {
	for _, v := range input {
		isValid, lineScore := isValidLine(v)
		if isValid {
			validLines = append(validLines, v)
		} else {
			score += lineScore
		}
	}

	return
}

func Part2(input []string) int {
	var scores []int
	for _, v := range input {
		lineScore := 0
		isComplete, stack := isCompleteLine(v)
		if !isComplete {
			for i := len(stack) - 1; i >= 0; i-- {
				lineScore = (lineScore * 5) + getIncompleteScore(stack[i])
			}
		}
		scores = append(scores, lineScore)
	}

	sort.Ints(scores)
	return scores[len(scores)/2]
}

func isValidLine(v string) (bool, int) {
	line := strings.Split(v, "")
	var stack []string
	for _, c := range line {
		if isClosingChar(c) {
			n := len(stack) - 1
			last := stack[n]
			if isClosing(last, c) {
				// pop
				stack = stack[:n]
			} else {
				return false, getIncorrectScore(c)
			}
		} else {
			stack = append(stack, c)
		}
	}
	return true, 0
}

func isCompleteLine(v string) (bool, []string) {
	line := strings.Split(v, "")
	var stack []string
	for _, c := range line {
		if isClosingChar(c) {
			n := len(stack) - 1
			last := stack[n]
			if isClosing(last, c) {
				// pop
				stack = stack[:n]
			}
		} else {
			stack = append(stack, c)
		}
	}
	return len(stack) == 0, stack
}

func isClosing(o string, c string) bool {
	opening := []rune(o)[0]
	closing := []rune(c)[0]
	return closing-opening <= 2 && closing-opening > 0
}

func isClosingChar(c string) bool {
	return c == ")" || c == "]" || c == "}" || c == ">"
}

func getIncorrectScore(c string) (score int) {
	switch c {
	case ")":
		score = 3
	case "]":
		score = 57
	case "}":
		score = 1197
	case ">":
		score = 25137
	}
	return
}

func getIncompleteScore(c string) (score int) {
	switch c {
	case "(":
		score = 1
	case "[":
		score = 2
	case "{":
		score = 3
	case "<":
		score = 4
	}
	return
}
