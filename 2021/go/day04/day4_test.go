package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day04/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	numbers := parseNumbers(input[0])
	boards := parseBoards(input[1:])

	expected := 4512
	result := Part1(numbers, boards)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day04/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	numbers := parseNumbers(input[0])
	boards := parseBoards(input[1:])

	expected := 1924
	result := Part2(numbers, boards)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
