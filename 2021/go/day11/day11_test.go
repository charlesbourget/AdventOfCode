package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day11/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 1656
	result := Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day11/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 195
	result := Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
