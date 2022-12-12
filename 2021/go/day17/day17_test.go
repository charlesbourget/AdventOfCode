package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day17/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 45
	result := Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day17/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 112
	result := Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
