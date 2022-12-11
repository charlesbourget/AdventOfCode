package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day09/input.test")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	var expected int = 13
	var result int = Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day09/input.2.test")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	var expected int = 36
	var result int = Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
