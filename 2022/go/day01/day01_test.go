package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day01/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	expected := 24_000
	result := Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day01/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	expected := 45_000
	result := Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
