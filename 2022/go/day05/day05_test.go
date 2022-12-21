package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day05/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	expected := "CMZ"
	result := Part1(input, 3)
	if result != expected {
		t.Fatalf(`Part1() = %s, want %s, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day05/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	expected := "MCD"
	result := Part2(input, 3)
	if result != expected {
		t.Fatalf(`Part2() = %s, want %s, error`, result, expected)
	}
}
