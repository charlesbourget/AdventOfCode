package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day10/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 26397
	result, _ := Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 288957
	_, lines := Part1(input)
	result := Part2(lines)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
