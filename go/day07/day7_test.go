package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc2022/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day07/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	var expected int64 = 95437
	var result int64 = Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day07/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	var expected int64 = 24933642
	var result int64 = Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
