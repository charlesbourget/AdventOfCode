package main

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day05/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	expected := "CMZ"
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

	expected := "MCD"
	result := Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}

func TestParseStacks(t *testing.T) {
	input, err := lib.Read("../../inputs/day01/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	expected := [][]rune{
		{'Z', 'N'},
		{'M', 'C', 'D'},
		{'P'},
	}
	result := ParseStacks(input)
	if !reflect.DeepEqual(result, expected) {
		t.Fatalf(`Part2() = %d, want %d, error`, result, expected)
	}
}
