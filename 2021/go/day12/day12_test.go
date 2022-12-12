package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	// Mini test input
	input, err := lib.Read("../../inputs/day12/input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 10
	result := Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d on input 0, want %d, error`, result, expected)
	}

	// Small test input
	input, err = lib.Read("../../inputs/day12/input1.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected = 19
	result = Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d on input 1, want %d, error`, result, expected)
	}

	// Large test input
	input, err = lib.Read("input2.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected = 226
	result = Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d on input 2, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	// Mini test input
	input, err := lib.Read("input.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected := 36
	result := Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d on input 0, want %d, error`, result, expected)
	}

	// Small test input
	input, err = lib.Read("input1.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected = 103
	result = Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d on input 1, want %d, error`, result, expected)
	}

	// Large test input
	input, err = lib.Read("input2.test")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	expected = 3509
	result = Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %d on input 2, want %d, error`, result, expected)
	}
}
