package main

import (
	"fmt"
	"testing"

	"github.com/charlesbourget/aoc-lib/lib"
)

func TestPart1(t *testing.T) {
	input, err := lib.Read("../../inputs/day10/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	var expected int = 13140
	var result int = Part1(input)
	if result != expected {
		t.Fatalf(`Part1() = %d, want %d, error`, result, expected)
	}
}

func TestPart2(t *testing.T) {
	input, err := lib.Read("../../inputs/day10/input.test")
	if err != nil {
		panic(fmt.Errorf("Error while reading input, %s", err))
	}

	expected := "##..##..##..##..##..##..##..##..##..##..\n" +
		"###...###...###...###...###...###...###.\n" +
		"####....####....####....####....####....\n" +
		"#####.....#####.....#####.....#####.....\n" +
		"######......######......######......####\n" +
		"#######.......#######.......#######.....\n"

	var result string = Part2(input)
	if result != expected {
		t.Fatalf(`Part2() = %s, want %s, error`, result, expected)
	}
}
