package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

type region struct {
	x1, x2, y1, y2 int
}

func main() {
	input, err := lib.Read("../../inputs/day17/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	result := 0
	goal := parse(input[0])

	for i := 1; i < 100; i++ {
		for j := 0; j < 100; j++ {
			successful, maxY := throw(i, j, goal)
			if successful && maxY > result {
				result = maxY
			}
		}
	}

	return result
}

func Part2(input []string) int {
	count := 0
	goal := parse(input[0])

	for i := -500; i < 500; i++ {
		for j := -500; j < 500; j++ {
			successful, _ := throw(i, j, goal)
			if successful {
				count++
			}
		}
	}

	return count
}

func parse(input string) region {
	var goal region
	x, y := lib.Unpack(strings.Split(strings.Split(input, ": ")[1], ", "))
	goal.x1, goal.x2 = lib.UnpackInt(strings.Split(strings.Trim(x, "x="), ".."))
	goal.y1, goal.y2 = lib.UnpackInt(strings.Split(strings.Trim(y, "y="), ".."))
	return goal
}

func throw(i int, j int, goal region) (bool, int) {
	maxY := 0
	x := 0
	y := 0
	for x <= goal.x2 && y >= goal.y1 {
		if x >= goal.x1 && x <= goal.x2 && y <= goal.y2 && y >= goal.y1 {
			return true, maxY
		}

		x += i
		y += j
		if i < 0 {
			i++
		} else if i > 0 {
			i--
		}

		j--

		if y > maxY {
			maxY = y
		}
	}

	return false, 0
}
