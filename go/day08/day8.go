package main

import (
	"fmt"

	lib "github.com/charlesbourget/aoc-lib"
)

func main() {
	input, err := lib.Read("../inputs/day08/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	trees, dx, dy := parseTrees(input)
	count := 0

	for i := 1; i < dx-1; i++ {
		for j := 1; j < dy-1; j++ {
			isVisible, _ := applyToTrees(trees, i, j, isVisible)
			if isVisible {
				count++
			}
		}
	}

	return count
}

func Part2(input []string) int {
	trees, dx, dy := parseTrees(input)
	totals := []int{}

	for i := 1; i < dx-1; i++ {
		for j := 1; j < dy-1; j++ {
			_, total := applyToTrees(trees, i, j, isVisible)
			totals = append(totals, total)
		}
	}

	return lib.Max(totals)
}

func parseTrees(input []string) ([][]int, int, int) {
	dx := len(input[0]) + 2
	dy := len(input) + 2
	trees := make([][]int, dx)
	for i := range trees {
		trees[i] = make([]int, dy)
	}

	for i := 1; i < dx-1; i++ {
		for j := 1; j < dy-1; j++ {
			trees[i][j] = int(input[i-1][j-1] - '0')
		}
	}

	return trees, dx, dy
}

func applyToTrees(trees [][]int, x int, y int, f func(int, int) bool) (bool, int) {
	visible := false
	temp := 0
	total := 1

	for i := x - 1; i >= 0; i-- {
		if f(trees[i][y], trees[x][y]) {
			temp++
			break
		}

		if i == 0 {
			visible = true
		} else {
			temp++
		}

	}

	total, temp = incrementTotal(total, temp)

	for i := x + 1; i < len(trees); i++ {
		if f(trees[i][y], trees[x][y]) {
			temp++
			break
		}

		if i == len(trees[x])-1 {
			visible = true
		} else {
			temp++
		}
	}

	total, temp = incrementTotal(total, temp)

	for i := y + 1; i < len(trees[0]); i++ {
		if f(trees[x][i], trees[x][y]) {
			temp++
			break
		}

		if i == len(trees[x])-1 {
			visible = true
		} else {
			temp++
		}
	}

	total, temp = incrementTotal(total, temp)

	for i := y - 1; i >= 0; i-- {
		if f(trees[x][i], trees[x][y]) {
			temp++
			break
		}

		if i == 0 {
			visible = true
		} else {
			temp++
		}
	}

	total, _ = incrementTotal(total, temp)

	return visible, total
}

func incrementTotal(total int, inc int) (int, int) {
	return total * inc, 0
}

func isVisible(v int, other int) bool {
	return v >= other && v != 0
}
