package main

import (
	"fmt"
	"math"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day11/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	board := parse(input)
	flashes := 0
	for f := 0; f < 100; f++ {
		c := 0
		board, c = runStep(board)
		flashes += c
	}

	return flashes
}

func Part2(input []string) int {
	board := parse(input)
	for f := 1; f < math.MaxInt; f++ {
		c := 0
		board, c = runStep(board)
		if c == 100 {
			return f
		}
	}

	return 0
}

func parse(input []string) [][]int {
	var board [][]int
	for _, v := range input {
		var line []int
		line = append(line, lib.ToIntSlice(strings.Split(v, ""))...)
		board = append(board, line)
	}

	return board
}

func runStep(board [][]int) ([][]int, int) {
	flashes := 0

	board = increaseByOne(board)
	board, flashes = flash(board)
	board = resetBoard(board)

	return board, flashes
}

func increaseByOne(board [][]int) [][]int {
	for i := range board {
		for j := range board[i] {
			board[i][j] += 1
		}
	}

	return board
}

func flash(board [][]int) ([][]int, int) {
	newFlash := -1
	flashes := 0

	// Flash until no new flashes
	for newFlash != 0 {
		board, newFlash = flashBoard(board)
		flashes += newFlash
	}

	return board, flashes
}

func flashBoard(board [][]int) ([][]int, int) {
	flashes := 0
	for i := range board {
		for j := range board[i] {
			v := board[i][j]
			if v == 10 {
				flashes += 1
				board[i][j] = 11
				for ni := i - 1; ni <= i+1; ni++ {
					for nj := j - 1; nj <= j+1; nj++ {
						if ni >= 0 && ni < len(board) && nj >= 0 && nj < len(board[i]) {
							if board[ni][nj] <= 9 {
								board[ni][nj] += 1
							}
						}
					}
				}
			}
		}
	}

	return board, flashes
}

func resetBoard(board [][]int) [][]int {
	for i := range board {
		for j, v := range board[i] {
			if v == 11 {
				board[i][j] = 0
			}
		}
	}

	return board
}
