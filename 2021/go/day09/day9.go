package main

import (
	"fmt"
	"sort"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

type point struct {
	x int
	y int
}

func main() {
	input, err := lib.Read("../../inputs/day09/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	board := parse(input)
	count := 0
	for i := 1; i < len(board)-1; i++ {
		for j := 1; j < len(board[i])-1; j++ {
			if validateLowest(board, i, j) {
				count += board[i][j] + 1
			}
		}
	}
	return count
}

func Part2(input []string) int {
	board := parse(input)
	var basins []int
	for i := 1; i < len(board)-1; i++ {
		for j := 1; j < len(board[i])-1; j++ {
			if validateLowest(board, i, j) {
				basins = append(basins, flood(board, i, j))
			}
		}
	}
	sort.Ints(basins)
	return lib.Mult(basins[len(basins)-3:])
}

func parse(input []string) [][]int {
	var board [][]int
	board = append(board, createBorderRow(len(input[0])))
	for _, v := range input {
		var line []int
		line = append(line, 9)
		line = append(line, lib.ToIntSlice(strings.Split(v, ""))...)
		line = append(line, 9)
		board = append(board, line)
	}
	board = append(board, createBorderRow(len(input[0])))

	return board
}

func createBorderRow(length int) []int {
	border := make([]int, length+2)
	for i, _ := range border {
		border[i] = 9
	}

	return border
}

func validateLowest(board [][]int, i int, j int) bool {
	val := board[i][j]
	return !(board[i-1][j] < val || board[i][j+1] < val || board[i+1][j] < val || board[i][j-1] < val)
}

func flood(board [][]int, i int, j int) int {
	visited := make([][]bool, len(board))
	for x, _ := range visited {
		visited[x] = make([]bool, len(board[0]))
	}

	var stack []point
	stack = append(stack, point{i, j})

	count := 0
	for len(stack) > 0 {
		v := stack[0]
		stack = stack[1:]

		if visited[v.x][v.y] == true {
			continue
		}

		visited[v.x][v.y] = true
		s, c := neighbors(board, v)
		stack = append(stack, s...)
		count += c
	}

	return count
}

func neighbors(board [][]int, v point) (stack []point, count int) {
	if board[v.x][v.y] == 9 {
		return
	}

	count = 1
	if !(board[v.x-1][v.y] == 9) {
		stack = append(stack, point{v.x - 1, v.y})
	}
	if !(board[v.x][v.y+1] == 9) {
		stack = append(stack, point{v.x, v.y + 1})
	}
	if !(board[v.x+1][v.y] == 9) {
		stack = append(stack, point{v.x + 1, v.y})
	}
	if !(board[v.x][v.y-1] == 9) {
		stack = append(stack, point{v.x, v.y - 1})
	}
	return
}
