package main

import (
	"fmt"
	"strings"

	"github.com/RyanCarrier/dijkstra"
	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day15/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	board := parse(input)
	return solve(board)
}

func Part2(input []string) int {
	board := parseExt(input)
	return solve(board)
}

func parse(input []string) (board [][]int) {
	for _, v := range input {
		board = append(board, lib.ToIntSlice(strings.Split(v, "")))
	}
	return
}

func parseExt(input []string) [][]int {
	var tempBoard [][]int
	for _, v := range input {
		line := lib.ToIntSlice(strings.Split(v, ""))
		extLine := make([]int, len(line)*5)
		for i, value := range line {
			lastValue := value
			for j := 0; j < 5; j++ {
				extLine[i+len(line)*j] = lastValue
				if lastValue == 9 {
					lastValue = 1
				} else {
					lastValue++
				}
			}
		}
		tempBoard = append(tempBoard, extLine)
	}
	board := make([][]int, len(tempBoard)*5)
	for i := range board {
		board[i] = make([]int, len(tempBoard[0]))
	}
	for i, v := range tempBoard {
		for j, v2 := range v {
			lastValue := v2
			for k := 0; k < 5; k++ {
				board[i+len(tempBoard)*k][j] = lastValue
				if lastValue == 9 {
					lastValue = 1
				} else {
					lastValue++
				}
			}
		}
	}
	return board
}

// solve Got lazy and used a lib
func solve(board [][]int) int {
	graph := dijkstra.NewGraph()

	for i, row := range board {
		for j := range row {
			graph.AddVertex(i*len(row) + j)
		}
	}

	for i, row := range board {
		for j, value := range row {
			index := i*len(row) + j
			if inbound(i-1, j, board) {
				graph.AddArc((i-1)*len(row)+j, index, int64(value))
			}
			if inbound(i+1, j, board) {
				graph.AddArc((i+1)*len(row)+j, index, int64(value))
			}
			if inbound(i, j-1, board) {
				graph.AddArc((i)*len(row)+j-1, index, int64(value))
			}
			if inbound(i, j+1, board) {
				graph.AddArc((i)*len(row)+j+1, index, int64(value))
			}
		}
	}

	best, _ := graph.Shortest(0, len(board)*len(board[0])-1)

	return int(best.Distance)
}

func inbound(x, y int, board [][]int) bool {
	return !(x < 0 || x >= len(board)) && !(y < 0 || y >= len(board[x]))
}
