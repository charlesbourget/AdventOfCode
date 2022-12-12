package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

type instr struct {
	axis string
	pos  int
}

func main() {
	input, err := lib.Read("../../inputs/day13/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input, 894, 1306))
	fmt.Printf("Part 2: \n")
	printBoard(Part2(input, 894, 1306))
}

func Part1(input []string, x int, y int) int {
	board, instructions := parse(input, x, y)

	v := instructions[0]

	if v.axis == "x" {
		for i := 0; i < len(board); i++ {
			for j := v.pos; j < len(board[0]); j++ {
				if board[i][j] == "#" {
					board[i][v.pos-(j-v.pos)] = "#"
					board[i][j] = "."
				}

			}
		}
	} else {
		for i := v.pos; i < len(board); i++ {
			for j := 0; j < len(board[0]); j++ {
				if board[i][j] == "#" {
					board[v.pos-(i-v.pos)][j] = "#"
					board[i][j] = "."
				}
			}
		}
	}

	return countDots(board)
}

func Part2(input []string, x int, y int) [][]string {
	board, instructions := parse(input, x, y)

	for _, v := range instructions {
		if v.axis == "x" {
			for i := 0; i < len(board); i++ {
				for j := v.pos; j < len(board[0]); j++ {
					if board[i][j] == "#" {
						board[i][v.pos-(j-v.pos)] = "#"
						board[i][j] = "."
					}

				}
			}
		} else {
			for i := v.pos; i < len(board); i++ {
				for j := 0; j < len(board[0]); j++ {
					if board[i][j] == "#" {
						board[v.pos-(i-v.pos)][j] = "#"
						board[i][j] = "."
					}
				}
			}
		}
	}

	return board
}

func parse(input []string, lenX int, lenY int) ([][]string, []instr) {
	var instructions []instr

	board := make([][]string, lenX)
	for i := range board {
		board[i] = make([]string, lenY)
	}

	isFolding := false
	for _, v := range input {
		if v == "" {
			isFolding = true
			continue
		}

		if !isFolding {
			x, y := lib.UnpackInt(strings.Split(v, ","))
			board[y][x] = "#"
		} else {
			text, pos := lib.Unpack(strings.Split(v, "="))
			textSplit := strings.Split(text, " ")
			axis := textSplit[2]
			instructions = append(instructions, instr{axis, lib.ToInt(pos)})
		}
	}
	return board, instructions
}

func printBoard(board [][]string) {
	for i := range board {
		for j, v := range board[i] {
			if i <= 5 && j <= 38 {
				if v == "#" {
					fmt.Printf("%s", v)
				} else {
					fmt.Printf(".")
				}
			}

		}
		if i <= 5 {
			fmt.Println()
		}
	}
}

func countDots(board [][]string) (count int) {
	for i := range board {
		for _, v := range board[i] {
			if v == "#" {
				count++
			}
		}
	}

	return
}
