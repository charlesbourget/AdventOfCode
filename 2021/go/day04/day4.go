package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day04/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	numbers := parseNumbers(input[0])
	boards := parseBoards(input[1:])

	fmt.Printf("Part 1: %d\n", Part1(numbers, boards))
	fmt.Printf("Part 2: %d\n", Part2(numbers, boards))
}

func Part1(numbers []string, boards []board) int {
	for _, v := range numbers {
		for _, b := range boards {
			findNumber(b, v)
			isWinning := validate(b)
			if isWinning {
				return calculateScore(b, v)
			}
		}
	}

	return 0
}

func Part2(numbers []string, boards []board) int {
	isLast := false
	lastBoard := 0

	for _, v := range numbers {
		for i, b := range boards {
			findNumber(b, v)
			isWinning := false
			if isLast {
				if i == lastBoard {
					isWinning = validate(b)
				}
			} else {
				if !b.hasWon {
					isWinning = validate(b)
				}
			}
			if isWinning {
				if !isLast {
					boards[i].hasWon = true
					result, index := findLastBoard(boards)

					if result {
						isLast = true
						lastBoard = index
					}
				} else {
					return calculateScore(boards[lastBoard], v)
				}
			}
		}
	}

	return 0
}

type board struct {
	rows   [][]string
	hasWon bool
}

func parseNumbers(input string) []string {
	return strings.Split(input, ",")
}

func parseBoards(input []string) (boards []board) {
	var curBoard board
	re := regexp.MustCompile(" +")
	for i, v := range input {
		if v == "" {
			if i != 0 {
				boards = append(boards, curBoard)
			}
			curBoard = board{}
		} else {
			var curRow []string
			row := re.Split(strings.TrimSpace(v), -1)
			for _, r := range row {
				curRow = append(curRow, r)
			}
			curBoard.rows = append(curBoard.rows, curRow)
		}
	}
	boards = append(boards, curBoard)

	return
}

func findNumber(b board, n string) {
	for i, row := range b.rows {
		for j, v := range row {
			if v == n {
				b.rows[i][j] = ""
				return
			}
		}
	}
}

func validate(b board) bool {
	if validateRows(b) {
		return true
	}

	if validateColumns(b) {
		return true
	}

	return false
}

func validateRows(b board) bool {
	for _, r := range b.rows {
		isValid := true
		for _, v := range r {
			if v != "" {
				isValid = false
			}
		}
		if isValid {
			return true
		}
	}
	return false
}

func validateColumns(b board) bool {
	for c := 0; c < len(b.rows[0]); c++ {
		isValid := true
		for r := 0; r < len(b.rows); r++ {
			if b.rows[r][c] != "" {
				isValid = false
			}
		}
		if isValid {
			return true
		}
	}

	return false
}

func calculateScore(b board, n string) int {
	sum := 0
	for _, r := range b.rows {
		for _, v := range r {
			if v != "" {
				sum += lib.ToInt(v)
			}
		}
	}

	num, _ := strconv.Atoi(n)
	return sum * num
}

func findLastBoard(boards []board) (bool, int) {
	var nonWinnig []int
	for i, b := range boards {
		if !b.hasWon {
			nonWinnig = append(nonWinnig, i)
		}
	}

	if len(nonWinnig) == 1 {
		return true, nonWinnig[0]
	} else {
		return false, 0
	}
}
