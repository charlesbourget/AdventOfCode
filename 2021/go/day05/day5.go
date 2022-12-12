package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day05/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

type line struct {
	x1 int
	y1 int
	x2 int
	y2 int
}

func Part1(input []string) int {
	lines := parse(input, false)
	count := mapLines(lines, false)

	return count
}

func Part2(input []string) int {
	lines := parse(input, true)
	count := mapLines(lines, true)

	return count
}

func parse(input []string, diag bool) (lines []line) {
	for _, v := range input {
		values1, values2 := lib.Unpack(strings.Split(v, " -> "))
		x1, y1 := lib.UnpackInt(strings.Split(values1, ","))
		x2, y2 := lib.UnpackInt(strings.Split(values2, ","))

		lines = append(lines, line{x1, y1, x2, y2})
	}

	return
}

func mapLines(lines []line, diag bool) (count int) {
	array := make([][]uint8, 1000)
	for i := range array {
		array[i] = make([]uint8, 1000)
	}

	for _, l := range lines {
		if l.x1 == l.x2 {
			if l.y1 < l.y2 {
				for i := l.y1; i <= l.y2; i++ {
					array[l.x1][i] += 1
					if array[l.x1][i] == 2 {
						count++
					}
				}
			} else {
				for i := l.y1; i >= l.y2; i-- {
					array[l.x1][i] += 1
					if array[l.x1][i] == 2 {
						count++
					}
				}
			}
		} else if l.y1 == l.y2 {
			if l.x1 < l.x2 {
				for i := l.x1; i <= l.x2; i++ {
					array[i][l.y1] += 1
					if array[i][l.y1] == 2 {
						count++
					}
				}
			} else {
				for i := l.x1; i >= l.x2; i-- {
					array[i][l.y1] += 1
					if array[i][l.y1] == 2 {
						count++
					}
				}
			}
		} else if diag {
			if l.x1 < l.x2 {
				if l.y1 < l.y2 {
					for i := 0; i <= l.x2-l.x1; i++ {
						array[l.x1+i][l.y1+i] += 1
						if array[l.x1+i][l.y1+i] == 2 {
							count++
						}
					}
				} else {
					for i := 0; i <= l.x2-l.x1; i++ {
						array[l.x1+i][l.y1-i] += 1
						if array[l.x1+i][l.y1-i] == 2 {
							count++
						}
					}
				}
			} else {
				if l.y1 < l.y2 {
					for i := 0; i <= l.x1-l.x2; i++ {
						array[l.x1-i][l.y1+i] += 1
						if array[l.x1-i][l.y1+i] == 2 {
							count++
						}
					}
				} else {
					for i := 0; i <= l.x1-l.x2; i++ {
						array[l.x1-i][l.y1-i] += 1
						if array[l.x1-i][l.y1-i] == 2 {
							count++
						}
					}
				}
			}
		}
	}

	return count
}
