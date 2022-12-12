package main

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day12/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	aL := parse(input)
	count := traverse(aL, "start", []string{}, false, true)
	return count
}

func Part2(input []string) int {
	aL := parse(input)
	count := traverse(aL, "start", []string{}, false, false)
	return count
}

// parse creates the adjacency lists of our graph
func parse(input []string) map[string][]string {
	aL := make(map[string][]string)
	for _, v := range input {
		src, dest := lib.Unpack(strings.Split(v, "-"))
		if dest != "start" {
			_, exists := aL[src]
			if exists {
				aL[src] = append(aL[src], dest)
			} else {
				aL[src] = []string{dest}
			}
		}
		if dest != "end" {
			_, exists := aL[dest]
			if exists {
				aL[dest] = append(aL[dest], src)
			} else {
				aL[dest] = []string{src}
			}
		}
	}

	return aL
}

// traverse recursively traverse the graph
func traverse(aL map[string][]string, currentNode string, visited []string, visitedSmallTwice bool, p1 bool) int {
	if currentNode == "end" {
		return 1
	}

	visited = append(visited, currentNode)
	count := 0
	for _, v := range aL[currentNode] {
		if v != "start" {
			if !(isLower(v) && lib.Include(visited, v)) {
				count += traverse(aL, v, visited, visitedSmallTwice, p1)
			} else if !visitedSmallTwice && !p1 {
				count += traverse(aL, v, visited, true, p1)
			}
		}
	}

	return count
}

func isLower(s string) bool {
	for _, r := range s {
		if !unicode.IsLower(r) && unicode.IsLetter(r) {
			return false
		}
	}
	return true
}
