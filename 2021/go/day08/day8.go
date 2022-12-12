package main

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../../inputs/day08/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	values := parse(input)
	count := 0
	for _, v := range values {
		results := lib.Filter(v.out, isEasyNumber)
		count += len(results)
	}
	return count
}

func Part2(input []string) int {
	values := parse(input)
	count := 0

	for _, v := range values {
		segments := make([]string, 10)

		// Start our segments slice with all easy number from either input or output
		easy := lib.Filter(v.out, isEasyNumber)
		easy = append(easy, lib.Filter(v.in, isEasyNumber)...)
		for _, e := range easy {
			number := guess(e)
			segments[number] = e
		}

		// Construct our segements slice from input
		segments = decodeInput(v.in, segments)
		// Decode our output with the segments slice
		result := decodeOutput(v.out, segments)
		count += result
	}

	return count
}

type data struct {
	in  []string
	out []string
}

func parse(input []string) []data {
	values := make([]data, len(input))
	for i, v := range input {
		in, out := lib.Unpack(strings.Split(v, " | "))
		values[i] = data{strings.Split(in, " "), strings.Split(out, " ")}
	}

	return values
}

func isEasyNumber(in string) bool {
	return len(in) == 2 || len(in) == 3 || len(in) == 4 || len(in) == 7
}

func guess(in string) int {
	switch len(in) {
	case 2:
		return 1
	case 3:
		return 7
	case 4:
		return 4
	case 7:
		return 8
	}

	return 10
}

func segmentSubstraction(value []string, sub []string) []string {
	var result []string
	for _, s := range value {
		if !lib.Include(sub, s) {
			result = append(result, s)
		}
	}

	return result
}

func decodeInput(in []string, segments []string) []string {
	for _, iv := range in {
		switch len(iv) {
		case 6:
			// This is either a 0, 6 or 9
			// 6 - 1 = 5
			// 0 - 1 = 4
			// 9 - 1 = 4
			// Still in conflict
			// 0 - 4 = 3
			// 9 - 4 = 2
			toGuess := strings.Split(iv, "")
			one := strings.Split(segments[1], "")
			if len(segmentSubstraction(toGuess, one)) == 5 {
				segments[6] = lib.SortString(iv)
			} else {
				four := strings.Split(segments[4], "")
				r := segmentSubstraction(toGuess, four)
				if len(r) == 3 {
					segments[0] = lib.SortString(iv)
				} else {
					segments[9] = lib.SortString(iv)
				}
			}
		case 5:
			// This is either a 2, 3 or 5
			// 2 - 1 = 4
			// 3 - 1 = 3
			// 5 - 1 = 4
			// Still in conflict
			// 2 - 4 = 3
			// 5 - 4 = 2
			toGuess := strings.Split(iv, "")
			one := strings.Split(segments[1], "")
			if len(segmentSubstraction(toGuess, one)) == 3 {
				segments[3] = lib.SortString(iv)
			} else {
				four := strings.Split(segments[4], "")
				r := segmentSubstraction(toGuess, four)
				if len(r) == 3 {
					segments[2] = lib.SortString(iv)
				} else {
					segments[5] = lib.SortString(iv)
				}
			}
		}
	}

	return segments
}

func decodeOutput(out []string, segments []string) int {
	result := ""
	for _, ov := range out {
		if isEasyNumber(ov) {
			result += strconv.Itoa(guess(ov))
		} else {
			for i, s := range segments {
				if reflect.DeepEqual(lib.SortString(ov), s) {
					result += strconv.Itoa(i)
					break
				}
			}
		}
	}

	return lib.ToInt(result)
}
