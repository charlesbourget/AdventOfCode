package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"sort"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day13/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	result := 0
	idx := 1

	for i := 0; i < len(input)-2; i += 3 {
		var list1 []any
		var list2 []any

		json.Unmarshal([]byte(input[i]), &list1)
		json.Unmarshal([]byte(input[i+1]), &list2)

		if compare(list1, list2) {
			result += idx
		}

		idx++
	}

	return result
}

func Part2(input []string) int {
	result := 1

	signals := make([][]any, 0)

	for i := 0; i < len(input)-1; i += 3 {
		signals = append(signals, parseLine(input[i]))
		signals = append(signals, parseLine(input[i+1]))
	}

	signals = append(signals, parseLine("[[2]]"))
	signals = append(signals, parseLine("[[6]]"))

	sort.Slice(signals, func(i, j int) bool {
		return compare(signals[i], signals[j])
	})

	for i, v := range signals {
		s := fmt.Sprint(v)
		if s == "[[2]]" || s == "[[6]]" {
			result *= i + 1
		}
	}

	return result
}

const F_64 = "float64"
const ANY_LIST = "[]interface {}"

func parseLine(s string) []any {
	var o []any

	json.Unmarshal([]byte(s), &o)

	return o
}

func compare(a, b any) bool {
	return compareItems(a, b) <= 0
}

func compareItems(a, b any) int {
	at := reflect.TypeOf(a).String()
	bt := reflect.TypeOf(b).String()

	aValue, _ := a.([]any)
	bValue, _ := b.([]any)

	if at == F_64 && bt == F_64 {
		return int(a.(float64) - b.(float64))
	} else if at == ANY_LIST && bt == F_64 {
		bValue = []any{b}
	} else if at == F_64 && bt == ANY_LIST {
		aValue = []any{a}
	}

	for i := 0; i < len(aValue) && i < len(bValue); i++ {
		if c := compareItems(aValue[i], bValue[i]); c != 0 {
			return c
		}
	}
	return len(aValue) - len(bValue)
}
