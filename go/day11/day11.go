package main

import (
	"fmt"
	"sort"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day11/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	monkeys := parseMonkeys(input)

	for i := 0; i < 20; i++ {
		for _, m := range monkeys {
			for m.inspect(monkeys, true, 1) {
			}
		}
	}

	return findResult(monkeys)
}

func Part2(input []string) int {
	monkeys := parseMonkeys(input)
	modFactor := 1
	for _, v := range monkeys {
		modFactor *= v.mod
	}

	for i := 0; i < 10000; i++ {
		for _, m := range monkeys {
			for m.inspect(monkeys, false, modFactor) {
			}
		}
	}

	return findResult(monkeys)
}

func findResult(monkeys []*Monkey) int {
	sort.Slice(monkeys, func(i, j int) bool {
		return monkeys[i].count > monkeys[j].count
	})

	result := 1
	for _, v := range monkeys[:2] {
		result *= v.count
	}

	return result
}

func parseMonkeys(input []string) []*Monkey {
	monkeys := []*Monkey{}

	for i := 0; i < len(input); i += 7 {
		monkey := &Monkey{}
		monkey.items = parseItems(input[i+1])
		monkey.operation = parseOperation(input[i+2])
		monkey.mod = parseTest(input[i+3])
		monkey.tResult = lib.ToInt(strings.Split(input[i+4], "monkey ")[1])
		monkey.fResult = lib.ToInt(strings.Split(input[i+5], "monkey ")[1])
		monkey.count = 0

		monkeys = append(monkeys, monkey)
	}

	return monkeys
}

func parseItems(input string) []int {
	items := []int{}
	for _, item := range strings.Split(strings.TrimPrefix(input, "  Starting items: "), ", ") {
		items = append(items, lib.ToInt(item))
	}

	return items
}

func parseOperation(input string) func(int) int {
	operators := strings.Split(strings.Split(input, "= ")[1], " ")

	if operators[1] == "*" {
		if operators[2] == "old" {
			return pow
		} else {
			return mult(lib.ToInt(operators[2]))
		}
	} else {
		if operators[2] == "old" {
			return mult(2)
		} else {
			return sum(lib.ToInt(operators[2]))
		}
	}
}

func parseTest(input string) int {
	val := lib.ToInt(strings.Split(input, "by ")[1])
	return val
}

func mult(a int) func(int) int {
	return func(b int) int {
		return a * b
	}

}

func sum(a int) func(int) int {
	return func(b int) int {
		return a + b
	}
}

func pow(a int) int {
	return a * a
}

type Monkey struct {
	items     []int
	operation func(int) int
	test      func(int) bool
	mod       int
	tResult   int
	fResult   int
	count     int
}

func (m *Monkey) popItem() int {
	item := m.items[0]
	m.items = m.items[1:]
	return item
}

func (m *Monkey) pushItem(item int) {
	m.items = append(m.items, item)
}

func (m *Monkey) throwItem(item int, dest *Monkey) {
	dest.pushItem(item)
}

func (m *Monkey) inspect(monkeys []*Monkey, reduceWorry bool, modFactor int) bool {
	if len(m.items) == 0 {
		return false
	}

	item := m.operation(m.popItem())
	if reduceWorry {
		item = item / 3
	} else {
		item = item % modFactor
	}

	if item%m.mod == 0 {
		m.throwItem(item, monkeys[m.tResult])
	} else {
		m.throwItem(item, monkeys[m.fResult])
	}

	m.count++

	return true
}
