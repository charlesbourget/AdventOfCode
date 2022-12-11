package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day10/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: \n%s\n", Part2(input))
}

func Part1(input []string) int {
	computer := &Computer{
		register:   1,
		clockCycle: 1,
		screen:     [6][40]int{},
		x:          0,
		y:          0,
	}
	computer.process(input)

	return computer.signalStrength
}

func Part2(input []string) string {
	computer := &Computer{
		register:   1,
		clockCycle: 1,
		screen:     [6][40]int{},
		x:          0,
		y:          0,
	}
	computer.process(input)

	return computer.printScreen()
}

type Computer struct {
	clockCycle     int
	register       int
	signalStrength int
	screen         [6][40]int
	x              int
	y              int
}

func (c *Computer) process(input []string) {
	probePoint := [6]int{20, 60, 100, 140, 180, 220}
	c.drawPixel()

	for _, v := range input {
		if v == "noop" {
			c.clockCycle++
		} else {
			_, value := lib.Unpack(strings.Split(v, " "))
			c.clockCycle++
			c.probe(probePoint[:])
			c.drawPixel()

			c.register += lib.ToInt(value)
			c.clockCycle++
		}

		c.probe(probePoint[:])
		c.drawPixel()
	}
}

func (c *Computer) probe(probePoint []int) {
	idx := lib.Index(probePoint, c.clockCycle)
	if idx >= 0 {
		c.signalStrength += c.register * probePoint[idx]
	}
}

func (c *Computer) drawPixel() {
	if c.y >= 6 {
		return
	}

	if c.x >= c.register-1 && c.x <= c.register+1 {
		c.screen[c.y][c.x] = 1
	} else {
		c.screen[c.y][c.x] = 0
	}

	c.x++
	if c.x >= 40 {
		c.x = 0
		c.y++
	}

}

func (c *Computer) printScreen() string {
	screenOutput := ""
	for i := 0; i < 6; i++ {
		for j := 0; j < 40; j++ {
			if c.screen[i][j] == 1 {
				screenOutput += "#"
			} else {
				screenOutput += "."
			}
		}
		screenOutput += "\n"
	}

	return screenOutput
}
