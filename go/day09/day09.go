package main

import (
	"fmt"
	"strings"

	lib "github.com/charlesbourget/aoc-lib"
)

func main() {
	input, err := lib.Read("../inputs/day09/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

type void struct{}

type Point struct {
	X       int
	Y       int
	History map[string]void
}

func Part1(input []string) int {
	return runSimulation(input, 2)
}

func Part2(input []string) int {
	return runSimulation(input, 10)
}

func runSimulation(input []string, numPoints int) int {
	points := make([]*Point, numPoints)
	for i := 0; i < numPoints; i++ {
		points[i] = &Point{
			0,
			0,
			make(map[string]void),
		}
	}

	for _, v := range input {
		direction, i := lib.Unpack(strings.Split(v, " "))
		increment := lib.ToInt(i)
		for j := 0; j < increment; j++ {
			switch direction {
			case "U":
				points[0].Y++
			case "D":
				points[0].Y--
			case "L":
				points[0].X--
			case "R":
				points[0].X++
			}
			for i := 1; i < numPoints; i++ {
				points[i].follow(points[i-1])
			}
		}
	}

	return len(points[numPoints-1].History)
}

func (p *Point) follow(lead *Point) {
	dist_x := lead.X - p.X
	dist_y := lead.Y - p.Y
	if lib.Abs(dist_x) == 2 && dist_y == 0 {
		if dist_x > 0 {
			p.X++
		} else {
			p.X--
		}
	} else if lib.Abs(dist_y) == 2 && dist_x == 0 {
		if dist_y > 0 {
			p.Y++
		} else {
			p.Y--
		}
	} else if (lib.Abs(dist_y) == 2 && lib.Abs(dist_x) == 1 || lib.Abs(dist_x) == 2) || (lib.Abs(dist_x) == 2 && lib.Abs(dist_y) == 1 || lib.Abs(dist_y) == 2) {
		if dist_x > 0 {
			p.X++
		} else {
			p.X--
		}

		if dist_y > 0 {
			p.Y++
		} else {
			p.Y--
		}
	}

	p.History[fmt.Sprintf("%d,%d", p.X, p.Y)] = void{}
}
