package main

import (
	"fmt"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day12/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	maze, start, _ := parseMaze(input)
	finalNode := bfs(maze, start, testP1, testValidNeighboursP1)

	return finalNode.findNumSteps()
}

func Part2(input []string) int {
	maze, _, end := parseMaze(input)
	finalNode := bfs(maze, end, testP2, testValidNeighboursP2)

	return finalNode.findNumSteps()
}

type Square struct {
	X         int
	Y         int
	IsVisited bool
	Value     int
	IsGoal    bool
	Parent    *Square
}

func parseMaze(input []string) ([][]*Square, *Square, *Square) {
	maze := [][]*Square{}
	var start *Square
	var end *Square
	for i := 0; i < len(input); i++ {
		line := []rune(input[i])
		maze = append(maze, []*Square{})
		for j := 0; j < len(line); j++ {
			square := &Square{
				X:         i,
				Y:         j,
				IsVisited: false,
				Value:     int(line[j]),
				IsGoal:    false,
			}

			if line[j] == 'S' {
				start = square
				square.Value = int('a')
			} else if line[j] == 'E' {
				end = square
				square.IsGoal = true
				square.Value = int('z')
			}

			maze[i] = append(maze[i], square)
		}
	}

	return maze, start, end
}

func bfs(maze [][]*Square, start *Square, testEnd func(*Square) bool, testClimbable func(*Square, *Square) bool) *Square {
	queue := make([]*Square, 0)
	start.IsVisited = true
	queue = lib.Enqueue(queue, start)

	for len(queue) != 0 {
		newQueue, s := lib.Pop(queue)
		queue = newQueue
		if testEnd(s) {
			return s
		}

		for _, v := range s.getClimableNeighbours(maze, testClimbable) {
			v.IsVisited = true
			v.Parent = s
			queue = lib.Enqueue(queue, v)
		}
	}

	panic("Could not find solution")
}

func testValidNeighboursP1(s1 *Square, s2 *Square) bool {
	return s2.Value-s1.Value <= 1
}

func testValidNeighboursP2(s1 *Square, s2 *Square) bool {
	return s1.Value-s2.Value == 1 || s2.Value >= s1.Value
}

func testP1(s *Square) bool {
	return s.IsGoal
}

func testP2(s *Square) bool {
	return s.Value == int('a')
}

func (s *Square) getClimableNeighbours(maze [][]*Square, testClimbable func(*Square, *Square) bool) []*Square {
	neighbours := []*Square{}

	// up
	if s.X > 0 && testClimbable(s, maze[s.X-1][s.Y]) && !maze[s.X-1][s.Y].IsVisited {
		neighbours = append(neighbours, maze[s.X-1][s.Y])
	}

	// down
	if s.X < len(maze)-1 && testClimbable(s, maze[s.X+1][s.Y]) && !maze[s.X+1][s.Y].IsVisited {
		neighbours = append(neighbours, maze[s.X+1][s.Y])
	}

	//left
	if s.Y > 0 && testClimbable(s, maze[s.X][s.Y-1]) && !maze[s.X][s.Y-1].IsVisited {
		neighbours = append(neighbours, maze[s.X][s.Y-1])
	}

	// right
	if s.Y < len(maze[0])-1 && testClimbable(s, maze[s.X][s.Y+1]) && !maze[s.X][s.Y+1].IsVisited {
		neighbours = append(neighbours, maze[s.X][s.Y+1])
	}

	return neighbours
}

func (s *Square) findNumSteps() int {
	if s.Parent == nil {
		return 0
	}

	return 1 + s.Parent.findNumSteps()
}
