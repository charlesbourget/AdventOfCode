package main

import (
	"fmt"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

func main() {
	input, err := lib.Read("../inputs/day18/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	points := parsePoints(input)

	count := 0
	for p := range points {
		count += isEmptyNeighbours(p, points)
	}

	return count
}

func Part2(input []string) int {
	points := parsePoints(input)

	x, y, z := findBoundingBox(points)

	return bfs(x, y, z, points)
}

type Point struct {
	x int
	y int
	z int
}

func parsePoints(input []string) map[Point]bool {
	points := make(map[Point]bool)
	for _, v := range input {
		coordinates := strings.Split(v, ",")
		point := Point{
			x: lib.ToInt(coordinates[0]),
			y: lib.ToInt(coordinates[1]),
			z: lib.ToInt(coordinates[2]),
		}
		points[point] = true
	}

	return points
}

func isEmptyNeighbours(v Point, points map[Point]bool) int {
	count := 0

	for i := -1; i <= 1; i += 2 {
		np := Point{v.x + i, v.y, v.z}
		if !points[np] {
			count++
		}

		np = Point{v.x, v.y + i, v.z}
		if !points[np] {
			count++
		}

		np = Point{v.x, v.y, v.z + i}
		if !points[np] {
			count++
		}
	}

	return count
}

func findBoundingBox(points map[Point]bool) (x int, y int, z int) {
	for p := range points {
		if p.x > x {
			x = p.x
		}

		if p.y > y {
			y = p.y
		}

		if p.z > z {
			z = p.z
		}
	}

	return x, y, z
}

func bfs(x int, y int, z int, points map[Point]bool) int {
	queue := make([]Point, 0)
	visited := make(map[Point]bool)
	startPoint := Point{x: -1, y: -1, z: -1}
	visited[startPoint] = true
	queue = lib.Enqueue(queue, startPoint)

	count := 0

	for len(queue) != 0 {
		newQueue, s := lib.Pop(queue)
		queue = newQueue

		newPoints, newCount := classifyNeighbours(x, y, z, s, visited, points)
		queue = append(queue, newPoints...)
		count += newCount
	}

	return count
}

func classifyNeighbours(x int, y int, z int, p Point, visited map[Point]bool, points map[Point]bool) ([]Point, int) {
	count := 0
	newPoints := make([]Point, 0)

	if p.x >= 0 {
		np := Point{p.x - 1, p.y, p.z}
		if points[np] {
			count++
		} else if !visited[np] {
			visited[np] = true
			newPoints = append(newPoints, np)
		}
	}

	if p.x <= x {
		np := Point{p.x + 1, p.y, p.z}
		if points[np] {
			count++
		} else if !visited[np] {
			visited[np] = true
			newPoints = append(newPoints, np)
		}
	}

	if p.y >= 0 {
		np := Point{p.x, p.y - 1, p.z}
		if points[np] {
			count++
		} else if !visited[np] {
			visited[np] = true
			newPoints = append(newPoints, np)
		}
	}

	if p.y <= y {
		np := Point{p.x, p.y + 1, p.z}
		if points[np] {
			count++
		} else if !visited[np] {
			visited[np] = true
			newPoints = append(newPoints, np)
		}
	}

	if p.z >= 0 {
		np := Point{p.x, p.y, p.z - 1}
		if points[np] {
			count++
		} else if !visited[np] {
			visited[np] = true
			newPoints = append(newPoints, np)
		}
	}

	if p.z <= z {
		np := Point{p.x, p.y, p.z + 1}
		if points[np] {
			count++
		} else if !visited[np] {
			visited[np] = true
			newPoints = append(newPoints, np)
		}
	}

	return newPoints, count
}
