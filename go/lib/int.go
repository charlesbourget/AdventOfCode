package lib

import "math"

func Abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func MinInt(x int, y int) int {
	min := math.Min(float64(x), float64(y))
	return int(min)
}

func MaxInt(x int, y int) int {
	max := math.Max(float64(x), float64(y))
	return int(max)
}

func Floor(x int) int {
	return int(math.Floor(float64(x)))
}

func Ceil(x int) int {
	return int(math.Ceil(float64(x)))
}
