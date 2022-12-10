package lib

import (
	"fmt"
	"math"
)

// Sum To sum all values of int slice
func Sum(values []int) (count int) {
	for _, v := range values {
		count += v
	}

	return
}

// Mult To sum all values of int slice
func Mult(values []int) (count int) {
	count = 1
	for _, v := range values {
		count *= v
	}

	return
}

// Unpack To unpack a slice of two elements
func Unpack(values []string) (string, string) {
	if len(values) != 2 {
		fmt.Println("Unpack only accept len=2 this will fail...")
		return "", ""
	}

	return values[0], values[1]
}

// UnpackInt To unpack a slice of two elements and convert them to int
func UnpackInt(values []string) (int, int) {
	value1, value2 := Unpack(values)

	return ToInt(value1), ToInt(value2)
}

// ToIntSlice Convert a slice of string to a slice of int
func ToIntSlice(values []string) []int {
	result := make([]int, len(values))
	for i, v := range values {
		result[i] = ToInt(v)
	}

	return result
}

func Min(values []int) (min int) {
	min, _ = MinMax(values)
	return
}

func Max(values []int) (max int) {
	_, max = MinMax(values)
	return
}

func MinMax(values []int) (min int, max int) {
	min = math.MaxInt
	for _, v := range values {
		if v < min {
			min = v
		}

		if v > max {
			max = v
		}
	}

	return
}

func MinInt64(values []int64) (min int64) {
	min, _ = MinMaxInt64(values)
	return
}

func MaxInt64(values []int64) (max int64) {
	_, max = MinMaxInt64(values)
	return
}

func MinMaxInt64(values []int64) (min int64, max int64) {
	min = math.MaxInt64
	for _, v := range values {
		if v < min {
			min = v
		}

		if v > max {
			max = v
		}
	}

	return
}

// Seq returns a slice of values between min and max
func Seq(min int, max int) []int {
	r := make([]int, 0, max-min+1)
	for v := min; v <= max; v++ {
		r = append(r, v)
	}

	return r
}

// Index returns the first index of the target string t, or -1 if no match is found
func Index(vs []string, t string) int {
	for i, v := range vs {
		if v == t {
			return i
		}
	}
	return -1
}

// Include returns true if the target string t is in the slice
func Include(vs []string, t string) bool {
	return Index(vs, t) >= 0
}

// Any returns true if one of the strings in the slice satisfies the predicate f
func Any(vs []string, f func(string) bool) bool {
	for _, v := range vs {
		if f(v) {
			return true
		}
	}
	return false
}

// All returns true if all of the strings in the slice satisfy the predicate f
func All(vs []string, f func(string) bool) bool {
	for _, v := range vs {
		if !f(v) {
			return false
		}
	}
	return true
}

// Filter returns a new slice containing all strings in the slice that satisfy the predicate f
func Filter(vs []string, f func(string) bool) []string {
	vsf := make([]string, 0)
	for _, v := range vs {
		if f(v) {
			vsf = append(vsf, v)
		}
	}
	return vsf
}

// Map returns a new slice containing the results of applying the function f to each string in the original slice
func Map(vs []string, f func(string) string) []string {
	vsm := make([]string, len(vs))
	for i, v := range vs {
		vsm[i] = f(v)
	}
	return vsm
}
