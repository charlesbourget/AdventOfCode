package lib

import (
	"reflect"
	"testing"
)

func TestSum(t *testing.T) {
	expected := 20
	result := Sum([]int{10, 10})
	if result != expected {
		t.Fatalf(`Sum() = %d, want %d, error`, result, expected)
	}
}

func TestMult(t *testing.T) {
	expected := 100
	result := Mult([]int{10, 10})
	if result != expected {
		t.Fatalf(`Mult() = %d, want %d, error`, result, expected)
	}
}

func TestUnpack(t *testing.T) {
	expected1 := "value1"
	expected2 := "value2"
	result1, result2 := Unpack([]string{"value1", "value2"})
	if result1 != expected1 || result2 != expected2 {
		t.Fatalf(`Unpack() = %s, %s, want %s, %s, error`, result1, result2, expected1, expected2)
	}

	expected1 = ""
	expected2 = ""
	result1, result2 = Unpack([]string{"value1", "value2", "value3"})
	if result1 != expected1 || result2 != expected2 {
		t.Fatalf(`Unpack() = %s, %s, want %s, %s, error`, result1, result2, expected1, expected2)
	}
}

func TestUnpackInt(t *testing.T) {
	expected1 := 100
	expected2 := 42
	result1, result2 := UnpackInt([]string{"100", "42"})
	if result1 != expected1 || result2 != expected2 {
		t.Fatalf(`UnpackInt() = %d, %d, want %d, %d, error`, result1, result2, expected1, expected2)
	}
}

func TestToIntSliceTest(t *testing.T) {
	expected := []int{0, 1, 2, 3}
	result := ToIntSlice([]string{"0", "1", "2", "3"})
	if !reflect.DeepEqual(result, expected) {
		t.Fatalf(`ToIntSlice() = %d, want %d, error`, result, expected)
	}
}

func TestMin(t *testing.T) {
	expected := 0
	result := Min([]int{0, 1, 17, 3})
	if result != expected {
		t.Fatalf(`Min() = %d, want %d error`, result, expected)
	}
}

func TestMax(t *testing.T) {
	expected := 17
	result := Max([]int{0, 1, 17, 3})
	if result != expected {
		t.Fatalf(`Max() = %d, want %d error`, result, expected)
	}
}

func TestMinMax(t *testing.T) {
	expectedMin := 0
	expectedMax := 17
	resultMin, resultMax := MinMax([]int{0, 1, 17, 3})
	if resultMin != expectedMin || resultMax != expectedMax {
		t.Fatalf(`MinMax() = %d, %d, want %d, %d, error`, resultMin, resultMax, expectedMin, expectedMax)
	}
}
