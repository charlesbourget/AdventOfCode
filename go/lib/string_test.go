package lib

import "testing"

func TestSortString(t *testing.T) {
	expected := "abcd"
	result := SortString("bdac")
	if result != expected {
		t.Fatalf(`Abs() = %s, want %s, error`, result, expected)
	}
}
