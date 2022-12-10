package lib

import "testing"

func TestAbs(t *testing.T) {
	expected := 5
	result := Abs(-5)
	if result != expected {
		t.Fatalf(`Abs() = %d, want %d, error`, result, expected)
	}

	result = Abs(5)
	if result != expected {
		t.Fatalf(`Abs() = %d, want %d, error`, result, expected)
	}
}
