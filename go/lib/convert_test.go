package lib

import (
	"testing"
)

func TestToInt(t *testing.T) {
	expected := 5
	result := ToInt("5")
	if result != expected {
		t.Fatalf(`ToInt() = %d, want %d, error`, result, expected)
	}

	result = ToInt("5 ")
	if result != expected {
		t.Fatalf(`ToInt() = %d, want %d, error`, result, expected)
	}

	result = ToInt(" 5 ")
	if result != expected {
		t.Fatalf(`ToInt() = %d, want %d, error`, result, expected)
	}
}

func TestToIntBin(t *testing.T) {
	expected := 4
	result := ToIntBin("100")
	if result != expected {
		t.Fatalf(`ToIntBin() = %d, want %d, error`, result, expected)
	}

	result = ToIntBin("100 ")
	if result != expected {
		t.Fatalf(`ToIntBin() = %d, want %d, error`, result, expected)
	}

	result = ToIntBin(" 100 ")
	if result != expected {
		t.Fatalf(`ToIntBin() = %d, want %d, error`, result, expected)
	}
}
