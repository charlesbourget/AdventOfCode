package lib

import (
	"strconv"
	"strings"
)

// ToInt Basically ignoring errors
func ToInt(value string) (result int) {
	formattedValue := strings.Trim(value, " ")
	result, err := strconv.Atoi(formattedValue)
	if err != nil {
		panic(err)
	}
	return
}

func ToInt64(value string) (result int64) {
	formattedValue := strings.Trim(value, " ")
	result, err := strconv.ParseInt(formattedValue, 10, 64)
	if err != nil {
		panic(err)
	}
	return
}

// ToIntBin Basically ignoring errors and converting from int64 to int
func ToIntBin(value string) int {
	formattedValue := strings.Trim(value, " ")
	result64, err := strconv.ParseInt(formattedValue, 2, 64)
	if err != nil {
		panic(err)
	}
	return int(result64)
}
