package lib

import (
	"sort"
	"strings"
)

func SortString(in string) string {
	temp := strings.Split(in, "")
	sort.Strings(temp)
	return strings.Join(temp, "")
}
