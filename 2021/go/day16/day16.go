package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/charlesbourget/aoc-lib/lib"
)

type packet struct {
	version, typeId int
	value           int
	subPacket       []packet
}

func main() {
	input, err := lib.Read("../../inputs/day16/input.txt")
	if err != nil {
		fmt.Println("Error while reading input. ", err)
		return
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int {
	message := toBitSlice(strings.Split(input[0], ""))
	parsedMessage, _ := parseMessage(message)
	count := countVersions(parsedMessage)
	return count
}

func Part2(input []string) int {
	message := toBitSlice(strings.Split(input[0], ""))
	parsedMessage, _ := parseMessage(message)
	return calculateMessage(parsedMessage)
}

func toBitSlice(input []string) (message []string) {
	for _, v := range input {
		num, _ := strconv.ParseInt(v, 16, 0)
		for _, n := range strings.Split(fmt.Sprintf("%04b", num), "") {
			message = append(message, n)
		}
	}

	return
}

func parseMessage(message []string) (packet, []string) {
	var p packet
	p.version = parseVersion(message[:3])
	message = message[3:]
	p.typeId = parseTypeId(message[:3])
	message = message[3:]
	switch p.typeId {
	case 4:
		binValue := ""
		continueReading := true
		for continueReading {
			subMessage := message[:5]
			message = message[5:]

			if subMessage[0] == "0" {
				continueReading = false
			}
			binValue += strings.Join(subMessage[1:], "")
		}
		p.value = lib.ToIntBin(binValue)
	default:
		lengthTypeId := message[0]
		message = message[1:]
		switch lengthTypeId {
		case "0":
			lengthSubPackets := lib.ToIntBin(strings.Join(message[:15], ""))
			message = message[15:]
			subMessage := message[:lengthSubPackets]
			message = message[lengthSubPackets:]
			for len(subMessage) > 0 {
				var subPacket packet
				subPacket, subMessage = parseMessage(subMessage)
				p.subPacket = append(p.subPacket, subPacket)
			}
		case "1":
			numSubPackets := lib.ToIntBin(strings.Join(message[:11], ""))
			message = message[11:]
			for i := 0; i < numSubPackets; i++ {
				var subPacket packet
				subPacket, message = parseMessage(message)
				p.subPacket = append(p.subPacket, subPacket)
			}
		}
	}

	return p, message
}

func parseVersion(message []string) int {
	return lib.ToIntBin(strings.Join(message, ""))
}

func parseTypeId(message []string) int {
	return lib.ToIntBin(strings.Join(message, ""))
}

func countVersions(p packet) (count int) {
	count += p.version
	for _, v := range p.subPacket {
		count += countVersions(v)
	}

	return
}

func calculateMessage(p packet) int {
	switch p.typeId {
	case 0:
		sum := 0
		for _, v := range p.subPacket {
			sum += calculateMessage(v)
		}
		return sum
	case 1:
		product := 1
		for _, v := range p.subPacket {
			product *= calculateMessage(v)
		}
		return product
	case 2:
		min := math.MaxInt
		for _, v := range p.subPacket {
			min = lib.MinInt(min, calculateMessage(v))
		}
		return min
	case 3:
		max := math.MinInt
		for _, v := range p.subPacket {
			max = lib.MaxInt(max, calculateMessage(v))
		}
		return max
	case 4:
		return p.value
	case 5:
		if calculateMessage(p.subPacket[0]) > calculateMessage(p.subPacket[1]) {
			return 1
		} else {
			return 0
		}
	case 6:
		if calculateMessage(p.subPacket[0]) < calculateMessage(p.subPacket[1]) {
			return 1
		} else {
			return 0
		}
	case 7:
		if calculateMessage(p.subPacket[0]) == calculateMessage(p.subPacket[1]) {
			return 1
		} else {
			return 0
		}
	}

	panic("Not entered in switch/case...")
	return 0
}
