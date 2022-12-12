from input_reader import *


def part1(values):
    for val_1 in values:
        for val_2 in values:
            if val_1 + val_2 == 2020:
                return val_1 * val_2


def part2(values):
    for val_1 in values:
        for val_2 in values:
            for val_3 in values:
                if val_1 + val_2 + val_3 == 2020:
                    return val_1 * val_2 * val_3


lines = read_input('day01')
lines = [int(n) for n in lines]

print("Part 1 : %d" % part1(lines))
print("Part 2 : %d" % part2(lines))
