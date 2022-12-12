from functools import reduce

from input_reader import *

lines = read_input('day06')


def part1(lines):
    answers = set()
    count = 0
    for line in lines:
        if line == "":
            count += len(answers)
            answers.clear()
        else:
            answers.update(set(line))

    return count + len(answers)


def part2(lines):
    answers = []
    count = 0
    for line in lines:
        if line == "":
            count += len(reduce(lambda x, y: x.intersection(y), answers))
            answers.clear()
        else:
            answers.append(set(line))

    return count + len(reduce(lambda x, y: x.intersection(y), answers))


print("Part 1 : %d" % part1(lines))
print("Part 2 : %d" % part2(lines))
