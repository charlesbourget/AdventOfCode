lines = open("../inputs/day04/input.txt").readlines()
count1 = 0
count2 = 0

for line in lines:
    l_elf1, u_elf1, l_elf2, u_elf2 = [int(v) for v in line.replace(",", "-").split("-")]

    assignements1 = set(range(l_elf1, u_elf1 + 1))
    assignements2 = set(range(l_elf2, u_elf2 + 1))

    if assignements1.issubset(assignements2) or assignements2.issubset(assignements1):
        count1 += 1
        count2 += 1
    elif not assignements1.isdisjoint(assignements2):
        count2 += 1

print(f"Part 1: {count1}\n Part2: {count2}")
