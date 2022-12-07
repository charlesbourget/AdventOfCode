line = open("../inputs/day06/input.txt").read().strip()
def calculate(length):
    return [i+length for i in range(len(line)) if len(set(line[i:i+length]))==length][0]
print(f"Part 1 {calculate(4)}\nPart 2 {calculate(14)}")