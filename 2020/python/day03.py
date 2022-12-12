from input_reader import *

lines = read_input('day03')


def traverse_terrain(terrain, hor, vert):
    tree = 0
    x = 0
    y = 0
    while y < len(terrain):
        if x >= len(terrain[y]):
            x = x - len(terrain[y])

        if terrain[y][x] == '#':
            tree += 1
        x += hor
        y += vert

    return tree


result = traverse_terrain(lines, 3, 1)
print("Part 1 : %d" % result)

result *= traverse_terrain(lines, 1, 1)
result *= traverse_terrain(lines, 5, 1)
result *= traverse_terrain(lines, 7, 1)
result *= traverse_terrain(lines, 1, 2)
print("Part 2 : %d" % result)


