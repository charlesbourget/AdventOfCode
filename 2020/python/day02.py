from input_reader import *

lines = read_input('day02')

num_password_1 = 0
num_password_2 = 0

for line in lines:
    values = line.split(" ")
    min_max = values[0].split("-")
    min_time = int(min_max[0])
    max_time = int(min_max[1])
    letter = values[1].strip(':')
    password = values[2]

    letter_count = password.count(letter)

    # Part 1
    if min_time <= letter_count <= max_time:
        num_password_1 += 1

    # Part 2
    if password[min_time - 1] == letter and password[max_time - 1] != letter:
        num_password_2 += 1
    elif password[min_time - 1] != letter and password[max_time - 1] == letter:
        num_password_2 += 1

print("Part 1 : %d" % num_password_1)
print("Part 2 : %d" % num_password_2)
