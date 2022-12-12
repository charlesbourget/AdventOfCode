from input_reader import *
import re

lines = read_input('day04')

passport = ''
valid_passport = 0
valid_passport_values = 0

regex_part_1 = "(byr|iyr|eyr|hgt|hcl|ecl|pid)"
regex_part_2 = "(byr:(19[2-9][0-9]|200[0-2]) |iyr:(20[1][0-9]|2020) |" \
               "eyr:(20[2][0-9]|2030) |hgt:((1[5][0-9]|1[6-8][0-9]|19[0-3])cm|(59|[6][0-9]|[7][0-6])in) |" \
               "hcl:(#[0-9a-f]{6}) |ecl:(amb|blu|brn|gry|grn|hzl|oth) |" \
               "pid:([0-9]{9}) )"

for line in lines:
    if line == '':
        if len(re.findall(regex_part_1, passport)) == 7:
            valid_passport += 1
            if len(re.findall(regex_part_2, passport)) == 7:
                valid_passport_values += 1
        passport = ''
    else:
        passport += line + " "

# Account for last lines
if len(re.findall(regex_part_1, passport)) == 7:
    valid_passport += 1
    if len(re.findall(regex_part_2, passport)) == 7:
        valid_passport_values += 1

print("Part 1 : %d" % valid_passport)
print("Part 2 : %d" % valid_passport_values)
