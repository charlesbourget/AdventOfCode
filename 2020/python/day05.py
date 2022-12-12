from input_reader import *

lines = read_input('day05')

seatBin = [int(line.translate("".maketrans('FBLR', '0101')), 2) for line in lines]

print("Part 1 : %d" % max(seatBin))
print("Part 2 : %s" % str(set(range(min(seatBin), max(seatBin))) - set(seatBin)).strip("[{}]"))
