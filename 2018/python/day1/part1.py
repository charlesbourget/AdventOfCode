path = '../../inputs/day01/input.txt'

input_file = open(path, 'r')

resultat = 0

lines = input_file.readlines()

for l in lines:
    resultat += int(l)

print (resultat)