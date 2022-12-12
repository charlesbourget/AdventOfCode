import collections

path = '../../inputs/day02/input.txt'
input_file = open(path, 'r')
lines = input_file.readlines()
input_file.close()

double = 0
triple = 0

foundDouble = False
foundTriple = False

for l in lines:
    d = collections.defaultdict(int)
    for c in l:
        d[c] += 1

    for c in sorted(d, key=d.get , reverse=True):
        if (d[c] == 2 and not foundDouble):
            double+=1
            foundDouble = True
        elif (d[c] == 3 and not foundTriple):
            triple+=1
            foundTriple = True
    
    foundDouble = False
    foundTriple = False

checksum = double * triple
print(checksum)

    