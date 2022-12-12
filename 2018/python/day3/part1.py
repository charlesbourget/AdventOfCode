path = '../../inputs/day03/input.txt'
input_file = open(path, 'r')
lines = input_file.readlines()
input_file.close()

def parseData(s):
    s = s.replace(" ", "")
    s = s.replace(",", "@")
    s = s.replace(":", "@")
    s = s.replace("x", "@")
    s = s.replace("\n", "")
    s = s.split('@')
    s.pop(0)
    return s

overlap = 0

w, h = 1000, 1000
tab = [[0 for x in range(w)] for y in range(h)] 

for l in lines:
    l = parseData(l)
    for i in range(0, int(l[2])):
        for j in range(0, int(l[3])):
            hor = int(l[0]) + i
            vert = int(l[1]) + j
            if (tab[hor][vert] == 1 or tab[hor][vert] == 9):
                tab[hor][vert] = 9
            else:
                tab[hor][vert] = 1

for i in range(w):
    for j in range(h):
        if (tab[i][j] == 9):
            overlap+=1

print(overlap)
