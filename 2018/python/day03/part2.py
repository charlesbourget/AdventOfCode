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
    return s

newLine = None

w, h = 1000, 1000
tab = [[0 for x in range(w)] for y in range(h)] 

for l in lines:
    l = parseData(l)
    for i in range(0, int(l[3])):
        for j in range(0, int(l[4])):
            hor = int(l[1]) + i
            vert = int(l[2]) + j
            if (tab[hor][vert] != 0 or tab[hor][vert] == -1):
                tab[hor][vert] = -1
            else:
                tab[hor][vert] = l[0]

for l in lines:
    l = parseData(l)
    l.append('Y')
    for i in range(0, int(l[3])):
        for j in range(0, int(l[4])):
            hor = int(l[1]) + i
            vert = int(l[2]) + j
            if (tab[hor][vert] == -1):
                l[5] = 'N'

    if (l[5] == 'Y'):
        print(l[0])