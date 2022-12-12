path = '../../inputs/day02/input.txt'
input_file = open(path, 'r')
lines = input_file.readlines()
input_file.close()

for l in lines:
    for l2 in lines:
        tab = [i for i in range(len(l)) if l[i] != l2[i]]
        if (len(tab) == 1):
            print(tab)
            print(l)
            print(l2)