path = '../../inputs/day01/input.txt'

input_file = open(path, 'r')

resultat = 0
listresult = [None]

lines = input_file.readlines()
iteration = 0

while(1):
    for l in lines:
        resultat += int(l)
        iteration += 1
        if (resultat in listresult):
            print('The result is : ')
            print(resultat)
            print('It took :')
            print(iteration)
            exit()
        else:
            listresult.append(resultat)

print (resultat)