def read_input(day: str):
    file = open(f"../inputs/{day}/input.txt", 'r')
    lines = file.readlines()

    lines = [n.strip("\n") for n in lines]

    return lines
