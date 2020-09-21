with open("prob-01-input.txt") as file:
    lines = [line.rstrip('\n') for line in file]

result = 0

for line in lines:
    if line[0] == '+':
        result += int(line[1:])
    else:
        result -= int(line[1:])
    print(result)


