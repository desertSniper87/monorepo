if __name__ == '__main__':
    #file = 'input272.txt'

    #with open (file) as lines:
    output = []
    iput = []
    # input = lines.read()
    lines = []
    # while True:
    #     line = input()

    while True:
        try:
            line = input()
            if line:
                lines.append(line)
            else:
                break
        except EOFError:
            break
    iput = '\n'.join(lines)

    flag = 0
    for char in iput:
        if char == '"' and flag == 0:
            output.append('``')
            flag = 1
        elif char == '"' and flag == 1:
            output.append('\'\'')
            flag = 0
        else:
            output.append(char)



print(''.join(output))
exit(0)