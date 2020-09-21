i, j = 0, 0

f = open("input10189.txt")
# field = [][]

column, row = f.readline().rstrip().split(" ")
while(True):
    if column==0 and row==0:
        break
    while(f.readline()):
        for i in range(int(column)):
            for j in range(int(row)):
               c = f.read(1)
               print(c)
                
