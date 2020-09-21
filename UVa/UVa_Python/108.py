def kadane(array):
    current_sum = 0
    local_start = 0




if __name__ == '__main__':
    mat_dim = int(input())
    row_num, col_num = mat_dim, mat_dim

    mat = [[0 for x in range (mat_dim)] for y in range (mat_dim) ]

    lines = []
    while True:
        line = str(input())
        type(line)
        if line:
            lines += line.split()
        else:
            break
    for i in range(row_num):
        for j in range(col_num):
            mat[i][j] = lines[i*4+j]

    for i in range(row_num):
        for j in range (col_num):


print (mat)
