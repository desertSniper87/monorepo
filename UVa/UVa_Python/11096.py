if __name__ == '__main__':
    T = int(input())

    possibility = [100][100]
    conn = [100][100]
    # for i in range (0, 100):
    #     new = []
    #     for i in range(0,100):
    #         new.append(0)

    for i in range(0, T):
        R = int(input())
        C = int(input())
        M = int(input())
        N = int(input())
        W = int(input())

        for i in range(0, W):
            x = int(input())
            y = int(input())
            possibility[x][y] = False

    even = 0
    odd = 0
    moves = [
        [M,N],
        [M,-N],
        [-M,N],
        [-M,-N],
        [N,M],
        [N,-M],
        [-M,N],
        [-N,-M]
    ]

    DR = []
    DC = []

    head = tail = 0

    conn[0][0] = True
    for move in moves:
        DR.append(move[0])
        DC.append(move[1])

    size = moves.size

