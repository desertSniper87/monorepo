import sys

"""Smith-Waterman algorithm to check local alignment
   Some help taken from https://gist.github.com/radaniba?page=5"""


def create_score_matrix(rows, cols):
    score_matrix = [[0 for col in range(cols)] for row in range(rows)]

    max_score = 0
    max_pos   = None
    for i in range(1, rows):
        for j in range(1, cols):
            score = calc_score(score_matrix, i, j)
            if score > max_score:
                max_score = score
                max_pos   = (i, j)

            score_matrix[i][j] = score

    return score_matrix, max_pos

def create_score_matrix_lcs(rows, cols):
    score_matrix = [[0 for col in range(cols)] for row in range(rows)]

    max_score = 0
    max_pos   = None
    for i in range(1, rows):
        for j in range(1, cols):
            score = calc_score(score_matrix, i, j)
            if score > max_score:
                max_score = score
                max_pos   = (i, j)

            score_matrix[i][j] = score

    return score_matrix, max_pos


def calc_score(matrix, x, y):
    similarity = 1
    diag_score = matrix[x - 1][y - 1] + similarity
    up_score   = matrix[x - 1][y] + gap
    left_score = matrix[x][y - 1] + gap

    return max(0, diag_score, up_score, left_score)
    
    
def calc_score_lcs(matrix, x, y):
    similarity = match if seq1[x - 1] == seq2[y - 1] else mismatch

    diag_score = matrix[x - 1][y - 1] 
    up_score   = matrix[x - 1][y] 
    left_score = matrix[x][y - 1] 

    return max(0, diag_score, up_score, left_score)
    
    
def lcs(matrix, start_pos):
    END, DIAG, UP, LEFT = range(4)
    lcs = []
    x, y         = start_pos
    move         = next_move(score_matrix, x, y)
    while move != END:
        if move == DIAG and seq1[x-1]==seq2[y-1]:
            lcs.append(seq1[x-1])
            x -= 1
            y -= 1
        elif move == UP:
            x -= 1
        else:
            y -= 1

        move = next_move(score_matrix, x, y)
    
    #print(seq1[x-1], seq2[y-1])
    if seq1[x-1]==seq2[y-1]: 
        lcs.append(seq1[x-1])
    print()   
    print("Longest Common Subsequence is: ", ''.join(reversed(lcs)))
    print()
        


def traceback(score_matrix, start_pos):
    END, DIAG, UP, LEFT = range(4)
    aligned_seq1 = []
    aligned_seq2 = []
    x, y         = start_pos
    move         = next_move(score_matrix, x, y)
    while move != END:
        if move == DIAG:
            aligned_seq1.append(seq1[x - 1])
            aligned_seq2.append(seq2[y - 1])
            x -= 1
            y -= 1
        elif move == UP:
            aligned_seq1.append(seq1[x - 1])
            aligned_seq2.append('-')
            x -= 1
        else:
            aligned_seq1.append('-')
            aligned_seq2.append(seq2[y - 1])
            y -= 1

        move = next_move(score_matrix, x, y)

    aligned_seq1.append(seq1[x - 1])
    aligned_seq2.append(seq1[y - 1])

    return ''.join(reversed(aligned_seq1)), ''.join(reversed(aligned_seq2))


def next_move(score_matrix, x, y):
    diag = score_matrix[x - 1][y - 1]
    up   = score_matrix[x - 1][y]
    left = score_matrix[x][y - 1]
    if diag >= up and diag >= left:     
        return 1 if diag != 0 else 0    
    elif up > diag and up >= left:      
        return 2 if up != 0 else 0      
    elif left > diag and left > up:
        return 3 if left != 0 else 0    
    else:
        raise ValueError('invalid move during traceback')


def alignment_string(aligned_seq1, aligned_seq2):
    idents, gaps, mismatches = 0, 0, 0
    alignment_string = []
    for base1, base2 in zip(aligned_seq1, aligned_seq2):
        if base1 == base2:
            alignment_string.append('|')
            idents += 1
        elif '-' in (base1, base2):
            alignment_string.append(' ')
            gaps += 1
        else:
            alignment_string.append(':')
            mismatches += 1

    return ''.join(alignment_string), idents, gaps, mismatches


def print_matrix(matrix):

    for row in matrix :
        inv_matrix = [[matrix[j][i] for j in range(len(matrix))] for i in range(len(matrix[0]))]

    for row in inv_matrix:
        for i in row:
            sys.stdout.write(str(i))
            sys.stdout.write("\t")
        print()



if __name__ == '__main__':
    f = open('offline_3_input.txt')
    num_of_tests = int(f.readline())

    while (num_of_tests):
        line = f.readline().rstrip().split(" ")
        match    = int(line[2])
        mismatch = int(line[3])
        gap      = int(line[4])
        seq1     = f.readline().rstrip()
        seq2     = f.readline().rstrip()
        
        print(seq1)
        print(seq2)

        rows = len(seq1) + 1
        cols = len(seq2) + 1

        score_matrix, start_pos = create_score_matrix(rows, cols)
        score_matrix_lcs, start_pos_lcs = create_score_matrix_lcs(rows, cols)

        seq1_aligned, seq2_aligned = traceback(score_matrix, start_pos)
        lcs(score_matrix, start_pos_lcs)

        alignment_str, idents, gaps, mismatches = alignment_string(seq1_aligned, seq2_aligned)
        alength = len(seq1_aligned)
        print_matrix(score_matrix)
        print()
        print(seq1_aligned)
        print(alignment_str)
        print(seq2_aligned)
        print()
        print("= = = = = = = = = = = = = = =")
        print()


        num_of_tests = num_of_tests - 1





