# solution = [['O', '0'],
            # ['M', '1'],
            # ['Y', '2'],
            # ['E', '5'],
            # ['N', '6'],
            # ['D', '7'],
            # ['R', '8'],
            # ['S', '9']]

# crypt = ["SEND", "MORE", "MONEY"]
# crypt = ["TEN", "TWO", "ONE"]

# solution = [['O', '1'],
            # ['T', '0'],
            # ['W', '9'],
        # ['E', '5'],
crypt= ["A", 
 "A", 
 "A"]
solution= [["A","0"]]

def find_solution(word, sol_dic):
    code = ''.join(sol_dic[x] for x in word)
    # for i in word:
        # sum += sol_dic[i]

    print(code)
    return code


def isCryptSolution(crypt, solution):
    sol_dic = dict((x, y)for x,y in solution)
    # print(sol_dic)
    for i in crypt:
        if len(i)>14:
            return False
    
    x = find_solution(crypt[0], sol_dic)
    y = find_solution(crypt[1], sol_dic)
    z = find_solution(crypt[2], sol_dic)

    if x[0] == '0' and  x !='0' or y[0]=='0' and  y!='0':
        return False
    elif z[0] == '0' and z!='0':
        return False
    elif int(x) + int(y) == int(z):
        return True
    else: 
        return False


# for i in crypt:
    # for j in i:
        # print(j)

# for i in solution:

    # for j in enumerate(i):
        # print(j)

# for j in enumerate(solution):
    # print(j)
# for x in solution:
    # print(x)

print(isCryptSolution(crypt, solution))
