# from math import floor
import math

# def merge_sort(array):
    # # print(array)
    # p = 0
    # r = len(array)
    # q = math.floor((p+r)/2)

    # merge(array[:q])
    # merge(array[q:])

    # print(p, q, r)

def merge_sort(array):
    if len(array)==1:
        return array
    p = 0
    r = len(array)
    q = math.floor((p+r)/2)

    left = merge_sort(array[:q])
    right = merge_sort(array[q:])

    res = []
    print("left, right: ", left, right)
    i, j = 0, 0

    while(i<len(left) and j<len(right)):
        if left[i]>right[j]:
            res.append(right[j])
            j += 1
        else: 
            res.append(left[i])
            i += 1
    if (i<len(left)):
        res.extend(left[i:])
    if (j<len(right)):
        res.extend(right[j:])
    # i += 1
            
    # res.extend(left)
    # res.extend(right)
    # print("left, right: ", left, right)

    print("res: ", res)
    return res
    # return merge(left, right)



    # print(p, q, r)



    
if __name__ == '__main__':
    # resource.setrlimit(resource.RLIMIT_STACK, (2**29,-1))
    # sys.setrecursionlimit(10**6) 

    f = open('merge_sort_input.txt')
    num_of_tests = int(f.readline())
    for _ in range(num_of_tests):
        n = int(f.readline())
        line = f.readline().rstrip().split(" ")
        array = []
        for i in line:
            array.append(int(i))

        print("Doing merge sort on array:")
        print(array)
        print()
        merge_sort(array)
        print ("-"*40)

