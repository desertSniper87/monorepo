import random
import resource, sys
import math

# def rand_median(array):

# def findquicksort(array, high):
    # # print(array)
    # # array = quicksort(array, high, len(array)-1)
    # array = quicksort(array, 0, high)

# def quicksort(array, low, high):
    # """TODO: Docstring for quicksort.
    # Taken from Cormen/Wikipedia

    # :array: unsorted list
    # :low: TODO
    # :high: TODO
    # :returns: TODO

    # """
    # print(array, low, high)
    # if low<high :
        # p = partition(array, low, high)
        # quicksort(array, low, p-1)
        # quicksort(array, p+1, high)

    # return array

def find_dup(array):
    n = math.floor((len(array))/2)
    print(array)
    if len(array)==1:
        return True
    elif len(array)==2:
        if (array[0]!=array[1]):
            return False
        else:
            return True
    else:
        left = array[n:]
        right = array[:n]
        return find_dup(left) or find_dup(right)
   

if __name__ == '__main__':
    # resource.setrlimit(resource.RLIMIT_STACK, (2**29,-1))
    # sys.setrecursionlimit(10**6) 

    f = open('online_dnc_input.txt')
    num_of_tests = int(f.readline())
    for _ in range(num_of_tests):
        n = int(f.readline())
        line = f.readline().rstrip().split(" ")
        array = []
        for i in line:
            array.append(int(i))
        # print(array)
        x = find_dup(array)
        print(x)


