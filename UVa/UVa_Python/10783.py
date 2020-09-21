if __name__ == '__main__':
    #f = open('input10783.txt')
    # number_of_tests = int(f.readline())
    number_of_tests = int(input())

    for case in range (number_of_tests):
        first_integer = int(input())
        second_integer = int(input())
        sum = 0
        for i in range (first_integer, second_integer+1):
            if i%2!=0:
                sum += i

        print ("Case %d: %d"%(case+1, sum))

exit(0)