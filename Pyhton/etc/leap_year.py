while(True):
    year = int(input())
    if (year%100 != 0 and year%4 ==0):
        print("Leap")
    else:
        print("Non leap")
