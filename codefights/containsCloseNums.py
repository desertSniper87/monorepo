def containsCloseNums(nums, k):
    dict = {}
    for i, x in enumerate(nums):
        # print(dict)
        if x in dict:
            if i-dict[x]<=k:
                # print(dict[x]-i)
                # print(x)
                return True
            else:
                dict[x] = i
        else:
            dict[x] = i

    return False



nums = [0, 1, 2, 3, 5, 2]
k = 3
print(containsCloseNums(nums, k))

nums = [0, 1, 2, 3, 5, 2] 
k = 2
print(containsCloseNums(nums, k))
