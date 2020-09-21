def areFollowingPatterns(strings, patterns):
    dict = {}
    for i, j in zip(patterns, strings):
        # print(dict.items())
        if i not in dict and j not in dict.values():
            # print (dict)
            # print(i, j)
            dict[i] = j
        
        # elif dict[i] == j: 
            # print (dict)
            # print(i, j)
            # pass
        else:
            try:
                if dict[i]!=j:
                    # print(i, j)
                    # print (dict)
                    return False
            except KeyError:
                return False
            # else:
                # print(i, j)

    print (dict)
    return True

strings = ["cat", "dog", "dog"] 
patterns = ["a", "b", "b"]

# print(areFollowingPatterns(strings, patterns))

strings = ["cat", "dog", "doggy"] 
patterns = ["a", "b", "b"]

# print(areFollowingPatterns(strings, patterns))
strings= ["cat", 
 "dog", 
 "dog"]
patterns= ["a", 
 "b", 
 "c"]

print(areFollowingPatterns(strings, patterns))
