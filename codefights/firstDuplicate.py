from collections import Counter
import math

def firstDuplicate(a):
    #pair = [k for (k,v) in Counter(a).items() if v > 1]
    #pair_reversed = [k for (k,v) in Counter(reversed(a)).items() if v > 1]
    keep = {k for (k,v) in Counter(a).items() if v > 1}
    pairs = [x for x in a if x in keep]
    print("pairs: ", pairs)
    # if len(pairs) == 1:
    # print ("pairs[middleIndex]: ", pairs[middleIndex])
    if pairs==[]:
        return -1
    middleIndex = math.floor((len(pairs) - 1)/2)
    print ("middleIndex: ", middleIndex)
    for x in pairs:
        if pairs.count(x) > 2:
            # pairs.remove(x)
            print("pairs: ", pairs)
            indices = [i for i, idx in enumerate(pairs) if idx == x]
            last_index = indices.pop()
            print("last_index: ", last_index)
            print("indices: ", indices)
            del(pairs[last_index])
            print(pairs)
    return pairs[middleIndex]
    
    #print(pair)
    #print(pair_reversed)

# a = [2, 1, 3, 5, 3, 2]
# a = [-1]
# a = [2, 2]
# a = [8, 4, 6, 2, 6, 4, 7, 9, 5, 8]
a = [1, 1, 2, 2, 1]
x = firstDuplicate(a)
print(x)

