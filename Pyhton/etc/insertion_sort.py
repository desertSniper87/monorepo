a = [39, 49, 43, 11, 43]
for j in range(2, len(a)):
    key = a[j]
    i = j-1
    while(i>0 and a[i]>key):
        a[i+1] = a[i]
        i = i-1
    a[i+1] = key
    print(a)

# we should repair this code following http://interactivepython.org/courselib/static/pythonds/SortSearch/TheInsertionSort.html 
