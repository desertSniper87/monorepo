x = 1
summ = 0

while x<1000:
    if x%3 == 0:
        summ = summ + x
    elif x%5 == 0:
        summ = summ + x

    x = x + 1
        
print summ
