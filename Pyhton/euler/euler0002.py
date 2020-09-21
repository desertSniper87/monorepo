x = 1
y = 1
z = 0
summ = 0

while z<4000000:
    z = x + y
    x = y
    y = z

    if z%2==0:
        summ = summ + z
        #print summ
        
print summ
