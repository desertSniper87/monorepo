import random
# print("Please enter your equation")
x = input()
y = random.randint(-99999,99999)
while (x!=y):
    y = random.randint(-99999,99999)
    print(y)
    if (x==y):
        print ("gotcha!")
