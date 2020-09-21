from pip._vendor.distlib.compat import raw_input

n = int(raw_input())
m = int(raw_input())
a = int(raw_input())

blockHeight = int(n/a)
blockWidth = int(m/a)

# if bool(n%a) == False:
if bool(n%a):
    blockHeight += 1
#     print (bool(n%a))
    
# if bool(m%a) == False:
if bool(m%a):
    blockWidth += 1
#     print (bool(m%a))
    
block = blockHeight+blockWidth

print(block)

