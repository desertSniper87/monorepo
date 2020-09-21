x = sorted("This is a test string from Andrew".split(), key= lambda v : v.lower if v.isupper()==True else v)
print(x)
x = sorted("This is a test string from Andrew".split(), key=str.lower)
print(x)

