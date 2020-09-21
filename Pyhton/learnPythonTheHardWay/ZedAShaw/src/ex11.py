from pip._vendor.distlib.compat import raw_input

print ("How old are you?"),
age = raw_input()

print ("So you are %r years old" % age)

name = raw_input ("What is your name\n")
print ("So you are %r" % name)          