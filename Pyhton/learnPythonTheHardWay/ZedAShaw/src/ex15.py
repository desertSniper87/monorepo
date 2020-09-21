from sys import argv
from pip._vendor.distlib.compat import raw_input

script, filename = argv

text = open(filename)

print("Here is your file %s" % filename)
print(text.read())

print("Type the filename again")
file_again = raw_input("> ")

text_again = open(file_again)
print(text_again.read())