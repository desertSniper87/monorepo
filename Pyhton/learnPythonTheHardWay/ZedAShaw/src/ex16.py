from sys import argv
from pip._vendor.distlib.compat import raw_input

script, filename = argv

print ("We are going to erase file %r" % filename)

raw_input("?\nPlease press enter")

print("Opening the file")
target = open (filename, "a+")

print ("Truncating the file. Goodbye")
target.truncate();

print("Please enter three lines")
line_1 = raw_input("Line1:")
line_2 = raw_input("Line2:")
line_3 = raw_input("Line3:")

print("Now writing to the file")

target.write (line_1)
target.write("\n")
target.write (line_2)
target.write("\n")
target.write (line_3)
target.write("\n")

print ("And finally we close it")
target.close()