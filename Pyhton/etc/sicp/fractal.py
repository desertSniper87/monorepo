import turtle
import tkinter
from itertools import cycle

# turtle.colormode(255)
myPen = turtle.Turtle()
myPen.ht()
myPen.speed(100)
myPen.pencolor('black')

points = [[-175,-125], [0,175], [175,-125]]

# def triangle(points, depth):
    # myPen.up()
    # myPen.goto(points[0][0], points[0][1])
    # myPen.down()
    # myPen.goto(points[1][0], points[1][1])
    # myPen.goto(points[2][0], points[2][1])
    # myPen.goto(points[0][0], points[0][1])
    # myPen.up()

def rectangle(color, scale, direction):
    myPen.right(direction)
    myPen.pencolor(color)
    myPen.fillcolor(color)
    myPen.down()
    myPen.begin_fill()
    for x in range(4):
        myPen.forward(50*((x%2)+1)*scale)
        myPen.right(90)
    myPen.end_fill()
    myPen.up()
    myPen.backward(10*scale)

def colorful_rectangles(scale):
    rectangle(color="blue", scale=scale, direction=90)
    rectangle(color="red", scale=scale, direction=90)
    rectangle(color="green", scale=scale, direction=90)
    rectangle(color="violet", scale=scale, direction=90)

# def more_colorful_rectangles(scale, distance):
    # scale *= 0.75
    # distance *= 0.75
    # colorful_rectangles(scale)
    # myPen.forward(distance)
    # myPen.right(30)
    # more_colorful_rectangles(scale, distance)

def more_colorful_rectangles(scale, distance):
    # distance *= 0.75
    # colorful_rectangles(scale)

    myPen.forward(distance)
    myPen.right(180)
    color = [ "violet", "red", "green",  "blue"]
    color_cycle = cycle(color)

    for i in range(100):

        rectangle(color=next(color_cycle), scale=scale, direction=0)
        rectangle(color=next(color_cycle), scale=scale, direction=0)
        rectangle(color=next(color_cycle), scale=scale, direction=0)
        rectangle(color=next(color_cycle), scale=scale, direction=0)

        scale *= 0.75

        # myPen.right(90)
    # scale *= 0.75

    # more_colorful_rectangles(scale, distance)





# def getMid(p1, p2):
    # return((p1[0]+p2[0])/2, (p1[1]+p2[1])/2)

# if depth>0:
    # triangle(points[0])

# triangle(points, 4)

# depth, scale, n, d = 200, 1, 1, 1
# while (depth>0):
    # scale *= 0.75 
    # myPen.setpos(0,0)
    # myPen.right(90)
    # myPen.forward(n*n)
    # for _ in range(n):
        # myPen.right(45)
        # myPen.forward(d*50)
        # colorful_rectangles(scale)
        # myPen.backward(d*50)
    # n *= 8
    # depth -= 1 
    # d+=2

more_colorful_rectangles(2, 0)

tkinter.mainloop()

