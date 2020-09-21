#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 06.05.2018
# Last Modified Date: 06.05.2018

from tkinter import Tk, Canvas, Frame, BOTH
from PIL import Image, ImageTk

class Background(Frame):
    canvas = None

    def __init__(self, root):
        super().__init__()

        self.initUI(root)

    def initUI(self, root):
        self.master.title("George")
        width = 200
        height = 300

        canvas = Canvas(root, width=width, height=height)
        canvas.pack()

        canvas.pack(fill=BOTH, expand=1)

        self.canvas = canvas

def draw_pic(canvas, rect_pts, tri_pts, oval_pts, scale, start_x, start_y, rotate, mirror):
    rect_pts[:] = [x*scale for x in rect_pts]
    tri_pts[:] = [x*scale for x in tri_pts]
    oval_pts[:] = [x*scale for x in oval_pts]

    if rotate==True:
        rect_pts[:] = rect_pts[::-1]
        tri_pts[:] = tri_pts[::-1]
        oval_pts[:] =  oval_pts[::-1]

    if mirror==True:
        rect_pts[:] = [x*(-1) for x in rect_pts]
        tri_pts[:] = [x*(-1) for x in tri_pts]
        oval_pts[:] = [x*(-1) for x in oval_pts]

    print(rect_pts)


    rect_pts = transform(rect_pts, start_x, start_y)
    tri_pts = transform(tri_pts, start_x, start_y)
    oval_pts = transform(oval_pts, start_x, start_y)

    canvas.create_rectangle(*rect_pts, fill="red")
    canvas.create_polygon(*tri_pts, fill="green")
    canvas.create_oval(*oval_pts, fill="blue")

# def rotate_and_draw_pic(canvas, rect_pts, tri_pts, oval_pts, scale):
    # canvas.create_rectangle(*rect_pts[::-1], fill="#fb0")
    # canvas.create_polygon(*tri_pts[::-1], fill="green")
    # canvas.create_oval(*oval_pts[::-1], fill="blue")

def transform(pts, x, y):
    for index, i in enumerate(pts):
        if index%2:
            pts[index] += y
        else:
            pts[index] += x

    return pts



def main():
    root = Tk()
    bg = Background(root)

    rect_pts = [30, 10, 100, 80]
    tri_pts = [65, 90, 30, 140, 100, 140]
    oval_pts = [120, 10, 200, 140]

    # draw_pic(canvas, rect_pts, tri_pts, oval_pts, scale, start_x, start_y, rotate, mirror):
    draw_pic(bg.canvas, rect_pts, tri_pts, oval_pts, 1, 0, 0, False, False)
    draw_pic(bg.canvas, rect_pts, tri_pts, oval_pts, 1, 240, -30, True, False)
    draw_pic(bg.canvas, rect_pts, tri_pts, oval_pts, 1, 240, -30, True, True)
    draw_pic(bg.canvas, rect_pts, tri_pts, oval_pts, 1, 400, 100, False, True)

    root.geometry('100x100-100+200')
    root.mainloop()

if __name__ == "__main__":
    main()

