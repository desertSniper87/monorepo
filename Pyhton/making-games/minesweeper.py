import pygame, sys, random
from pygame.locals import *

WINDOWWIDTH = 400
WINDOWHEIGHT = 300

DISPLAYSURF = pygame.display.set_mode((WINDOWWIDTH, WINDOWHEIGHT))
pygame.display.set_caption("Minesweeper")

#          R G B 
GRAY     = (100, 100, 100)
NAVYBLUE = ( 60, 60, 100)
WHITE    = (255, 255, 255)
RED      = (255, 0, 0)
GREEN    = ( 0, 255, 0)
BLUE     = ( 0, 0, 255)
YELLOW   = (255, 255, 0)
ORANGE   = (255, 128, 0)
PURPLE   = (255, 0, 255)
CYAN     = ( 0, 255, 255)

BGCOLOR = NAVYBLUE
# BOXCOLOR = WHITE

BOARDWIDTH = 8
BOARDHEIGHT = 8

XMARGIN = int((WINDOWWIDTH - (BOARDWIDTH * (BOXSIZE + GAPSIZE))) / 2)
YMARGIN = int((WINDOWHEIGHT - (BOARDWIDTH * (BOXSIZE + GAPSIZE))) / 2)

def leftTopCoordsOfBox(boxX, boxY):



def getRandomizedBoard():
    board = []
    for x in range(BOARDWIDTH):
        column = []
        for y in range(BOARDWIDTH):
            # column.append('o')
            board.append(column)

    return board

def getBoxAtPixel(x, y):
    for boxX in range(BOARDWIDTH):
        for boxY in range(BOARDHEIGHT):
            left, top = leftTopCoordsOfBox(boxx, boxy)
            boxRect = pygame.Rect(left, top, BOXSIZE, BOXSIZE)
            if boxRect.collidepoint(x, y):
                return (boxX, boxY)

    return (None, None)

def main():
    global DISPLAYSURF

    mouseX = 0
    mouseY = 0

    mainBoard = getRandomizedBoard()

    while True:
        mouseClicked = False

        DISPLAYSURF.fill(BGCOLOR)

        for event in pygame.event.get():
            if event.type == QUIT:
                pygame.quit()
                sys.exit()
            elif event.type == MOUSEMOTION:
                mouseX, mouseY = event.pos
            elif event.type == MOUSEBUTTONUP:
                mouseX, mouseY = event.pos
                mouseClicked = True

        boxX, boxY = getBoxAtPixel(mouseX, mouseY)

        pygame.display.update()

if __name__ == "__main__":
    main()
