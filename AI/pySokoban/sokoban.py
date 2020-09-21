# Name: pySokoban
# Description: A sokoban implementation using python & pyGame
# Author: Kazantzakis Nikos <kazantzakisnikos@gmail.com>
# Date: 2015
# Last Modified: 31-03-2016
import asyncio
import datetime
import os
import pickle

import pygame
import time
import sys

import SokoMap
from Environment import Environment
from Level import Level
from solver import IDAstar, heuristic, depth_first_search__scan, getFormattedMoves


from settings import DEV_DEBUG


def drawLevel(matrix_to_draw):
    # Load level images
    wall = pygame.image.load(myEnvironment.getPath() + '/themes/' + theme + '/images/wall.png').convert()
    box = pygame.image.load(myEnvironment.getPath() + '/themes/' + theme + '/images/box.png').convert()
    box_on_target = pygame.image.load(
        myEnvironment.getPath() + '/themes/' + theme + '/images/box_on_target.png').convert()
    space = pygame.image.load(myEnvironment.getPath() + '/themes/' + theme + '/images/space.png').convert()
    target = pygame.image.load(myEnvironment.getPath() + '/themes/' + theme + '/images/target.png').convert()
    player = pygame.image.load(myEnvironment.getPath() + '/themes/' + theme + '/images/player.png').convert()

    # If horizontal or vertical resolution is not enough to fit the level images then resize images
    if myLevel.getSize()[0] > myEnvironment.size[0] / 36 or myLevel.getSize()[1] > myEnvironment.size[1] / 36:

        # If level's x size > level's y size then resize according to x axis
        if myLevel.getSize()[0] / myLevel.getSize()[1] >= 1:
            new_image_size = myEnvironment.size[0] / myLevel.getSize()[0]
        # If level's y size > level's x size then resize according to y axis
        else:
            new_image_size = myEnvironment.size[1] / myLevel.getSize()[1]

        # Just to the resize job
        wall = pygame.transform.scale(wall, (new_image_size, new_image_size))
        box = pygame.transform.scale(box, (new_image_size, new_image_size))
        box_on_target = pygame.transform.scale(box_on_target, (new_image_size, new_image_size))
        space = pygame.transform.scale(space, (new_image_size, new_image_size))
        target = pygame.transform.scale(target, (new_image_size, new_image_size))
        player = pygame.transform.scale(player, (new_image_size, new_image_size))

    # Just a Dictionary (associative array in pyhton's lingua) to map images to characters used in level design
    images = {'#': wall, ' ': space, '$': box, '.': target, '@': player, '*': box_on_target}

    # Get image size. Images are always squares so it doesn't care if you get width or height
    box_size = wall.get_width()

    # Iterate all Rows
    for i in range(0, len(matrix_to_draw)):
        # Iterate all columns of the row
        for c in range(0, len(matrix_to_draw[i])):
            myEnvironment.screen.blit(images[matrix_to_draw[i][c]], (c * box_size, i * box_size))

    pygame.display.update()


def movePlayer(direction, myLevel):
    matrix = myLevel.getMatrix()

    myLevel.addToHistory(matrix)

    x = myLevel.getPlayerPosition()[0]
    y = myLevel.getPlayerPosition()[1]

    global target_found

    # print boxes
    # print(myLevel.getBoxes())

    if direction == "L":
        # print("######### Moving Left #########")

        # if is_space

        if matrix[y][x - 1] == " ":
            # print("OK Space Found")
            matrix[y][x - 1] = "@"
            if target_found == True:
                matrix[y][x] = "."
                target_found = False
            else:
                matrix[y][x] = " "

        # if is_box
        elif matrix[y][x - 1] == "$":
            # print("Box Found")
            if matrix[y][x - 2] == " ":
                matrix[y][x - 2] = "$"
                matrix[y][x - 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "
            elif matrix[y][x - 2] == ".":
                matrix[y][x - 2] = "*"
                matrix[y][x - 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "


        # if is_box_on_target
        elif matrix[y][x - 1] == "*":
            #
            # ("Box on target Found")
            if matrix[y][x - 2] == " ":
                matrix[y][x - 2] = "$"
                matrix[y][x - 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

            elif matrix[y][x - 2] == ".":
                matrix[y][x - 2] = "*"
                matrix[y][x - 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

        # if is_target
        elif matrix[y][x - 1] == ".":
            # print("Target Found")
            matrix[y][x - 1] = "@"
            if target_found == True:
                matrix[y][x] = "."
            else:
                matrix[y][x] = " "
            target_found = True

        # else
        else:
            print("There is a wall here")

    elif direction == "R":
        # print("######### Moving Right #########")

        # if is_space
        if matrix[y][x + 1] == " ":
            # print("OK Space Found")
            matrix[y][x + 1] = "@"
            if target_found == True:
                matrix[y][x] = "."
                target_found = False
            else:
                matrix[y][x] = " "

        # if is_box
        elif matrix[y][x + 1] == "$":
            # print("Box Found")
            if matrix[y][x + 2] == " ":
                matrix[y][x + 2] = "$"
                matrix[y][x + 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "

            elif matrix[y][x + 2] == ".":
                matrix[y][x + 2] = "*"
                matrix[y][x + 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "

        # if is_box_on_target
        elif matrix[y][x + 1] == "*":
            # print("Box on target Found")
            if matrix[y][x + 2] == " ":
                matrix[y][x + 2] = "$"
                matrix[y][x + 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

            elif matrix[y][x + 2] == ".":
                matrix[y][x + 2] = "*"
                matrix[y][x + 1] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

        # if is_target
        elif matrix[y][x + 1] == ".":
            # print("Target Found")
            matrix[y][x + 1] = "@"
            if target_found == True:
                matrix[y][x] = "."
            else:
                matrix[y][x] = " "
            target_found = True

        else:
            print("There is a wall here")

    elif direction == "D":
        # print("######### Moving Down #########")

        # if is_space
        if matrix[y + 1][x] == " ":
            # print("OK Space Found")
            matrix[y + 1][x] = "@"
            if target_found == True:
                matrix[y][x] = "."
                target_found = False
            else:
                matrix[y][x] = " "

        # if is_box
        elif matrix[y + 1][x] == "$":
            # print("Box Found")
            if matrix[y + 2][x] == " ":
                matrix[y + 2][x] = "$"
                matrix[y + 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "

            elif matrix[y + 2][x] == ".":
                matrix[y + 2][x] = "*"
                matrix[y + 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "

        # if is_box_on_target
        elif matrix[y + 1][x] == "*":
            # print("Box on target Found")
            if matrix[y + 2][x] == " ":
                matrix[y + 2][x] = "$"
                matrix[y + 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

            elif matrix[y + 2][x] == ".":
                matrix[y + 2][x] = "*"
                matrix[y + 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

        # if is_target
        elif matrix[y + 1][x] == ".":
            # print("Target Found")
            matrix[y + 1][x] = "@"
            if target_found == True:
                matrix[y][x] = "."
            else:
                matrix[y][x] = " "
            target_found = True

        # else
        else:
            print("There is a wall here")

    elif direction == "U":
        # print("######### Moving Up #########")

        # if is_space
        if matrix[y - 1][x] == " ":
            # print("OK Space Found")
            matrix[y - 1][x] = "@"
            if target_found == True:
                matrix[y][x] = "."
                target_found = False
            else:
                matrix[y][x] = " "

        # if is_box
        elif matrix[y - 1][x] == "$":
            # print("Box Found")
            if matrix[y - 2][x] == " ":
                matrix[y - 2][x] = "$"
                matrix[y - 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "

            elif matrix[y - 2][x] == ".":
                matrix[y - 2][x] = "*"
                matrix[y - 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                    target_found = False
                else:
                    matrix[y][x] = " "

        # if is_box_on_target
        elif matrix[y - 1][x] == "*":
            # print("Box on target Found")
            if matrix[y - 2][x] == " ":
                matrix[y - 2][x] = "$"
                matrix[y - 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

            elif matrix[y - 2][x] == ".":
                matrix[y - 2][x] = "*"
                matrix[y - 1][x] = "@"
                if target_found == True:
                    matrix[y][x] = "."
                else:
                    matrix[y][x] = " "
                target_found = True

        # if is_target
        elif matrix[y - 1][x] == ".":
            # print("Target Found")
            matrix[y - 1][x] = "@"
            if target_found == True:
                matrix[y][x] = "."
            else:
                matrix[y][x] = " "
            target_found = True

        # else
        else:
            print("There is a wall here")

    drawLevel(matrix)

    # print("Boxes remaining: " + str(len(myLevel.getBoxes())))

    if len(myLevel.getBoxes()) == 0:
        nextLevel()


def moveFiveSteps(direction, myLevel):
    for i in range(5):
        movePlayer(direction, myLevel)


def nextLevel(skip=0):
    """TODO: Docstring for nextLevel.

    :skip: whether we are skipping the level
    :type skip: bool
    :returns: None

    """
    myEnvironment.screen.fill((221, 213, 172))
    if skip == 0:
        print("Level Completed")
    else:
        print("Skipping the level")
    global current_level
    current_level += 1
    initLevel(level_set, current_level)


def initLevel(level_set, level):
    # Create an instance of this Level
    global myLevel
    myLevel = Level(level_set, level)

    # Draw this level
    drawLevel(myLevel.getMatrix())

    global target_found
    target_found = False

    return myLevel


def savegame(current_level):
    """screenshot

    :return:
    """
    fileName = "savegames/savegame_level_" + str(current_level) + \
               "_time_" + datetime.date.strftime(datetime.datetime.now(), "%Y-%m-%d-%H-%M-%S") + \
               ".pickle"
    print("Saving game")
    with open(fileName, "wb") as f:
        pickle.dump(myLevel, f)


# Create the environment

# Choose a theme
theme = "soft"

# Choose a level set
level_set = "original"

# Set the start Level
current_level = 1

def draw_level_in_pygame():
    # Initialize Level
    global myEnvironment
    myEnvironment = Environment()

    global myLevel
    myLevel = initLevel(level_set, current_level)

target_found = False

async def pygameEventLoop():
    while True:
        for event in pygame.event.get():
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("L", myLevel)
                    else:
                        movePlayer("L", myLevel)

                elif event.key == pygame.K_RIGHT:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("R", myLevel)
                    else:
                        movePlayer("R", myLevel)

                elif event.key == pygame.K_DOWN:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("D", myLevel)
                    else:
                        movePlayer("D", myLevel)

                elif event.key == pygame.K_UP:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("U", myLevel)
                    else:
                        movePlayer("U", myLevel)

                elif event.key == pygame.K_u:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        drawLevel(myLevel.getLastMatrix(5))
                    else:
                        drawLevel(myLevel.getLastMatrix())
                elif event.key == pygame.K_r:
                    initLevel(level_set,current_level)
                elif event.key == pygame.K_s and pygame.key.get_mods() & pygame.KMOD_SHIFT:
                    myEnvironment.saveScreen(current_level)
                elif event.key == pygame.K_w and pygame.key.get_mods() & pygame.KMOD_SHIFT:
                    savegame(current_level)
                elif event.key == pygame.K_n and pygame.key.get_mods() & pygame.KMOD_SHIFT:
                    print (f"Going to level {current_level}")
                    nextLevel(skip=True)

                elif event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    sys.exit()

            elif event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()


def solve():
    sokomap = SokoMap.SokoMap()
    sokomap.readMap(os.path.dirname(os.path.abspath(__file__)) + '/levels/' + level_set + '/level' + str(current_level), )
    solution = IDAstar(sokomap, heuristic)
    if solution is not None:
        draw_level_in_pygame()
        pygameEventLoop()
        draw_moves(solution)


def draw_moves(solution):
    solution.printMap()
    print(solution.getMoveList())
    getFormattedMoves(solution.getMoveList())


    for i in solution.getMoveList():
        time.sleep(0.90)

        if i == (1, 0):
            movePlayer("R", myLevel)

        if i == (0, 1):
            movePlayer("D", myLevel)

        if i == (-1, 0):
            movePlayer("L", myLevel)

        if i == (0, -1):
            movePlayer("U", myLevel)



def play():
    draw_level_in_pygame()

    while True:
        for event in pygame.event.get():
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("L", myLevel)
                    else:
                        movePlayer("L", myLevel)

                elif event.key == pygame.K_RIGHT:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("R", myLevel)
                    else:
                        movePlayer("R", myLevel)

                elif event.key == pygame.K_DOWN:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("D", myLevel)
                    else:
                        movePlayer("D", myLevel)

                elif event.key == pygame.K_UP:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        moveFiveSteps("U", myLevel)
                    else:
                        movePlayer("U", myLevel)

                elif event.key == pygame.K_u:
                    if pygame.key.get_mods() and pygame.KMOD_LSHIFT:
                        drawLevel(myLevel.getLastMatrix(5))
                    else:
                        drawLevel(myLevel.getLastMatrix())
                elif event.key == pygame.K_r:
                    initLevel(level_set,current_level)
                elif event.key == pygame.K_s and pygame.key.get_mods() & pygame.KMOD_SHIFT:
                    myEnvironment.saveScreen(current_level)
                elif event.key == pygame.K_w and pygame.key.get_mods() & pygame.KMOD_SHIFT:
                    savegame(current_level)
                elif event.key == pygame.K_n and pygame.key.get_mods() & pygame.KMOD_SHIFT:
                    print (f"Going to level {current_level}")
                    nextLevel(skip=True)

                elif event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    sys.exit()

            elif event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()




solve()
# play()