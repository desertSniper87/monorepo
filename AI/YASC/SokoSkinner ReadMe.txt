_____________________________________
SokoSkinner Sokoban Skin Collection
Version: 1.0
Author: Brian Damgaard, Denmark


______________
Introduction

This package contains 183 skins by SokoSkinner (Annie Lee) for the Sokoban game. 

All the skins have hand-tailored settings for the best viewing experience in Sokoban YASC. Sokoban YASC version 1.647, or newer, is required for displaying the skins as intended, in particular for getting the correct main menu button colors, and for easy browsing though the skins.

The skins have been formatted so they can be used by all Sokoban clones with support for the so-called "Common Skin Format", which in its basic form (images arranged in a 4 columns x 8 rows grid) was developed by George Petrov for YSokoban. 

There is one caveat though. The skins have Sokoban YASC specific settings, telling the program that the skins contain directional player imagery. If other Sokoban clones don't have that information, they will interpret the directional player imagery as a highligthing animation of the player avatar. This doesn't affect the playability in any way, but the resulting animation is an unconvincing full rotation of the player. 


____________
How to Use

After installation, the skins are located in the folder "{your documents folder}\Sokoban\Sokoban YASC\Skins\Common Skins\SokoSkinner".

In Sokoban YASC, you can select a skin this way from the main window:

* Left-click the "Settings" button (or press F2).

* In the settings window, select "Skins | Load skin..." on the menu (or press F2 again).

* In the skins window, you can browse though the folders and the skins installed on your computer.

Please note the following feature, which may come in handy when you browse through a large skin collection:

* When you have loaded a skin and is back in the main gameplay window, you can browse though the skins without going through all the trouble of first opening the settings window and then the skins window. Instead, you just right-click the "Settings" button (or press F4). This loads the next skin from the current skin folder. Combining the right-click or the F4 key with the shift key loads the previous skin in the folder.

A tip: When you browse though these skins in Sokoban YASC for the mere enjoyment of their graphical qualities, it's best that you pick a small level and then enlarge the main window as much as possible, without maximizing the window. The reason is that due to long-forgotten and deeply buried dependencies of various program parts, the program must always "un-maximize" the main window before the program loads a new skin.


_______
Links

Sokoskinner original website:
http://sokoskinneriam1.webs.com/

Sokoban YASC original website:
https://sourceforge.net/projects/sokobanyasc/

YSokoban original website:
http://ygp.orgfree.com/

Sokoban.dk original website:
http://sokoban.dk/


_______________
Release Notes


__________________
1.0 (2017-10-13)

The skins have been converted from their original Sokoban++ format to the so-called "common skin format" with images arranged in a 4 columns x 8 rows grid.

Many of the original skins were composed of differently sized images. In these cases, the images have been scaled up to the largest size, excluding the size of the skin background image. The scaling was made by means of one of the best existing scaling algorithms, Lanczos 3, to ensure that the scaled versions look as good as the original versions.

The skin background images were typically a little larger than the skin tiles proper. These background images have been scaled down to fit the skin tile size. Also here, the scaling was made by means of the Lanczos 3 algorithm to ensure that the scaled versions look as good as the original versions.

About 20 of the skins have been corrected. 5 or 6 skins had simple drawing errors in form of pixels and lines which should not be there. For the other skins, it was a matter of objects (players and boxes) not having an exact copy of the floor or the goal as background. This makes it impossible for a Sokoban program to separate the object from the background, and without a clean separation, a program cannot "lift up" the object and move it around on the board smoothly, one pixel at a time.

6 skin titles had spelling errors: "Broken Hearted", "Luck of The Irish", "Monsters Inc Sully", "Phantom Of the Opera, "Posion Ivy", and "Tug Boat Captian". They have been renamed to "Brokenhearted", "Luck of the Irish", "Monsters Inc. Sully", "Phantom of the Opera, "Poison Ivy", and "Tug Boat Captain". The skin name "Knights Quest" was probably a spelling error too, even though "knights" technically can be read as a correct plural form here. But the name "Knights Quest" was inconsistent with the names of the other skins in the "Knight's Quest" series. For that reason, "Knights Quest" has been renamed to "Knight's Quest 4", the next free number in the series.

Images for "immovable box on floor" and "immovable box on goal" have been generated programmatically for all the skins. (It's the two topmost images in the fourth column.) The original box shadows were drawn so accurately with darker shades of the background colors that it was possible to "lift up" a box without its shadow. Then a gray version of the box could be made and drawn back on top of the original image. This way, the grayed box retains its colored, original shadow. To finish it off, edge smoothing was applied to the border between the grayed box and the underlying background. The "Dragon Eye" skin is a fine example of how good the produced image can be.

Each individual skin has been equipped with hand-tailored settings for the best viewing experience in Sokoban YASC. The most noticeable choice is the color and the transparency of the main menu buttons and the status bar. A more subtle choice is the color difference threshold for the object/background separation, something of utmost importance for a successful move animation.
