====================================================
Sokoban YASC - Yet Another Sokoban Clone
Version 1.654
Copyright (c) 2001-2019 by Brian Damgaard, Denmark
E-mail: BrianDamgaard@jubii.dk
====================================================

Sokoban(r) Registered Trademark of Falcon Co., Ltd., Japan
Sokoban Copyright (c) 1982-2019 by Hiroyuki Imabayashi, Japan
Sokoban Copyright (c) 1989, 1990, 2001-2019 by Falcon Co., Ltd., Japan

Sokoban YASC Copyright (c) 2001-2019 by Brian Damgaard, Denmark


License
--------

Sokoban YASC - Yet Another Sokoban Clone
Copyright (c) 2001-2019 by Brian Damgaard, Denmark

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.


Credits
--------
Please note that the links mentioned in this section are the original ones and may not exist anymore.

Standard Sokoban skin and application icons:
Copyright (c) 1997-2000 by Games 4 Brains.
Original website: http://www.games4brains.de

Additional Sokoban skins and illustrations:
Copyright (c) by Gerry Wiseman.

"KSokoban" skin:
Copyright (c) by Anders Widell.
Original website: http://hem.passagen.se/awl/ksokoban

"Macintosh" skin:
Copyright (c) by Scott Lindhurst.
Original website: http:sneezingtiger.com/sokoban/

Built-in level collection "Crazy Monk":
Copyright (c) by Jean-Pierre Martel and Matthias Meger.
Original level e-mail address: sokomonk@laposte.net

Built-in level collection "du Peloux":
Copyright (c) by Aymeric du Peloux.
Original level website: http://membres.lycos.fr/nabokos

Built-in level collection ""GRIGoRusha":
Copyright (c) by Evgeny Grigoriev.
Original level website: http://grigr.narod.ru

Built-in level collection "Haywood":
Copyright (c) by Lee J Haywood.
Original level website: http://leehaywood.org/games/crossoban/

Built-in level collection "Holland":
Copyright (c) by David Holland.
Original level website: http://www.clickfest88.freeserve.co.uk/

Built-in level collection "Skinner":
Copyright (c) by David W. Skinner.
Original level website: http://users.bentonrea.com/~sasquatch/sokoban/

Built-in level collection "Sladkey":
Copyright (c) by Rick Sladkey.
Original level website: http://home.comcast.net/~jrsladkey/sokoban/index.htm

Built-in level collection "YASGen":
Copyright (c) by Brian Damgaard.
Original level website: https://sourceforge.net/projects/sokobanyasc/

Built-in level collection "Yoshio":
Copyright (c) by Yoshio Murase.
Original level website: http://www.ne.jp/asahi/ai/yoshio/sokoban/

"Mandala" eyecandy:
Copyright (c) by Anthony Steele.
Original website: http://users.iafrica.com/a/as/asteele/mandala/

PNG Image loader source code:
Copyright (c) by Michael Vinther.
Original website: http://sourceforge.net/projects/imagefilelib/ 

Sokoban YASC, YASGen, YASO, and YASS source code:
Copyright (c) by Brian Damgaard.
Original level website: https://sourceforge.net/projects/sokobanyasc/


Acknowledgments
----------------

I would like to thank all who have contributed to the program, e.g., by sending comments, suggestions, and bug-reports. I am particularly indebted to:

* David Holland, for sharing a lot of insight into how a good Sokoban program should work.

* Fred Minklei, for so many good ideas and suggestions about game features and user-interface and for graphical contributions.

* Games 4 Brains, for letting me use their excellent Sokoban game graphics.

* Gerry Wiseman, for allowing the distribution of his excellent skins and illustrations under the GPL license.

* John Polhemus, for his great help with the English texts and for writing an exceptionally good skin tutorial.

* Joris Wit, for fruitful comments and suggestions concerning the file format.

* Lee J Haywood, for sharing a lot of ideas and for a long-standing correspondence on Sokoban issues.

* Matthias Meger, for a fruitful exchange of ideas about Sokoban related algorithms, e.g., solvers and deadlock detection.

* Raymond Groenestein, for his great help with the English texts.

* Sebastien Gouezel, for inventing the "Vicinity search" optimization method, and generously sharing information on the method and its implementation. By sharing his ideas and insights on the subject, he has made a significant and lasting contribution to the Sokoban game itself, transcending the implementation of the algorithm in the YASO optimizer.


Trademarks
-----------

Company names, brand names and product names are trademarks or registered trademarks of their respective holders.


======================
Technical Information
======================

The program is written in Delphi version 4, and as such it uses 8 bit Windows Ansi code text. New versions of Delphi are built from the ground up using Unicode text. That's a natural evolution, but since Delphi does not offer backward compatibility, the program can only be compiled with old Delphi versions, from before the change to Unicode.


Compiling the Program
----------------------

To compile the program, you need to build the resource-file 'Res.res' first:
1.	The resource-file description is 'Res.rc'.
2.	Build 'Res.res' using a resource-compiler, such as
	Delphi's BRCC32.exe.

You can use the bat-file 'Res.bat' to do that, but of course, you must first edit it to match the directories where Delphi and Sokoban YASC are installed on your computer.

Sokoban YASC is distributed as a compressed exe-file, so when you compile it yourself, the new exe-file will be about 3-4 times larger. This is normal, and not a result of any wrong compiler-settings from your side.

For easy distribution, all data used by the program are included in the exe-file as resources. First time the program is activated, the files are extracted. THUS, IS YOU CHANGE ANY OF THESE FILES, SUCH AS THE HELP-FILE 'Sokoban.rtf', YOU MUST REBUILD THE RESOURCE-FILE BEFORE COMPILING THE PROGRAM, OTHERWISE YOUR CHANGES MAY BE LOST, WHEN YOU RUN THE PROGRAM.


The Code
---------

Please note that the code is not something I am proud of. Most of it is OK, but some modules are somewhat below the standard for contracted code. In particular, the modules "Open1_.pas" and "Tools_.pas" suffer from the way the project has evolved.


=========
Feedback
=========

Comments, suggestions, and bug reports are more than welcome. Please use this e-mail address:

	BrianDamgaard@jubii.dk

Happy Sokoban'ing!
