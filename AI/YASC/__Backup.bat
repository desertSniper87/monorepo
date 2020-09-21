@echo off
del *.~*
del *.dcu
del *.bak
del *.dsm
del *.map
copy *.* bak
del bak\*.cfg
del bak\*.dof
del bak\Sokoban.exe
del bak\"Sokoban YASC File Associations.exe"
del bak\upx.exe
del bak\UnRAR.exe
del bak\Unzip.exe
del bak\private.zip
rem del bak\0*.xsb
rem del bak\b*.xsb
rem del bak\d*.xsb
rem del bak\how*.xsb
rem del bak\k*.xsb
rem del bak\r*.xsb
rem del bak\s*.xsb
rem del bak\t*.xsb
del bak\buttons.bmp
del bak\mandala*.bmp
del "bak\mplayer.bmp"
del "bak\mplayer 01.bmp"
del "bak\mplayer 02.bmp"
del "bak\mplayer 02.jpg"
del bak\playlist*.mpl
rem del bak\res.res
rem del bak\screens.dat
del bak\sokoban.dsk
del bak\*.ini*
del "bak\sokoban 01.bmp"
del bak\*.set
rem del bak\tiles.bmp
del bak\DXSound_.pas
del bak\*.log
del bak\Sokoban.sed
del bak\Internal*.txt

rem copy bak\*.* "d:\Program Files\Compilers\Delphi4\Pg\Sokoban\*.*"
rem copy bak\*.* "d:\Program Files\Compilers\Delphi4\Pg\Sokoban\Bak\*.*"
copy bak\*.* "g:\Program Files\Compilers\Delphi4\Pg\Sokoban\*.*"