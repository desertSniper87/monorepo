@echo off
cls
echo __________________________________________________________________
echo Sokoban YASC
echo Install Portable Version
echo Copyright (c) 2018 by Brian Damgaard, Denmark
echo __________________________________________________________________
echo.

set YASCFileName=Sokoban.exe
set YASCFolder=BDSokobanYASC
set YASCPluginsFolder=Plugins

set WindowsProgramFiles=%programfiles(x86)%
set YASC=%WindowsProgramFiles%\%YASCFolder%\%YASCFileName%
if exist "%YASC%" goto YASCInstalled

set WindowsProgramFiles=%programfiles%
set YASC=%WindowsProgramFiles%\%YASCFolder%\%YASCFileName%
if exist "%YASC%" goto YASCInstalled

set WindowsProgramFiles=%ProgramW6432%
set YASC=%WindowsProgramFiles%\%YASCFolder%\%YASCFileName%
if exist "%YASC%" goto YASCInstalled

echo No YASC installation was found on the computer.
echo.
echo Before a portable version can be installed, 
echo please install YASC using the normal installation procedure.
goto End

:YASCInstalled
echo Enter DRIVE and PATH where you want YASC/portable to be installed.
echo An example: G:\YASC
echo.
set /p InstallPath="Enter DRIVE and PATH, followed by [Enter]: "
if "%InstallPath%"=="" goto Cancel

:Confirm
echo.
echo YASC will be installed in the folder "%InstallPath%".
set /p OK="Is that OK? Please answer y/n and press [Enter]: "
if "%OK%"=="y" goto Install
if "%OK%"=="Y" goto Install
if "%OK%"=="n" goto Cancel
if "%OK%"=="N" goto Cancel
goto Confirm

:Install
echo d | xcopy "%YASC%" "%InstallPath%" /q /y /i
if errorlevel 1 goto Error 
echo d | xcopy "%WindowsProgramFiles%\%YASCFolder%\PNG.dll" "%InstallPath%" /q /y /i
if errorlevel 1 goto Error

set BatFileName=YASC.bat
set BatFileNameAndPath=%InstallPath%\%BatFileName%
set Command=@start /b "" "%%~dp0%YASCfileName%" /documents "%%~dp0"
echo %Command%> "%BatFileNameAndPath%"
if errorlevel 1 goto Error

set YASC=%WindowsProgramFiles%\%YASCFolder%
echo d | xcopy "%YASC%\Plugins\*.dll" "%InstallPath%\Sokoban\Sokoban YASC\Plugins" /q /y /i
if errorlevel 1 goto Error

set ApplicationDataFolder=%InstallPath%\Sokoban\Sokoban YASC
set IniFile=%ApplicationDataFolder%\Sokoban.ini
if exist "%IniFile%" goto RunFirstTimeToInstall
echo [Solver]>> "%IniFile%"
if errorlevel 1 goto Error
echo FileName=%ApplicationDataFolder%\Plugins\YASS.dll>> "%IniFile%"
if errorlevel 1 goto Error
echo [Optimizer]>> "%IniFile%"
if errorlevel 1 goto Error
echo FileName=%ApplicationDataFolder%\Plugins\YASO.dll>> "%IniFile%"
if errorlevel 1 goto Error

:RunFirstTimeToInstall
echo Please wait while YASC/portable is being installed.
@start /b /wait "" "%InstallPath%\Sokoban.exe" /install /documents "%InstallPath%"
if errorlevel 1 goto Error

:Done
echo.
echo YASC/Portable has been installed in the folder "%InstallPath%".
echo To run it, please open the file "%BatFileName%" in that folder.
goto End

:Error
echo.
echo The task failed.

:Cancel
echo.
echo YASC/Portable was not installed. You are welcome to try again later.

:End
echo.
set /p Prompt = "Press [Enter] to exit: "
