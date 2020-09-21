; -- UnpackZipAndRarFiles.iss --

; INNO Setup Script File
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=Sokoban YASC - Unpack Zip and Rar Files
AppVerName=Sokoban YASC - Unpack Zip and Rar Files
AppCopyright=Copyright © 2008 by Brian Damgaard
DefaultDirName={pf}\BDSokobanYASC
DefaultGroupName=Sokoban
; uncomment the following line if you want your installation to run on NT 3.51 too.
; MinVersion=4,3.51
;MessagesFile=Sokoban.isl
AppMutex=BDSokobanYASCMutex,Global\BDSokobanYASCMutex
DirExistsWarning=no
DisableStartupPrompt=yes
DisableDirPage=yes
DisableProgramGroupPage=yes
DisableFinishedPage=no
WizardStyle=modern
WindowVisible=no
SetupIconFile=Setup.ico
;PrivilegesRequired=admin

[Files]
Source: "unrar.exe"; DestDir: "{app}"
Source: "unzip.exe"; DestDir: "{app}"

