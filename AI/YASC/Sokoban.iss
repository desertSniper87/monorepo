; -- Sokoban.iss --

; INNO Setup Script File
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=Sokoban YASC - Yet Another Sokoban Clone
AppVerName=Sokoban YASC
AppCopyright=Copyright © 2001-2019 by Brian Damgaard
DefaultDirName={pf}\BDSokobanYASC
DefaultGroupName=Sokoban
UninstallDisplayIcon={app}\Sokoban.exe
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
Source: "Sokoban.exe"; DestDir: "{app}"
;Source: "FileAssoc.exe"; DestDir: "{app}"; DestName: "Sokoban YASC File Associations.exe"
;Source: "Readme.txt"; DestDir: "{userdocs}\Sokoban\Sokoban YASC"; Flags: isreadme
Source: "PNG.dll"; DestDir: "{app}"
Source: "YASO.dll"; DestDir: "{app}\Plugins"
Source: "YASS.dll"; DestDir: "{app}\Plugins"
Source: "Alchemyst - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "AntiqueDesk - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
;Source: "AntiqueDesk2 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "AntiqueDesk3 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "ChineseCheckers - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "Games 4 Brains Animated - Red.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "Games 4 Brains Animated - Yellow.bmp"; DestDir: "{app}\Skins\Common Skins"
;Source: "HeavyMetal - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "HeavyMetal3 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
;Source: "Jumanji - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
;Source: "Jumanji2 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "Jumanji3 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "KSokoban - Anders Widell.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "Macintosh.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "Magneto - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "MarbleMagic - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
;Source: "Metalica2 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "Metalica3 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
;Source: "NightShift2 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "NightShift3 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "Skulloban - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
;Source: "Skulloban2 - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
Source: "SokoGems - Gerry Wiseman.bmp"; DestDir: "{app}\Skins\Common Skins"
;
; "Unrar.exe" and "Unzip.exe" must be distributed separately
; because even though these utilities are free, they are not
; under the GPL license like Sokoban YASC.
;
;Source: "unrar.exe"; DestDir: "{app}"
;Source: "unzip.exe"; DestDir: "{app}"

[Icons]
Name: "{group}\Sokoban YASC"; Filename: "{app}\Sokoban.exe"
Name: "{userdesktop}\Sokoban YASC"; Filename: "{app}\Sokoban.exe"

[Registry]
;Root: HKCU; Subkey: "Software\BDSokobanYASC"; Flags: uninsdeletekey 
;Root: HKLM; Subkey: "Software\BDSokobanYASC"; Flags: uninsdeletekey

; -- "File associations for pre-Vista Windows versions Win9x, WinNT, and WinXP" section
; The following file associations are not installed here but it's left to the program to do so.
; They are, however, removed by this script during un-installation (see the [Code] section).

;Root: HKCR; Subkey: ".xsb"; ValueType: string; ValueName: ""; ValueData: "Sokoban YASC (bd).Level"; Flags: uninsdeletevalue
;Root: HKCR; Subkey: ".sok"; ValueType: string; ValueName: ""; ValueData: "Sokoban YASC (bd).Level"; Flags: uninsdeletevalue
;Root: HKCR; Subkey: "Sokoban YASC (bd).Level"; ValueType: string; ValueName: ""; ValueData: "Sokoban Level"; Flags: uninsdeletekey
;Root: HKCR; Subkey: "Sokoban YASC (bd).Level\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Sokoban.exe,0"
;Root: HKCR; Subkey: "Sokoban YASC (bd).Level\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Sokoban.exe"" ""%1"""
; -- end of "File associations for pre-Vista Windows versions Win9x, WinNT, and WinXP" section

[Run]
;Filename: "{app}\Sokoban.exe"; Description: "{cm:LaunchProgram,Sokoban YASC}"; Flags: nowait postinstall skipifsilent

[UninstallDelete] 
Type: filesandordirs; Name: "{app}\*.*"
Type: dirifempty; Name: "{app}"
Type: filesandordirs; Name: "{userdocs}\Sokoban\Sokoban YASC\*.*"
Type: dirifempty; Name: "{userdocs}\Sokoban\Sokoban YASC"
Type: dirifempty; Name: "{userdocs}\Sokoban"

[Code]
procedure CurStepChanged(CurStep: TSetupStep);
var ResultCode:Integer;
begin
  if CurStep = ssPostInstall then begin
     //MsgBox('CurStepChanged:' #13#13 'Install just finished.', mbInformation, MB_OK);
     if FileExists(ExpandConstant('{app}\Sokoban.ini')) or
        FileExists(ExpandConstant('{localappdata}\VirtualStore\Program Files\BDSokobanYASC\Sokoban.ini'))
        then begin
        // there are user data in the "program files\<application name>" folder,
        // e.g., '{app}\Levels' and '{app}\Skins';
        // run the application now in order to move data files to the
        // user's private document folder;
        //
        // running the application now means it automatically inherits
        // the administrator privileges from the installation program,
        // hence, the file transfer won't be blocked by the operating system;
        if Exec(ExpandConstant('{app}\Sokoban.exe'), '/install', '', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode) then begin
           //MsgBox('CurStepChanged:' #13#13 'Install just finished.' #13#13 'There are data folders in the program folder.', mbInformation, MB_OK);
           end
        else
           MsgBox('After Installation:' #13#13 'The application could not be executed. ' + SysErrorMessage(ResultCode) + '.', mbError, MB_OK);
        end;
    end;
end;

(*
; -- "File associations for pre-Vista Windows versions Win9x, WinNT, and WinXP" section
procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  APPLICATION_DATA_SUB_FOLDER, FILE_ASSOC_SECTION, INI_FILE_NAME,
  OLD_SOK_ASSOC_KEY, OLD_XSB_ASSOC_KEY,
  s, SOK, SOKOBAN_YASC_LEVEL, XSB:String;
begin
  // The following constants must match the values in the [Registry] section above
  // and the source-file "FileAssoc_.pas".
  APPLICATION_DATA_SUB_FOLDER := 'Sokoban\Sokoban YASC'
  FILE_ASSOC_SECTION          := 'File Associations';
  INI_FILE_NAME               := 'Sokoban.ini';
  OLD_SOK_ASSOC_KEY           := 'Old SOK Association';
  OLD_XSB_ASSOC_KEY           := 'Old XSB Association';
  SOK                         := '.sok';
  SOKOBAN_YASC_LEVEL          :='Sokoban YASC (bd).Level';
  XSB                         := '.xsb';
  

  case CurUninstallStep of
    usUninstall:
      begin
        //MsgBox('CurUninstallStepChanged:' #13#13 'Uninstall is about to start.', mbInformation, MB_OK)
        // ...insert code to perform pre-uninstall tasks here...
        
        if RegQueryStringValue(HKCR, SOK, '', s) then
           if    s = SOKOBAN_YASC_LEVEL then
                 begin RegDeleteValue(HKCR, SOK, '');
                       s:=AddBackslash(AddBackslash(ExpandConstant('{userdocs}')) + APPLICATION_DATA_SUB_FOLDER) + INI_FILE_NAME;
                       s:=GetIniString(FILE_ASSOC_SECTION, OLD_SOK_ASSOC_KEY, '', s);
                       if (s <> SOK) and (s <> '') then
                          // associate the previously registered program with the file-type
                          RegWriteStringValue(HKCR, SOK, '', s);
                 end
           else if s = '' then RegDeleteValue(HKCR,SOK,'');
        
        if RegQueryStringValue(HKCR, XSB, '', s) then
           if    s = SOKOBAN_YASC_LEVEL then
                 begin RegDeleteValue(HKCR, XSB, '');
                       s:=AddBackslash(AddBackslash(ExpandConstant('{userdocs}')) + APPLICATION_DATA_SUB_FOLDER) + INI_FILE_NAME;
                       s:=GetIniString(FILE_ASSOC_SECTION, OLD_XSB_ASSOC_KEY, '', s);
                       if (s <> XSB) and (s <> '') then
                          //associate the previously registered program with the file-type
                          RegWriteStringValue(HKCR, XSB, '', s);
                 end
           else if s = '' then RegDeleteValue(HKCR,XSB,'');
        
        RegDeleteKeyIfEmpty(HKCR, XSB);
        RegDeleteKeyIfEmpty(HKCR, SOK);
        RegDeleteKeyIncludingSubKeys(HKCR,SOKOBAN_YASC_LEVEL);
      end;
    usPostUninstall:
      begin
        //MsgBox('CurUninstallStepChanged:' #13#13 'Uninstall just finished.', mbInformation, MB_OK);
        // ...insert code to perform post-uninstall tasks here...
      end;
  end;
end;
; -- end of "File associations for pre-Vista Windows versions Win9x, WinNT, and WinXP" section
*)

