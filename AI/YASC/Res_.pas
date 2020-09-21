unit Res_;

interface

uses Classes,Sound_,Misc_;

const // don't localize
  RC_EXE              : String = 'EXE';
  RC_JPG              : String = 'JPG';
  RC_LEVEL            : String = 'LEVEL';
  RC_PALETTE          : String = 'PAL';
  RC_RTF              : String = 'RTF';
  RC_TXT              : String = 'TXT';
  RC_WAV              : String = 'WAV';

  ARROW_CURSOR_RES_NAME
                      = 'ARROW_CURSOR';
  BACKGROUND_RES_NAME = 'Background';
  BOARD_BACKGROUND_RES_NAME
                      = 'Board_Background';
  CRAZY_MONK_OLD_RES_NAME {name changed in version 1.501}
                      = 'Martel_and_Meger';
  CRAZY_MONK_RES_NAME = 'Crazy_Monk'; {underscores are substituted by spaces during installation}

  DRAG_CURSOR_RES_NAME= 'DRAG_CURSOR';
  DRAWINGTOOL_CURSOR_NAME
                      : array[TDrawingTool] of String
                      = ('','CURSOR_WALL','CURSOR_BOX','CURSOR_GOAL','CURSOR_PLAYER','CURSOR_FLOOR','CURSOR_ERASE','CURSOR_SELECT');
  DU_PELOUX_OLD_RES_NAME  {name changed in version 1.118}
                      = 'Peloux';
  DU_PELOUX_RES_NAME  = 'du_Peloux'; {underscores are substituted by spaces during installation}
  FILE_ASSOC_PROGRAM_RES_NAME
                      = 'FILE_ASSOC_PROGRAM';
  HAND_CURSOR_RES_NAME= 'HAND_CURSOR';
  HELP_RES_NAME       = 'Help';
  HOLLAND_RES_NAME    = 'Holland';
  JSOKO_RES_NAME      = 'JSoko';
  SKINNER_RES_NAME    = 'Skinner';
  YASGEN_RES_NAME     = 'YASGen';
  LEVEL_RES_NAME      : array[0..8] of String =
    (CRAZY_MONK_RES_NAME,DU_PELOUX_RES_NAME,'GRIGoRusha','Haywood',HOLLAND_RES_NAME,SKINNER_RES_NAME,'Sladkey',YASGEN_RES_NAME,'Yoshio');
  LICENSE_RES_NAME    = 'License';
  PALETTE_RES_NAME    : array[0..10] of String =
    ('Blues','Cromatic','Fire','Firestorm','Grey','Neon','Pastels',
     'Purple','Spectrum','Standard','Volcano'
    );
  README_RES_NAME     = 'ReadMe';
  RELEASE_NOTES_RES_NAME
                      = 'Releases';
  SOKOBAN_BDE_OLD_SKIN_SCRIPT_FILE_NAME         // notice it's the filename, not the resource name
                      = 'Sokoban 3400 2004';    // changed name to 'Sokoban BDE' in 1.134

  COMMON_SKINS_RES_NAME
                      = 'Common_Skins';
  SOKOBAN_BDE_RES_NAME= 'Sokoban_BDE';
  SOKOBAN_PLUS_PLUS_RES_NAME
                      = 'SokobanPP';
  SOKOBAN_FOR_WINDOWS_RES_NAME
                      = 'Sokoban_for_Windows';
  YASC_SETTINGS_RES_NAME
                      = 'YASC_Settings';

  SKIN_TYPE_RES_NAME  : array[0..10] of String = // underscores are substituted by spaces during installation
    (COMMON_SKINS_RES_NAME,
     JSOKO_RES_NAME,
     SOKOBAN_BDE_RES_NAME,
     SOKOBAN_FOR_WINDOWS_RES_NAME,
     'SokobanP',
     SOKOBAN_PLUS_PLUS_RES_NAME,
     'Sokofan',
     'SokoStation4',
     'SuperSoko',
     'Visual_Sokoban',
     YASC_SETTINGS_RES_NAME
    );
  SOUND_RES_NAME      : array[TSoundType] of String =
    ('MOVE','PUSH','GOAL','BLOCK',
     'SOLUTION','UNDO','JUMP','RESTART','MENU','MENUCLICK');

function  MemoryStreamLoadFromResource(const RCName,RCType:String):TMemoryStream;
function  ResourceSaveToFile(const FileName,RCName,RCType:String):Boolean;

implementation

uses SysUtils,Windows,Forms,SokUtil_,Text_;

// ----------------------------------------------------------------
// Use the bat-file 'Res.bat' to build the resource-file 'Res.res'.
// ----------------------------------------------------------------
{
@echo off
rem RES.BAT - build resource-file for the Sokoban project
rem
rem First edit this file to match your directory-structure
rem and your Delphi version
rem
c:\compiler\delphi4\bin\brcc32 c:\compiler\delphi4\pg\sokoban\res.rc
pause
}

{$R Res.res}

function  LockResource(const RCName,RCType:String; var Size:Cardinal; var Memory:Pointer):Boolean;
var H:HRSRC; H1:HGLOBAL;
begin
  Memory:=nil;
  H:=FindResource(0,PChar(RCName),PChar(RCType));
  if H<>0 then begin
     Size:=SizeOfResource(0,H);
     if Size<>0 then begin
        H1:=LoadResource(0,H);
        if H1<>0 then Memory:=Windows.LockResource(H1);
        end;
     end;
  Result:=Memory<>nil;
end;

function  MemoryStreamLoadFromResource(const RCName,RCType:String):TMemoryStream;
var Ok:Boolean; RCSize:Cardinal; RCMemory:Pointer;
begin
  Result:=nil;
  Ok:=LockResource(RCName,RCType,RCSize,RCMemory);
  if Ok then
     try    Result:=TMemoryStream.Create;
            Result.Size:=RCSize;
            Move(RCMemory^,Result.Memory^,Result.Size);
     except on E:Exception do begin
            Result.Free; Error(E.Message,'');
            Result:=nil;
            end;
     end;
end;

function  ResourceSaveToFile(const FileName,RCName,RCType:String):Boolean;
var M:TMemoryStream;
begin
  Result:=False;
  M:=MemoryStreamLoadFromResource(RCName,RCType);
  if M<>nil then
     try     try    //raise exception.create('Test #13455');
                    M.SaveToFile(FileName);
                    Result:=True;
             except on E:Exception do
                       Result:=Error(Format(SaveFileFailedText__,[FileName])+
                                     NL+NL+TEXT_FAILURE_DESCRIPTION+NL+E.Message,
                                     Application.Title);
             end;
     finally M.Free;
     end;
end;

end.

