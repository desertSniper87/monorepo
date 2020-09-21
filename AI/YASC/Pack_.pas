unit Pack_;
{most of the code is this module is obsolete. Currently, Sokoban YASC uses the}
{file-format supported by the module 'SokFile_'.}

interface

uses Windows,Classes,IniFile_;

const
  {keys and tags specific to a Sokoban YASC level-collection file}
  KEY_LEVEL                            = 'Level';    {caution: not necessarily identical to 'text_level'}
  PACK_MACRO_BEGIN                     = '<#@@_';    {old style, no need to be that complicated; use '<#' instead}
  PACK_MACRO_END                       = '_@@#/>';   {old style, use '#/>' instead}
  PACK_MACRO_END1                      = '_@@#\>';   {kludge: an early version used this tag (a bad choice, since it doesn't follow the normal html convention with "/" used as terminator)}
  PACK_COMBINED_MOVE_BEGIN             = '<:';       {old style, use '(' instead}
  PACK_COMBINED_MOVE_END               = ':>';       {old style, use ')' instead}
  SECTION_NAME_MACROS                  = '*Macros*'; {old style macro section; 'SokFile_' handles filenotes and macros together}
  SECTION_NAME_PACK                    = '*Sokoban Package*'; {old style package file identification}

type

  TPack = class
  private
  public
    Index:TStringList;
    IniFile:TIniFile;
    Modified:Boolean;

    procedure Clear;
    procedure Close;
    constructor Create;
    destructor Destroy; override;
    function  FindPriorNextSection(var SectionName:String; Prior,WrapAround:Boolean):Boolean;
    function  FirstItemName:String;
    function  LastItemName:String;
    function  LoadGame(const Name:string):Boolean;
    function  MacroExpand(var Str:String):Boolean;
    function  MoveItem(const SectionName:String; NewPosition:Integer):Boolean;
    function  New(const FileName:String):Boolean;
    function  Open(const FileName:String):Boolean;
  end;

  function  ConvertPackFileToTextFile(var FileName__:String):Boolean;
  function  ExtractIniFileName(const IniFileSectionFileName:String):String;
  function  ExtractSectionName(const IniFileSectionFileName:String):String;
  function  IniFileLoadSection(const IniFile:TIniFile; const SectionName:String; StringList:TStringList):Boolean;
  function  IniFileSaveSection(const IniFile:TIniFile; const SectionName:String; StringList:TStringList; DeleteOld:Boolean):Boolean;
  function  IsAnIniFileSectionFileName(const FileName:String):Boolean;
//function  IsAPackFile(const FileName:String):Boolean;
  function  IsAValidItemSectionName(const SectionName:String):Boolean;
  function  MakeIniFileSectionFileName(const IniFileName,SectionName:String):String;
  procedure YASGenStatistics(GeneratedFileName__,OriginalFileName__:String);

implementation

uses SysUtils,Forms,Controls,
     Misc_,Text_,Main_,Game_,
     SokUtil_,SokFile_,SokGame_;

function  IsAPackFile(const FileName:String):Boolean; forward;

constructor TPack.Create;
begin
  Modified:=False; IniFile:=nil; Index:=nil;
  try Index:=TStringList.Create;
  except on E:Exception do begin
         Error(E.Message,Application.Title); Index.Free; Index:=nil;
         end;
  end;
end;

destructor TPack.Destroy;
begin
  Close; Index.Free; Index:=nil;
end;

procedure TPack.Close;
begin
  if      Modified then
          try    if IniFile<>nil then IniFile.WriteString(SECTION_NAME_PACK,KEY_DATE_LAST_CHANGE,FormatDateTime(FORMAT_DATE_TIME,Now));
          except on E:Exception do Error(E.Message,Application.Title);
          end;
  try     try IniFile.Free;
          except on E:Exception do Error(E.Message,Application.Title);
          end;
  finally IniFile:=nil; Modified:=False; if Index<>nil then Index.Clear;
  end;
end;

function TPack.Open(const FileName:String):Boolean;
begin
  Result:=(IniFile<>nil) and
          (AnsiCompareText(IniFile.FileName,FileName)=0);
  if not Result then
     try    Close;
            IniFile:=TIniFile.Create(FileName);
            Result:=IniFile.SectionExists(SECTION_NAME_PACK);
            if   Result then IniFile.ReadSections(Index)
            else Close;
     except
       on E:Exception do begin
          Result:=False; Close; Error(E.Message,Application.Title);
          end;
     end;
end;

function  TPack.New(const FileName:String):Boolean;
begin
  try    Close;
         IniFile:=TIniFile.Create(FileName);
         Clear;
         IniFile.WriteString(SECTION_NAME_PACK,KEY_DATE_CREATED,FormatDateTime(FORMAT_DATE_TIME,Now));
         Result:=True; Modified:=True;
  except
    on E:Exception do begin
       Result:=False; Close; Error(E.Message,Application.Title);
       end;
  end;
end;

procedure TPack.Clear;
var i:Integer;
begin
  if (IniFile<>nil) and (Index<>nil) then with IniFile do begin
     ReadSections(Index);
     for i:=0 to Pred(Index.Count) do EraseSection(Index[i]);
     end;
end;

function  TPack.LoadGame(const Name:string):Boolean;
var b:Boolean;
begin
  if MainForm<>nil then
     Result:=MainForm.LoadGame(StrWithBrackets(Name),True)
  else begin
     Result:=MainForm.GameViewer.Game.LoadFromFileOrClipboard(Name,nil,nil,b);
     MainForm.GameViewer.Game.InitGame;
     end;
end;

function  TPack.FirstItemName:String;
var i:Integer;
begin
  for i:=0 to Pred(Index.Count) do
      if IsAValidItemSectionName(Index[i]) then begin
         Result:=Index[i]; exit;
         end;
  Result:='';
end;

function  TPack.LastItemName:String;
var i:Integer;
begin
  for i:=Pred(Index.Count) downto 0 do
      if IsAValidItemSectionName(Index[i]) then begin
         Result:=Index[i]; exit;
         end;
  Result:='';
end;

function  TPack.FindPriorNextSection(var SectionName:String; Prior,WrapAround:Boolean):Boolean;
var i,j,FirstSectionNo,PriorSectionNo:Integer; s:String;
begin
  Result:=False; s:=StrWithoutBrackets(SectionName);
  FirstSectionNo:=-1; PriorSectionNo:=-1;
  for i:=0 to Pred(Index.Count) do
      if IsAValidItemSectionName(Index[i]) then begin
         if   FirstSectionNo=-1 then FirstSectionNo:=i;
         if   AnsiCompareText(SectionName,Index[i])=0 then begin
              if   Prior then begin
                   if (PriorSectionNo=-1) and WrapAround then
                      for j:=Pred(Index.Count) downto Succ(i) do
                          if IsAValidItemSectionName(Index[j]) then begin
                             PriorSectionNo:=j; break;
                             end;
                   if PriorSectionNo<>-1 then begin
                      Result:=True; SectionName:=Index[PriorSectionNo];
                      end
                   end
              else begin for j:=Succ(i) to Pred(Index.Count) do
                             if IsAValidItemSectionName(Index[j]) then begin
                                Result:=True; SectionName:=Index[j]; break;
                                end;
                         if (not Result) and WrapAround and
                            (FirstSectionNo<>-1) and (PriorSectionNo<>-1) then begin
                            Result:=True; SectionName:=Index[FirstSectionNo];
                            end;
                   end;
              break;
              end
         else PriorSectionNo:=i;
         end;
end;

function  TPack.MacroExpand(var Str:String):Boolean;
var i,j:Integer; Key:String;
begin {old style tags only; see 'SokUtil_.StrMacroExpand' for current version}
  Result:=False;
  if IniFile<>nil then begin
     i:=AnsiPos(PACK_MACRO_BEGIN,Str);
     while i<>0 do begin
       Delete(Str,i,Length(PACK_MACRO_BEGIN));
       j:=AnsiPos(PACK_MACRO_END,Str);
       if j=0 then j:=AnsiPos(PACK_MACRO_END1,Str); // kludge: an early version used another tag for 'MACRO_END'
       if j>0 then begin
          Key:=System.Copy(Str,i,j-i);
          Delete(Str,i,j-i+Length(PACK_MACRO_END));
          System.Insert(StrWithoutDoubleQuotes(IniFile.ReadString(SECTION_NAME_MACROS,Key,Key)),Str,i);
          Result:=True;
          end;
       i:=AnsiPos(PACK_MACRO_BEGIN,Str);
       end;
     end;
end;

function  TPack.MoveItem(const SectionName:String; NewPosition:Integer):Boolean;
var i,j:Integer;
    IniFileName,TempFileName:String; oCursor:TCursor;
    TempIniFile:TIniFile; StringList:TStringList;
begin // doesn't work; maybe because of timing problems: windows doesn't flush file-functions immidiately. An example: a renamed file can still be opened with the old name?!
  Result:=(IniFile<>nil) and (Index<>nil);
  if Result then
     try    IniFileName:=IniFile.FileName;
            TempFileName:=//MainForm.ApplicationDataPath+'tmp1.tmp';
                          MakeNewFileName(MainForm.ApplicationDataPath+System.Copy(TEMP_FILE_NAME_EXT,2,MaxInt),TEMP_FILE_NAME_EXT,True);
            if TempFileName='' then raise Exception.Create(DiskFullText)
            else begin
               DeleteFile(TempFileName);

               Close;
               if not RenameFile(IniFileName,TempFileName) then
                  raise Exception.Create(Format(FileRenameFailedText__,[IniFileName,TempFileName]));
               if not Open(IniFileName) then
                  raise Exception.Create(Format(FileNotFoundText__,[IniFileName]));
               for i:=0 to Pred(Index.Count) do IniFile.EraseSection(Index[i]);

               j:=0;
               TempIniFile:=TIniFile.Create(TempFileName);
               StringList:=TStringList.Create; oCursor:=Screen.Cursor;
               try
                 Screen.Cursor:=crHourGlass;

                 TempIniFile.ReadSections(Index);
                 i:=Index.IndexOf(SectionName);
                 if i<>-1 then begin
                    Index.Delete(i); if NewPosition>i then Dec(NewPosition);
                    end;

                 for i:=0 to Pred(Index.Count) do
                     if Result then begin
                        if j=NewPosition then begin
                           Result:=IniFileLoadSection(TempIniFile,SectionName,StringList);
                           if Result then
                              Result:=IniFileSaveSection(IniFile,SectionName,StringList,False);
                           NewPosition:=-3;
                           Inc(j);
                           end;
                        if Result then begin
                           Result:=IniFileLoadSection(TempIniFile,Index[i],StringList);
                           if Result then
                              Result:=IniFileSaveSection(IniFile,Index[i],StringList,False);
                           end;
                        Inc(j);
                        end;
                 if Result and (NewPosition<>-3) then begin
                    Result:=IniFileLoadSection(TempIniFile,SectionName,StringList);
                    if Result then
                       Result:=IniFileSaveSection(IniFile,SectionName,StringList,False);
                    end;

               finally
                 TempIniFile.Free;
                 StringList.Free;
                 if Result then DeleteFile(TempFileName)
                 else begin Close;
                            DeleteFile(IniFileName);
                            RenameFile(TempFileName,IniFileName);
                    end;
                 Screen.Cursor:=oCursor;
               end;

{              if Result then begin
                  s:=IniFile.FileName;
                  Close;

                  TempFileName2:=MakeNewFileName(MainForm.ApplicationDataPath+System.Copy(TEMP_FILE_EXT,2,MaxInt),TEMP_FILE_EXT);
                  Result:=TempFileName2<>'';
                  if Result then begin
                     Result:=RenameFile(s,TempFileName2);
                     if Result then begin
                        Result:=RenameFile(TempFileName,s);
                        if Result then
                           if   DeleteFile(TempFileName2) then //
                           else raise Exception.Create('Delete File '+TempfileName2+' failed.')
                        else begin
                           Result:=RenameFile(TempFileName2,s);
                           if not Result then Error(Format(FileWasRenamedText__,[s,TempFileName2]));
                           Result:=False;
                           end;
                        end;
                     end;
                  if Result then Result:=Open(s)
                  else Open(s);
                  end;
}
               end;
     except on E:Exception do begin
               Error(E.Message,Application.Title); Result:=False;
               end;
     end;
end;

function ConvertPackFileToTextFile(var FileName__:String):Boolean;
{Converts an old-style packfile to a textfile with the syntax
 supported by the module 'SokFile_'}
var s:String; Node:TNode; Level:TLevel; Lines:TList;
    SokoFile:TSokoFile; SokoGame:TSokoGame; Rle:TRle;

  procedure ExtractText(Rle__:TRle; var FirstIndex,LastIndex:Integer);
  begin
    FirstIndex:=0; LastIndex:=Rle__.Position;
    while (FirstIndex<=LastIndex) and
          (Rle__.Text[STRING_BASE+FirstIndex]<>'=') do Inc(FirstIndex);
    Inc(FirstIndex);
    if (FirstIndex<=LastIndex) and
       (Rle__.Text[STRING_BASE+FirstIndex]=DOUBLE_QUOTE) then Inc(FirstIndex);
    if (LastIndex>0) and
       (Rle__.Text[STRING_BASE+Pred(LastIndex)]=DOUBLE_QUOTE) then Dec(LastIndex);
  end;

  function ParseBoard(const s__:String; Level__:TLevel; Rle__:TRle):Boolean;
  var i,j,Len:Integer; Ch:Char; Node:TNode;
  begin
    Result:=Rle__.Expand(s__); ExtractText(Rle__,i,Len); j:=i;

    while Result and (j<Len) do begin
      Ch:=Rle__.Text[STRING_BASE+j];
      if      (Ch=FLOOR_NON_BLANK_CH1) or (Ch=FLOOR_NON_BLANK_CH2) then
              Rle__.Text[STRING_BASE+j]:=FLOOR_CH
      else if Ch=SOKOBAN_NEWLINE then begin
              Result:=CreateObject(otNode,Node) and
                      Level__.BoardAsTextLines.Push(Node).SetText(System.Copy(Rle__.Text,STRING_BASE+i,j-i));
              i:=Succ(j);
              end;
      Inc(j);
      if (j=Len) and (i<j) then {add last line}
         Result:=CreateObject(otNode,Node) and
                 Level__.BoardAsTextLines.Push(Node).SetText(System.Copy(Rle__.Text,STRING_BASE+i,j-i));
      end;
    if   Result then Level__.BoardAsTextLines.Reverse
    else Level__.BoardAsTextLines.Clear;
  end; {ParseBoard}

  function ParseNotes(const s__:String; Level__:TLevel; Rle__:TRle):Boolean;
  var i,j,n:Integer; s:String; Node:TNode;
  begin
    Result:=Rle__.SetText(s__); Rle__.Position:=Length(s__); ExtractText(Rle__,i,j);
    s:=StrSubstitute(
         StrSubstitute(
           StrSubstitute(
             System.Copy(Rle__.Text,STRING_BASE+i,j-i),
             PACK_MACRO_BEGIN,MACRO_BEGIN,n),
           PACK_MACRO_END,MACRO_END,n),
         PACK_MACRO_END1,MACRO_END,n);

    i:=AnsiPos(C_STYLE_NEWLINE,s); j:=Length(C_STYLE_NEWLINE);
    while Result and (i>=STRING_BASE) do begin
      Result:=CreateObject(otNode,Node) and
              Level.Notes.Lines.Push(Node).SetText(System.Copy(s,STRING_BASE,i-STRING_BASE));
      Delete(s,STRING_BASE,i-STRING_BASE+j);
      i:=AnsiPos(C_STYLE_NEWLINE,s);
      end;
    if (not IsBlank(s)) and Result then
       Result:=CreateObject(otNode,Node) and
              Level.Notes.Lines.Push(Node).SetText(s);

    if   Result then Level__.Notes.Lines.Reverse
    else Level__.Notes.Lines.Clear;
  end; {ParseNotes}

  function ParseSnapshot(const s__:String; Level__:TLevel; Rle__:TRle; First__:Boolean):Boolean;
  var i,j,n,Len:Integer; s:String; Node:TNode; SnapshotAsText:TSnapshotAsText;
  begin
    Result:=Rle__.Expand(s__); ExtractText(Rle__,i,j);
    if Result and (i<j) then begin
       s:=StrSubstitute(
            StrSubstitute(
              System.Copy(Rle__.Text,STRING_BASE+i,j-i),
              PACK_COMBINED_MOVE_END,GROUP_END_CH,n),
              PACK_COMBINED_MOVE_BEGIN,GROUP_BEGIN_CH,n);
       Len:=Length(s); j:=0;
       Result:=CreateObject(otSnapshotAsText,TNode(SnapshotAsText));
       if Result then begin
          if   First__ then Level__.SnapshotsAsText.Push(SnapshotAsText)
          else Level__.SnapshotsAsText.Add(SnapshotAsText);
          i:=AnsiPos(EQUAL,Rle__.Text);
          if i>=STRING_BASE then
             Result:=SnapshotAsText.SetName(System.Copy(Rle__.Text,STRING_BASE,i-STRING_BASE));
          while (j<Len) and Result do begin
            Result:=CreateObject(otNode,Node) and
                    SnapshotAsText.MovesAsTextLines.Push(Node).SetText(System.Copy(s,STRING_BASE+j,LINE_WIDTH_MOVES));
            Inc(j,LINE_WIDTH_MOVES);
            end;
          if   Result then SnapshotAsText.MovesAsTextLines.Reverse
          else SnapshotAsText.MovesAsTextLines.Clear;
          end;
       end;
  end;

begin {ConvertPackFileToTextFile}
  Result:=False; SokoFile:=nil; SokoGame:=nil; Rle:=nil; Level:=nil; Lines:=nil;
  try
    if CreateObject(otSokoFile,TNode(SokoFile)) and
       CreateObject(otRle     ,TNode(Rle)) and
       CreateObject(otList    ,TNode(Lines)) then
       try    SokoGame:=TSokoGame.Create;
              SokoGame.Verbose:=False;

              Result:=SokoFile.SetName(FileName__) and
                      SokoFile.UpdateFileFormatDescription(False) and
                      IsAPackFile(FileName__) and
                      Lines.LoadFromFile(FileName__);
              while (not Lines.IsEmpty) and Result do begin
                Node:=Lines.Pop;
                if   IsBlank(Node.Text) then {}
                else if Node.Text[STRING_BASE]=LEFT_BRACKET then begin
                        if (Level<>nil) and
                           Level.BoardAsTextLines.IsEmpty then
                           SokoFile.Levels.Remove(Level,True);
                        Level:=nil;
                        if AnsiPos(SECTION_NAME_MACROS,Node.Text)=STRING_BASE+1 then begin {macros}
                           if Node.SetText(StrRemoveCharacters(Node.Text,TITLE_ILLEGAL_CHARACTERS)) then begin
                              SokoFile.FileHeader.Lines.AddBlankLine;
                              SokoFile.FileHeader.Lines.AddBlankLine;
                              SokoFile.FileHeader.Lines.Add(Node); Node:=nil;
                              end;
                           while (not Lines.IsEmpty) and
                                 (IsBlank(Lines.First.Text)
                                  or
                                  (Lines.First.Text[STRING_BASE]<>LEFT_BRACKET)
                                 ) do SokoFile.FileHeader.Lines.Add(Lines.Pop);
                           SokoFile.FileHeader.Lines.TrimBlankLines;
                           end
                        else {level}
                           Result:=CreateObject(otLevel,TNode(Level)) and
                                   SokoFile.Levels.Push(Level).SetName(StrWithoutBrackets(Node.Text))
                        end
                else if (Level<>nil) and
                        (AnsiPos(KEY_LEVEL,Node.Text)=STRING_BASE) then
                         Result:=ParseBoard(Node.Text,Level,Rle)
                else if (Level<>nil) and
                        ((AnsiPos(SNAPSHOT_TYPE_NAME[stBestSolutionMoves ],Node.Text)=STRING_BASE) or
                         (AnsiPos(SNAPSHOT_TYPE_NAME[stBestSolutionMoves1],Node.Text)=STRING_BASE)
                        ) then
                        Result:=ParseSnapshot(Node.Text,Level,Rle,True)
                else if (Level<>nil) and
                        (AnsiPos(SNAPSHOT_TYPE_NAME[stBestSolutionPushes ],Node.Text)=STRING_BASE) then
                        Result:=ParseSnapshot(Node.Text,Level,Rle,True)
                else if (Level<>nil) and
                        (AnsiPos(SNAPSHOT_TYPE_NAME[stSaveGame           ],Node.Text)=STRING_BASE) then
                        Result:=ParseSnapshot(Node.Text,Level,Rle,False)
                else if (Level<>nil) and
                        (AnsiPos(SECTION_NAME_INFORMATION,Node.Text)=STRING_BASE) then
                        Result:=ParseNotes(Node.Text,Level,Rle)
                else if AnsiPos(KEY_DATE_CREATED         ,Node.Text)=STRING_BASE then begin
                        SokoFile.FileHeader.Lines.Add(Node); Node:=nil;
                        end
                else if AnsiPos(KEY_DATE_LAST_CHANGE     ,Node.Text)=STRING_BASE then begin
                        SokoFile.FileHeader.Lines.Add(Node); Node:=nil;
                        end
                else;
                Node.Free;
                end;

              if (Level<>nil) and
                 Level.BoardAsTextLines.IsEmpty then
                 SokoFile.Levels.Remove(Level,True);

              SokoFile.Levels.Reverse;

              if Result then begin
                 Level:=TLevel(SokoFile.Levels.Items);
                 while (Level<>nil) and Result do begin {for each level...}
                   Level.Notes.MacroExpanded:=True; {avoid macro-expansion}
                   Node:=Level.Next;
                   Result:=SokoGame.LoadFromFile(Level.Name,SokoFile,True,s);

                   if Result and (SokoGame.SaveGame<>nil) then begin
                      SokoGame.History.Top:=0;
                      SokoGame.SaveGame.MoveCount:=SokoGame.SaveGame.MoveTop; {override 'LoadFromFile' which sets 'MoveCount' to 0}
                      Result:=SokoGame.LoadSnapshot(SokoGame.SaveGame);
                      end;

                   if Result then
                      Result:=SokoGame.SaveToFile(SokoFile,True,True,True,True,False);
                   Level:=TLevel(Node); {next level}
                   end;
                 if Result then Result:=SokoFile.SaveToFile(FileName__);
                 if Result then begin
                    s:=ChangeFileExt(FileName__,SOKOBAN_FILE_NAME_EXT);
                    if (not StrEqual(FileName__,s)) and
                       (not FileExists(s)) and
                       RenameFile(FileName__,s) then
                       FileName__:=s; {the filename is changed to default-extension}
                    end;
                 end;
       except on E:Exception do Result:=Error(E.Message,'Convert PackFile To TextFile');
       end;
  finally if SokoFile<>nil then SokoFile.Modified:=False;
          SokoGame.Free; SokoFile.Free; Rle.Free; Lines.Free;
  end;
end; {ConvertPackFileToTextFile}

function  ExtractIniFileName(const IniFileSectionFileName:String):String;
begin
  if   IsAnIniFileSectionFileName(IniFileSectionFileName) then
       Result:=StrWithoutTrailingPathDelimiter(ExtractFilePath(IniFileSectionFileName))
  else Result:='';
end;

function  ExtractSectionName(const IniFileSectionFileName:String):String;
var Len:Integer;
begin
  Len:=Length(IniFileSectionFileName);
  if (Len>0) and (IniFileSectionFileName[Len]=RIGHT_BRACKET) then begin
     Result:=SysUtils.ExtractFileName(IniFileSectionFileName);
     Len:=Length(Result);
     if   (Len>2) and (Result[1]=LEFT_BRACKET) and (Result[Len]=RIGHT_BRACKET) then begin
          Delete(Result,Len,1); Delete(Result,1,1);
          end
     else Result:='';
     end
  else Result:='';
end;

function IniFileLoadSection(const IniFile:TIniFile; const SectionName:String; StringList:TStringList):Boolean;
var i:Integer; s:String;
begin
  Result:=(IniFile<>nil) and
          IniFile.SectionExists(SectionName) and
          (StringList<>nil);
  if Result then
     try    IniFile.ReadSection(SectionName,StringList);
            for i:=0 to Pred(StringList.Count) do begin
                s:=IniFile.ReadString(SectionName,StringList.Strings[i],'');
                StringList.Strings[i]:=StringList.Strings[i]+'='+StrWithDoubleQuotes(s);
                end;
     except on E:Exception do begin
               Error(E.Message,Application.Title); StringList.Clear; Result:=False;
               end;
     end
end;

function IniFileSaveSection(const IniFile:TIniFile; const SectionName:String; StringList:TStringList; DeleteOld:Boolean):Boolean;
var i,j:Integer; s:String;
begin
  Result:=(IniFile<>nil) and (StringList<>nil);
  if Result then
     try    if DeleteOld and IniFile.SectionExists(SectionName) then
               IniFile.EraseSection(SectionName);
            for i:=0 to Pred(StringList.Count) do begin
                s:=StringList.Strings[i];
                j:=AnsiPos('=',s);
                if j<>0 then
                   IniFile.WriteString(
                     SectionName,
                     System.Copy(s,1,Pred(j)),
                     System.Copy(s,Succ(j),MaxInt));
                end;
     except on E:Exception do begin
               Error(E.Message,Application.Title); Result:=False;
               end;
     end
end;

function  IsAnIniFileSectionFileName(const FileName:String):Boolean;
begin
 Result:=ExtractSectionName(FileName)<>'';
end;

function  IsAPackFile(const FileName:String):Boolean;
begin
  Result:=FileStartsWith(FileName,LEFT_BRACKET+SECTION_NAME_PACK+RIGHT_BRACKET);
end;

function  IsAValidItemSectionName(const SectionName:String):Boolean;
begin
  Result:=(SectionName<>'') and
          (SectionName[1]<>TITLE_ILLEGAL_FIRST_CHARACTER);
end;

function  MakeIniFileSectionFileName(const IniFileName,SectionName:String):String;
begin
  if   IsAnIniFileSectionFileName(IniFileName) then
       Result:=ExtractIniFileName(IniFileName)
  else Result:=IniFileName;
  if   IsAnIniFileSectionFileName(SectionName) then
       Result:=StrWithTrailingPathDelimiter(Result)+StrWithBrackets(ExtractSectionName(SectionName))
  else Result:=StrWithTrailingPathDelimiter(Result)+StrWithBrackets(SectionName);
end;

procedure YASGenStatistics(GeneratedFileName__,OriginalFileName__:String);const LINE_LENGTH=90; TITLE_SUFFIX=', ''YASGen'' Version';type  TStatistics=record    BoardHeight:Integer;    BoardWidth:Integer;    ExhaustiveSearch:Boolean;    ImprovementComplexityEstimate:Integer;    ImprovementPushCount:Integer;    LevelName:String;    OriginalComplexityEstimate:Integer;    OriginalPushCount:Integer;    YASGenComplexityEstimate:Integer;    YASGenPushCount:Integer;    end;var i,Index,LevelCount,CompletedLevelsCount:Integer;    s:String; F:Text;    Statistics:array[0..1000] of TStatistics;    Level:SokFile_.TLevel; SokoFile:SokFile_.TSokoFile;    Game:SokGame_.TSokoGame; Solution:SokGame_.TSnapshot;begin  LevelCount:=0; CompletedLevelsCount:=0;
  FillChar(Statistics,SizeOf(Statistics),0);

  Game:=SokGame_.TSokoGame.Create; SokoFile:=TSokoFile.Create;
  try

    if SokoFile.LoadFromFile(GeneratedFileName__) then begin
       Level:=TLevel(SokoFile.Levels.First);
       while Level<>nil do begin
         Inc(LevelCount);

         if (LevelCount<=High(Statistics)) and
            Game.LoadFromFile(Level.Name,SokoFile,True,s) then with Statistics[LevelCount] do begin
            BoardHeight:=Game.BoardHeight-2;
            BoardWidth:=Game.BoardWidth-2;

            LevelName:=Game.Name;
            i:=AnsiPos(DOUBLE_QUOTE,LevelName);
            if i<>0 then Delete(LevelName,1,i);
            i:=AnsiPos(DOUBLE_QUOTE,LevelName);
            if i<>0 then Delete(LevelName,i,MaxInt);
            i:=AnsiPos(TITLE_SUFFIX,LevelName);
            if i<>0 then Delete(LevelName,i,Length(TITLE_SUFFIX));

            if        Game.BestSolutionPushes<>nil then
                      Solution:=Game.BestSolutionPushes
            else if   Game.BestSolutionMoves<>nil then
                      Solution:=Game.BestSolutionMoves
                 else Solution:=nil;
            if (Solution<>nil) and Game.LoadSnapshot(Solution) then with Solution do begin
               YASGenComplexityEstimate:=SecondaryScoreMetrics.BoxChanges+SecondaryScoreMetrics.BoxLines;
               YASGenPushCount:=PushCount;
               end;

            ExhaustiveSearch:=Level.Notes.Lines.ReadString('Exhaustive Search',s) and
                              (AnsiPos('Yes',s)<>0);
            end;

         Level:=TLevel(Level.Next);
         end;
       end;

    Game.Clear;
    if SokoFile.LoadFromFile(OriginalFileName__) then begin
       Level:=TLevel(SokoFile.Levels.First);
       while Level<>nil do begin
         if Game.LoadFromFile(Level.Name,SokoFile,True,s) then begin

            s:=Game.Name;
            i:=AnsiPos(DOUBLE_QUOTE,s);
            if i<>0 then Delete(s,1,i);
            i:=AnsiPos(DOUBLE_QUOTE,s);
            if i<>0 then Delete(s,i,MaxInt);
            i:=AnsiPos(TITLE_SUFFIX,s);
            if i<>0 then Delete(s,i,Length(TITLE_SUFFIX));

            Index:=0;
            for i:=1 to LevelCount do
                if s=Statistics[i].LevelName then Index:=i;
            if Index<>0 then with Statistics[Index] do begin
               if        Game.BuiltinBestSolutionPushes<>nil then
                         Solution:=Game.BuiltinBestSolutionPushes
               else if   Game.BuiltinBestSolutionMoves<>nil then
                         Solution:=Game.BuiltinBestSolutionMoves
                    else Solution:=nil;
               if        Game.BestSolutionPushes<>nil then
                         Solution:=Game.BestSolutionPushes
               else if   Game.BestSolutionMoves<>nil then
                         Solution:=Game.BestSolutionMoves;
               if (Solution<>nil) and Game.LoadSnapshot(Solution) then with Solution do begin
                  OriginalComplexityEstimate:=SecondaryScoreMetrics.BoxChanges+SecondaryScoreMetrics.BoxLines;
                  OriginalPushCount:=PushCount;
                  end;
               end;

            end;

         Level:=TLevel(Level.Next);
         end;
       end;

    AssignFile(F,GeneratedFileName__+' - Statistics.txt'); Rewrite(F);
    Writeln(F,'''YASGen'' Results');
    Writeln(F,'File: ',GeneratedFileName__);
    Writeln(F);
    for i:=1 to LINE_LENGTH do Write(F,'-'); Writeln(F);
    Writeln(F,'  Level','':35,'------YASGen-----':18,'--Original---':15,' -Improvement-':15);
    for i:=1 to LINE_LENGTH do Write(F,'-'); Writeln(F);
    Writeln(F,'No':4,'Width':7,' Height':7,'Name':24,'All':4,'Pushes':7,'Score':7,'Pushes':8,'Score':7,'Pushes':8,'Score':7);
    for i:=1 to LINE_LENGTH do Write(F,'-'); Writeln(F);

    for i:=1 to LevelCount do with Statistics[i] do begin

        Inc(Statistics[0].YASGenPushCount,YASGenPushCount);
        Inc(Statistics[0].OriginalPushCount,OriginalPushCount);
        Inc(Statistics[0].YASGenComplexityEstimate,YASGenComplexityEstimate);
        Inc(Statistics[0].OriginalComplexityEstimate,OriginalComplexityEstimate);

        if (OriginalPushCount<>0) and
           (YASGenPushCount>=OriginalPushCount) then begin
           ImprovementComplexityEstimate:=YASGenComplexityEstimate-OriginalComplexityEstimate;
           Inc(Statistics[0].ImprovementComplexityEstimate,ImprovementComplexityEstimate);
           ImprovementPushCount:=YASGenPushCount-OriginalPushCount;
           Inc(Statistics[0].ImprovementPushCount,ImprovementPushCount);
           end;

        s:=LevelName; if Length(s)>23 then s:=Copy(s,Length(s)-22,MaxInt);
        Write(F,i:4,BoardWidth:7,BoardHeight:7,s:24);
        if ExhaustiveSearch then begin
           Write(F,'Y':4); Inc(CompletedLevelsCount);
           end
        else Write(F,'':4);
        if YASGenPushCount>0 then Write(F,YASGenPushCount:7)
        else Write(F,'':7);
        if YASGenComplexityEstimate<>0 then Write(F,YASGenComplexityEstimate:7)
        else Write(F,'':7);
        if OriginalPushCount<>0 then Write(F,OriginalPushCount:8)
        else Write(F,'':8);
        if OriginalComplexityEstimate>0 then Write(F,OriginalComplexityEstimate:7)
        else Write(F,'':7);
        if ImprovementPushCount<>0 then Write(F,ImprovementPushCount:8)
        else Write(F,'':8);
        if ImprovementComplexityEstimate<>0 then Write(F,ImprovementComplexityEstimate:7)
        else Write(F,'':7);
        Writeln(F);
        end;
    for i:=1 to LINE_LENGTH do Write(F,'-'); Writeln(F);
    with Statistics[0] do
      Writeln(F,LevelCount:4,'Total':38,CompletedLevelsCount:4,YASGenPushCount:7,
                YASGenComplexityEstimate:7,
                OriginalPushCount:8, OriginalComplexityEstimate:7,
                ImprovementPushCount:8,ImprovementComplexityEstimate:7);
    for i:=1 to LINE_LENGTH do Write(F,'-'); Writeln(F);
    CloseFile(F);

  finally
    Game.Free; SokoFile.Free;
  end;
end;

end.
