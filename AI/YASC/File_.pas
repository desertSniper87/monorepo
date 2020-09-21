unit File_;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Misc_,Game_, Dialogs;

type
  TFileForm = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    SaveDialog1: TSaveDialog;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    CharFilter:String;
  public
    function Perform(Task: TFileTaskType;
                     TaskItemType:TFileTaskItemType;
                     const Name1,Name2: String;
                     var Name3: String;
                     IsAPackItem,IsASingleLevelFile:Boolean;
                     PackFileName,CurrentFileName:String):Boolean;
  end;

var
  FileForm: TFileForm = nil;

implementation

uses
  {$WARNINGS OFF}
  FileCtrl, // Warning: Unit 'FileCtrl' is specific to a platform
  {$WARNINGS ON}
  Text_,Main_,Open1_,IniFile_,Pack_,SokUtil_,SokFile_,Music_, Tools_;

{$R *.DFM}

procedure TFileForm.FormCreate(Sender: TObject);
begin
  Caption:=Application.Title; CharFilter:='';
  Label3.Left:=Label1.Left; Label3.Top:=Label1.Top;
  Label4.Left:=Label2.Left; Label4.Top:=Label2.Top;
  Label5.Left:=Label2.Left; Label5.Top:=Label2.Top; Label5.Caption:=NoUndoText;
  SaveDialog1.Filter:=AllFilesFilterText;
  SaveDialog1.FilterIndex:=1;
end;

procedure TFileForm.FormActivate(Sender: TObject);
begin
//
end;

procedure TFileForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Key=VK_ESCAPE then Close;
end;

procedure TFileForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
     BtnCancelClick(Sender);
end;

procedure TFileForm.BtnOkClick(Sender: TObject);
begin
//
end;

procedure TFileForm.BtnCancelClick(Sender: TObject);
begin
//
end;

procedure TFileForm.BtnHelpClick(Sender: TObject);
begin
  MainForm.BtnHelpClick(Sender);
  if Edit2.Visible then ActiveControl:=Edit2;
end;

function  TFileForm.Perform(Task:TFileTaskType; TaskItemType:TFileTaskItemType; const Name1,Name2:String;
                            var Name3:String; IsAPackItem,IsASingleLevelFile:Boolean; PackFileName,CurrentFileName:String):Boolean;
const
  TT_FILE_FILE=1; TT_FILE_PACKITEM=2; TT_PACKITEM_FILE=3; TT_PACKITEM_PACKITEM=4; TT_FILE=5; // transfer types, i.e., file => file, etc.
var i,TransferType:Integer; OK,UseSaveDialog:Boolean;
    s,SourceFile,SourcePackItem,DestFile,DestPackItem:String;
    f:TextFile;

  procedure SynchronizeLevelNamesAndKeywordTitles;
  var LevelTitle: String; Level : TLevel;
  begin
    // level names may contain a suffix for separation of levels with identical
    // names; if a level has a 'Title' keyword in the notes, it's necessary to
    // update it, so it matches the level name; otherwise the suffix could
    // change when the collection is loaded into the program again; such a
    // discrepancy would cause havoc if internal level lists, e.g., in the
    // duplicate finder window, refer to a level using the old suffix;
    with MainForm.SokoFile do begin
         Level:=TLevel(Levels.First);
         while Assigned(Level) do begin
           if Level.Notes.Lines.ReadString(KEY_TITLE,LevelTitle) and
              (LevelTitle<>'') and
              (not StrEqual(LevelTitle,Level.Name)) then
              Level.Notes.Lines.WriteString(KEY_TITLE,Level.Name);
           Level:=TLevel(Level.Next);
           end;
         end;
  end;

  procedure CalculateTransferType(const Source,Dest:String;
                                  var   TransferType:Integer;
                                  var   SourceFile,SourcePackItem,
                                        DestFile  ,DestPackItem:String);
  var i:Integer;
  begin
    if      Source='' then
            TransferType     :=TT_FILE // save as...
    else    TransferType     :=TT_FILE_FILE; // 'TransferType': [file, collection-item] x [file, collection-item]
    if      IsAPackItem then begin
            SourceFile       :=PackFileName;
            SourcePackItem   :=Source;
            TransferType     :=TT_PACKITEM_FILE;
            end
    else if IsAnIniFileSectionFileName(Source) then begin
            SourceFile       :=ExtractIniFileName(Source);
            SourcePackItem   :=ExtractSectionName(Source);
            TransferType     :=TT_PACKITEM_FILE;
            end
         else begin
            SourceFile       :=Source;
            SourcePackItem   :='';
            //SourcePackItem :=ExtractFileNameWithoutExt(Source);
            end;

    if      IsAPackItem and
            (AnsiPos(FILE_NAME_PATH_DELIMITER,Dest)=0) then begin    // not strictly correct: a root-filename doesn't have a path-delimiter; but this will have to do;
            DestFile         :=PackFileName;                         // perform operation within current pack
            DestPackItem     :=Dest;
            TransferType     :=TT_PACKITEM_PACKITEM;
            end
    else if IsAnIniFileSectionFileName(Dest) then begin
            DestFile         :=ExtractIniFileName(Dest);
            DestPackItem     :=ExtractSectionName(Dest);
            if   TransferType =TT_PACKITEM_FILE then
                 TransferType:=TT_PACKITEM_PACKITEM
            else TransferType:=TT_FILE_PACKITEM;
            end
    else if (TransferType     =TT_PACKITEM_FILE) and                 // transfer a single level:
            MainForm.SokoFile.IsASokobanFile(Dest) then begin        // add it to another collection, don't let this single item overwrite the complete file;
            DestFile         :=Dest;                                 // otherwise, it may surprise the user that the old collection has been deleted;
            DestPackItem     :=SourcePackItem;
            TransferType     :=TT_PACKITEM_PACKITEM;
            end
    else if (TransferType     =TT_FILE_FILE) and
            IsASingleLevelFile and                                   // transfer a single level
            (AnsiCompareText(Source,Dest)<>0) and                    // if = '0' then source = destination, maybe with the exception of uppercase/lowercase characters
            MainForm.SokoFile.IsASokobanFile(Dest) then begin        // add it to another collection, don't let this single item overwrite the complete file;
            DestFile         :=Dest;                                 // otherwise, it may surprise the user that the old collection has been deleted;
            if   SourcePackItem='' then begin
                 DestPackItem:='';
                 if IsAnIniFileSectionFileName(CurrentFileName) then
                    DestPackItem:=ExtractSectionName(CurrentFileName);
                 if (DestPackItem='') or StrEqual(DestPackItem,TEXT_LEVEL) then
                    DestPackItem:=ExtractFileNameWithoutExt(SourceFile);
                 end
            else DestPackItem:=SourcePackItem;
            TransferType     :=TT_FILE_PACKITEM;
            end
         else begin
            DestFile         :=Dest;
            DestPackItem     :='';
            //DestPackItem   :=ExtractFileNameWithoutExt(Dest);
            end;

    for  i:=1 to Length(DestFile) do // a filename cannot contain double-quotes
         if DestFile[i]=DOUBLE_QUOTE then DestFile[i]:=QUOTE;

    if   (Task<>ftNewFolder) and
         (DestFile<>'') and (ExtractFileExt(DestFile)='') and
         (not (FileExists(DestFile) or DirectoryExists(DestFile))) then begin
         if   Task=ftNewPlayList then
              DestFile:=DestFile+PLAYLIST_FILE_EXT
         else if MainForm.SokoFile.IsASokobanFile(SourceFile) then
                 DestFile:=DestFile+SOKOBAN_FILE_NAME_EXT;
         end;
  end;

  function  AbortIfNameExists:Boolean;
  var Name:String;
  begin
    Result:=FileExists(DestFile) or DirectoryExists(DestFile);
    Name:=DestFile;

    case TransferType of
      TT_FILE_FILE        : if  Result and
                                (Task in [ftRename,ftCopy,ftMove,ftSaveAs]) then begin
                                Result:=Application.MessageBox(PChar(Format(OverwriteExistingItemText__,[Name])),PChar(Caption),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)<>ID_YES;
                                end;
      TT_PACKITEM_FILE    :;
      TT_FILE_PACKITEM,
      TT_PACKITEM_PACKITEM: if   Result and
                                 MainForm.SokoFile.IsASokobanFile(DestFile) then begin
                                 if   (TransferType=TT_FILE_PACKITEM) or (TransferType=TT_PACKITEM_PACKITEM) then
                                      Name:=MakeIniFileSectionFileName(DestFile,DestPackItem)
                                 else Name:=Name3;
                                 Result:=MainForm.SokoFile.Levels.ItemExists(DestPackItem);
                                 if Result then begin
                                    Result:=Application.MessageBox(PChar(Format(OverwriteExistingItemText__,[Name])),PChar(Caption),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)<>ID_YES;
                                    if not Result then with MainForm.SokoFile do begin // overwrite the existing level: remove it so the new level doesn't inherit old notes and/or old solutions
                                       Levels.Remove(Levels.GetItemByName(DestPackItem),True);
                                       Modified:=True; Close;
                                       end;
                                    end;
                                 end
                            else Result:=Application.MessageBox(PChar(Format(OverwriteExistingItemText__,[Name])),PChar(Caption),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)<>ID_YES;
      TT_FILE             : if   Result then
                                 Result:=Application.MessageBox(PChar(Format(OverwriteExistingItemText__,[Name])),PChar(Caption),MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)<>ID_YES;
    end; //case

  end;

  function  CopyLevel(RenameLevel__:Boolean):Boolean;
  var b:Boolean; Source,Dest:String; Game:TGame;
  begin
    try    Game:=TGame.Create;
           try     if   (TransferType=TT_PACKITEM_FILE) or (TransferType=TT_PACKITEM_PACKITEM) then
                        Source:=MakeIniFileSectionFileName(SourceFile,SourcePackItem)
                   else Source:=Edit1.Text;
                   if   (TransferType=TT_FILE_PACKITEM) or (TransferType=TT_PACKITEM_PACKITEM) then
                        Dest  :=MakeIniFileSectionFileName(DestFile,DestPackItem)
                   else begin Dest:=Name3; //Edit2.Text:=Dest;
                        end;
                   Result     :=Game.LoadFromFileOrClipboard(Source,nil,nil,b) and
                                Game.SaveToFile(Dest,True);

                   if Result and RenameLevel__ then begin
                      MainForm.SaveSnapshotToLogFile (Source,Dest,''); // this doesn't really save a snapshot to the log file but records a "rename" operation
                      MainForm.Solver   .RenameLevels(Source,Dest);
                      MainForm.Optimizer.RenameLevels(Source,Dest);
                      end;
                   MainForm.Solver   .RenamePlugins(Source,Dest);
                   MainForm.Optimizer.RenamePlugins(Source,Dest);
           finally Game.Free;
           end;
    except on E:Exception do Result:=Error(E.Message,Caption);
    end;
  end;

  function  DeleteLevel:Boolean;
  begin
    if (TransferType=TT_FILE_PACKITEM) and IsASingleLevelFile then
       Result:=SysUtils.DeleteFile(SourceFile)
    else with MainForm.SokoFile do begin
      Result:=Open(SourceFile);
      if Result then begin
         Levels.Remove(Levels.GetItemByName(SourcePackItem),True);
         SynchronizeLevelNamesAndKeywordTitles;
         Modified:=True; Flush;
         if Levels.Count=0 then begin
            Close;
            SysUtils.DeleteFile(SourceFile);
            end;
         end;
      end;
  end;

  function  MoveLevel:Boolean;
  begin
    Result:=CopyLevel(True) and DeleteLevel;
  end;

  function  RenameLevel:Boolean;
  var Node:TNode; s,s1:String;
  begin
    Result:=MainForm.SokoFile.Open(SourceFile);
    if Result then begin
       if not StrEqual(SourcePackItem,DestPackItem) then begin
          Node:=MainForm.SokoFile.Levels.GetItemByName(DestPackItem);   // find existing item with the new name, if any
          if Node<>nil then MainForm.SokoFile.Levels.Remove(Node,True); // delete it so the renaming can take place
          end;
       Node:=nil;
       Result:=MainForm.SokoFile.Levels.RenameItem(SourcePackItem,DestPackItem);
       if Result then Node:=MainForm.SokoFile.Levels.GetItemByName(DestPackItem);
       if (Node<>nil) and (Node.ObjectType=otLevel) then
          if TLevel(Node).Notes.Lines.ReadString (KEY_TITLE,s) then
             TLevel(Node).Notes.Lines.WriteString(KEY_TITLE,DestPackItem);
       end;
    if Result then with MainForm.SokoFile do begin
       SynchronizeLevelNamesAndKeywordTitles;
       Modified:=True; Flush;
       s :=MakeIniFileSectionFileName(SourceFile,SourcePackItem);
       s1:=MakeIniFileSectionFileName(SourceFile,DestPackItem);
       MainForm.SaveSnapshotToLogFile (s,s1,''); // this doesn't really save a snapshot to the log file but records a "rename" operation
       MainForm.Solver   .RenameLevels(s,s1);
       MainForm.Optimizer.RenameLevels(s,s1);
       MainForm.Solver   .RenamePlugins(s,s1);
       MainForm.Optimizer.RenamePlugins(s,s1);
       end;
  end;

  function  CopyFile:Boolean;
  begin
    if   TransferType=TT_FILE_FILE then
         Result:=Windows.CopyFile(PChar(SourceFile),PChar(DestFile),False)
    else Result:=CopyLevel(False);
  end;

  function  MoveFile:Boolean;
  begin
    if   TransferType=TT_FILE_FILE then
         begin if AnsiCompareText(SourceFile,DestFile)<>0 then begin
                  SysUtils.DeleteFile(DestFile); RemoveDir(DestFile);
                  end;
               Result:=Misc_.MoveFileOrFolder(SourceFile,DestFile);
               if Result then begin
                  if StrEqual(SourceFile,MainForm.SokoFile.Name) then
                     MainForm.SokoFile.SetName(DestFile);
                  if StrEqual(SourceFile,OpenForm.Game.SokoFile.Name) then
                     OpenForm.Game.SokoFile.SetName(DestFile);
                  if StrEqual(SourceFile,MainForm.Game.SokoFileName) then
                     MainForm.Game.SokoFileName:=DestFile;
                  if StrEqual(SourceFile,OpenForm.Game.SokoFileName) then
                     OpenForm.Game.SokoFileName:=DestFile;
                  if StrEqual(SourceFile,ExtractIniFileName(MainForm.Game.FileName)) then
                     MainForm.Game.FileName:=MakeIniFileSectionFileName(DestFile,ExtractSectionName(MainForm.Game.FileName));
                  if StrEqual(SourceFile,ExtractIniFileName(OpenForm.Game.FileName)) then
                     OpenForm.Game.FileName:=MakeIniFileSectionFileName(DestFile,ExtractSectionName(OpenForm.Game.FileName));
                  if StrEqual(SourceFile,ExtractIniFileName(OpenForm.BaseFileName)) then
                     OpenForm.BaseFileName:=MakeIniFileSectionFileName(DestFile,ExtractSectionName(OpenForm.BaseFileName));
                  if StrEqual(SourceFile,OpenForm.BaseFileName) then
                     OpenForm.BaseFileName:=DestFile;
                  MainForm.SaveSnapshotToLogFile   (SourceFile,DestFile,''); // this doesn't really save a snapshot to the log file but records a "rename" operation
                  MainForm .Solver   .RenameLevels (SourceFile,DestFile);
                  MainForm .Solver   .RenamePlugins(SourceFile,DestFile);
                  MainForm .Optimizer.RenameLevels (SourceFile,DestFile);
                  MainForm .Optimizer.RenamePlugins(SourceFile,DestFile);
                  end;
         end
    else Result:=MoveLevel;
  end;

begin // Perform
  Result:=False; Name3:=''; MainForm.SokoFile.Close;
  Caption:=Application.Title+' - '+Format(FileTaskText__[Task],[FileTaskItemTypeText[TaskItemType]]);
  if   IsAPackItem and (Task=ftRename) then begin
       CharFilter:=FILE_NAME_AND_PATH_NAME_ILLEGAL_CHARACTERS+LEFT_BRACKET+RIGHT_BRACKET;
       for i:=1 to Length(CharFilter) do // '"', and "/", are OK in a collection-item name
           if (CharFilter[i]=DOUBLE_QUOTE) or (CharFilter[i]=SLASH) then
              CharFilter[i]:=TAB;
       end
  else CharFilter:='';
  if IsAPackItem and (PackFileName='') and IsAnIniFileSectionFileName(CurrentFileName) then
     PackFileName:=ExtractIniFileName(CurrentFileName);

  Edit1.Text:=Trim(Name1);
  Edit2.Text:=Trim(Name2);
  if (Edit2.Text='') then Edit2.Text:=Edit1.Text;
  if (Task>=ftNewFolder) then Edit1.Text:='';

  UseSaveDialog:=(Task<>ftDelete) and (not (IsAPackItem and (Task=ftRename)));

  if UseSaveDialog then with SaveDialog1 do begin
     if   Edit1.Text='' then Title:=Caption
     else Title:=Caption+' - '+ExtractFileName(Edit1.Text);
     InitialDir:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Edit2.Text));
     s:=ExtractFileName(Edit2.Text);
     for i:=1 to Length(s) do // kludge: a filename cannot contain double-quotes
         if s[i]=DOUBLE_QUOTE then s[i]:=QUOTE;
     s:=StrRemoveCharacters(s,FILE_NAME_AND_PATH_NAME_ILLEGAL_CHARACTERS+LEFT_BRACKET+RIGHT_BRACKET);
     FileName:=s;
     end
  else begin
     Edit1.Visible :=(Task<ftNewFolder);
     Edit2.Visible :=Task<>ftDelete;
     Label1.Visible:=Edit1.Visible and (Task<>ftDelete);
     Label2.Visible:=(Task<>ftDelete) and (Task<ftNewFolder);
     Label3.Visible:=Task=ftDelete;
     Label4.Visible:=Task>=ftNewFolder;
     Label5.Visible:=Task=ftDelete;
     ActiveControl :=BtnOk;
     if Edit2.Visible then Edit2.SelLength:=Length(Edit2.Text);
     SaveDialog1.FileName:=Edit1.Text;
     Edit1.Text:=FileCtrl.MinimizeName(Edit1.Text,Canvas,Edit1.ClientWidth);
     end;

  if (     UseSaveDialog  and  SaveDialog1.Execute ) or
     ((not UseSaveDialog) and (ShowModal=mrOk     )) then begin
     if   UseSaveDialog then begin
          s:=CharFilter; CharFilter:=LEFT_BRACKET+RIGHT_BRACKET;
          Edit2.Text:=SaveDialog1.FileName;
          CharFilter:=s;
          end
     else Edit1.Text:=SaveDialog1.FileName;
     Edit2.Text:=Trim(Edit2.Text);
     if IsAPackItem and (Task=ftRename) then
        Edit2.Text:=TextThatCannotBeInterpretedAsMoves(Edit2.Text);
     if (not IsAPackItem) and
        (ExtractFilePath(Edit2.Text) ='') and
        (ExtractFilePath(Edit1.Text)<>'') then
        Edit2.Text:=StrWithTrailingPathDelimiter(ExtractFilePath(Edit1.Text))+Edit2.Text;

     s:=Edit2.Text;
     if UseSaveDialog then
        for i:=1 to Length(s) do
            if s[i]=QUOTE then s[i]:=DOUBLE_QUOTE; // convert ' to "

     if (Task=ftDelete)
        or
        ((AnsiCompareText(Edit1.Text,Edit2.Text)<>0) and
         (AnsiCompareText(Edit1.Text,s)<>0) // new value matches old value except for quotes and double-quotes
        )
        or
        ((Task=ftRename)
         and
         (AnsiCompareStr(Edit1.Text,Edit2.Text)<>0) and
         (AnsiCompareStr(Edit1.Text,s)<>0) // new value matches old value except for quotes and double-quotes
        )
        then begin
        if ((Task=ftNewPackFile)
            or
            (Task=ftSaveAs)
           )
           and
           (ExtractFileExt(Edit2.Text)='') then
           Edit2.Text:=Edit2.Text+SOKOBAN_FILE_NAME_EXT;

        CalculateTransferType(Edit1.Text,Edit2.Text,TransferType,SourceFile,SourcePackItem,DestFile,DestPackItem);
        Result:=True; OK:=True;
        if        Task=ftDelete then // update 'Name3' with  the return value
                  Name3:=Edit2.Text
        else if   DestPackItem='' then
                  Name3:=DestFile
             else Name3:=MakeIniFileSectionFileName(DestFile,DestPackItem);

        case Task of
          ftCopy   : if   AbortIfNameExists then
                          Result:=False
                     else if   IsAPackItem then
                               OK:=CopyLevel(False)
                          else OK:=CopyFile;
          ftMove   : if   AbortIfNameExists then
                          Result:=False
                     else if   IsAPackItem then
                               OK:=MoveLevel
                          else
//                             OK:=MoveFileEx(PChar(Edit1.Text),PChar(Edit2.Text),MOVEFILE_COPY_ALLOWED+MOVEFILE_REPLACE_EXISTING+MOVEFILE_WRITE_THROUGH);
                               OK:=MoveFile;
          ftDelete : if IsAPackItem then begin
                        SourceFile:=DestFile; SourcePackItem:=DestPackItem;
                        OK:=DeleteLevel;
                        end
                     else begin
                        OK:=SysUtils.DeleteFile(Name3);
                        if not OK then begin
                           if StrEqual(Edit2.Text,GetCurrentDir) then begin
                              Edit1.Text:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Name3));
                              SetCurrentDir(Edit1.Text);
                              OK:=SysUtils.RemoveDir(Name3);
                              if not OK then
                                 Result:=Error(TEXT_TASK_FAILED+NL+NL+DirectoryMustBeEmptyBeforeDeletionText,Caption)
                              end
                           else
                              OK:=SysUtils.RemoveDir(Name3);
                           end;
                        end;
          ftRename : if   (AnsiCompareText(Edit1.Text,Edit2.Text)<>0)
                          and
                          (AnsiCompareText(Edit1.Text,s)<>0) // new value matches old value except for quotes and double-quotes
                          and
                          AbortIfNameExists then
                          Result:=False
                     else if IsAPackItem then
                             OK:=RenameLevel
                          else
  //                         OK:=Windows.MoveFileEx(PChar(Edit1.Text),PChar(Name3),MOVEFILE_COPY_ALLOWED+MOVEFILE_REPLACE_EXISTING+MOVEFILE_WRITE_THROUGH);
                             OK:=MoveFile;
          ftNewFolder
                   : if DirectoryExists(Name3) or
                        FileExists(Name3) then begin
                        Application.MessageBox(PChar(Format(NameAlreadyExistsText__,[Name3])),PChar(Caption),MB_OK+MB_ICONERROR);
                        Result:=False;
                        end
                     else begin
                        SysUtils.DeleteFile(PChar(Name3));
                        RemoveDir(Name3);
                        OK:=CreateDirectory(PChar(Name3),nil);
                        end;
          ftNewPackFile
                   : if AbortIfNameExists then Result:=False
                     else
                        try
                          SysUtils.DeleteFile(PChar(Name3));
                          RemoveDir(Name3);
                          AssignFile(f,Name3); Rewrite(f);
                          try     for i:=Low(DEFAULT_FILE_FORMAT_DESCRIPTION) to High(DEFAULT_FILE_FORMAT_DESCRIPTION) do
                                      if        AnsiPos(KEY_DATE_CREATED,PChar(DEFAULT_FILE_FORMAT_DESCRIPTION[i]))=1 then
                                                Writeln(f,KEY_DATE_CREATED,': ',FormatDateTime(FORMAT_DATE_TIME,Now))
                                      else if   AnsiPos(KEY_DATE_LAST_CHANGE,DEFAULT_FILE_FORMAT_DESCRIPTION[i])=1 then
                                                Writeln(f,KEY_DATE_LAST_CHANGE,': ',FormatDateTime(FORMAT_DATE_TIME,Now))
                                           else Writeln(f,DEFAULT_FILE_FORMAT_DESCRIPTION[i]);
                                  Writeln(f,'');
                                  Writeln(f,'');
                                  Writeln(f,TEXT_LEVEL);
                                  Writeln(f,'');
                                  Writeln(f,'###');
                                  Writeln(f,'#@#');
                                  Writeln(f,'###');
                                  Writeln(f,''   );
                                  Writeln(f,DummyLevelNotesLine1Text);
                                  Writeln(f,DummyLevelNotesLine2Text);
                          finally CloseFile(f);
                          end;
                        except
                          on E:Exception do begin
                             Error(E.Message,Caption);
                             Result:=False;
                             end;
                        end;
          ftNewPlayList,
          ftNewTextFile
                   : if   AbortIfNameExists then Result:=False
                     else try
                            SysUtils.DeleteFile(PChar(Name3));
                            RemoveDir(Name3);
                            AssignFile(f,Name3); Rewrite(f);
                            try     if Task=ftNewPlayList then
                                       Writeln(f,LEFT_BRACKET+PLAYLIST_INIFILE_SECTION+RIGHT_BRACKET);
                            finally CloseFile(f);
                            end;
                          except on E:Exception do begin
                                    Error(E.Message,Caption);
                                    Result:=False;
                                    end;
                          end;
          ftSaveAs : if   AbortIfNameExists then
                          Result:=False
                     else if        IsAPackItem then
                                    OK:=CopyLevel(False) {not implemented; or rather, it hasn't been tested}
                          else if   SourceFile<>'' then
                                    OK:=CopyFile         {not implemented; or rather, it hasn't been tested}
                               else OK:=True;            {no source file: the caller is assumed to have called 'Perform' only to get a destination file-name}
        end; // case
        if Result and (not OK) then Result:=Error(TEXT_TASK_FAILED,Caption);
        end;
     end;
end;

procedure TFileForm.Edit2Change(Sender: TObject);
var i:Integer;
begin
  for i:=1 to Length(CharFilter) do Edit2.Text:=StrRemoveChar(Edit2.Text,CharFilter[i]);
end;

end.
