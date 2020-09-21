unit MPlayer2_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  {$WARNINGS OFF}
  FileCtrl, // Warning: Unit 'FileCtrl' is specific to a platform
  {$WARNINGS ON}
  ComCtrls, Buttons,
  Misc_,Music_,MPlayer1_,IniFile_;

type
  TDirTaskInfo = record
    Dir,
    Filter:String;
    FilterIndex:Integer;
    end;

  TMPlayerColor = (mcPanelPlayer,mcPanelSource,mcPanelList);

type
  TMPlayerForm = class(TForm)
    StatusBar1: TStatusBar;
    PanelSource: TPanel;
    PanelLeft: TPanel;
    GroupBoxMenu: TGroupBox;
    GroupBoxSource: TGroupBox;
    RadioButtonCD: TRadioButton;
    RadioButtonFolder: TRadioButton;
    RadioButtonPlaylist: TRadioButton;
    ComboBoxCD: TComboBox;
    ComboBoxFolder: TComboBox;
    ComboBoxPlaylist: TComboBox;
    PanelNew: TPanel;
    PanelSaveAs: TPanel;
    PanelAdd: TPanel;
    PanelRemove: TPanel;
    PanelClear: TPanel;
    PanelDeleteFiles: TPanel;
    PanelHelp: TPanel;
    PanelExit: TPanel;
    PanelBrowseMap: TPanel;
    PanelBrowsePlayList: TPanel;
    PanelPlayer: TPanel;
    GroupBoxPlayer: TGroupBox;
    EditPlay: TEdit;
    ScrollBarPosition: TScrollBar;
    LabelPlay: TLabel;
    PanelPlay: TPanel;
    OpenDialog1: TOpenDialog;
    PanelDummy: TPanel;
    ListBox1: TListBox;
    Timer1: TTimer;
    FileListBox1: TFileListBox;
    ScrollBarVolume: TScrollBar;
    LabelVolume: TLabel;
    PanelStop: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelExitClick(Sender: TObject);
    procedure RadioButtonMusicSourceClick(Sender: TObject);
    procedure PanelBrowseClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure ComboBoxCDEnter(Sender: TObject);
    procedure ComboBoxFolderEnter(Sender: TObject);
    procedure ComboBoxPlaylistEnter(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure OpenDialog1FolderChange(Sender: TObject);
    procedure ComboBoxMusicSourceExit(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure PanelRemoveClick(Sender: TObject);
    procedure PanelClearClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure PanelPlayPauseClick(Sender: TObject);
    procedure ListBox1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure ScrollBarPositionScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RadioButtonMusicSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelHelpClick(Sender: TObject);
    procedure ScrollBarVolumeScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure RadioButtonMusicSourceDblClick(Sender: TObject);
    procedure PanelStopClick(Sender: TObject);
  protected
    procedure ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
  private
    { Private declarations }
    LastFocusedControl:TWinControl;
    Music:TMusicManager;
    MPlayer:TMPlayer;
    MusicFileLoaded:Boolean;
    TaskInfo:array[TMPlayerDirTask] of TDirTaskInfo;

    procedure AddItemOrMoveItemToTopOfComboBox(var ComboBox: TComboBox;
      const Item: String; UpdateFileList__:Boolean);
    procedure ApplicationActivate(Sender: TObject);
    procedure ApplicationDeActivate(Sender: TObject);
    procedure ComboBoxGroupSetColors(Sender: TComboBox);
    procedure DisplayHint(Sender: TObject);
    procedure EnableDisableButtons;
    function  ExecuteOpenDialog(Sender: TObject): Boolean;
    function  FlushPlayList:Boolean;
    procedure InitScrollBarPosition;
//  procedure LoadSettings;
    procedure MPlayerNotify(Sender: TObject);
    procedure MusicSourceToRadioButton(MusicSource: TMusicSource);
    procedure RefreshData(UpdateFileList__:Boolean);
//  procedure SaveSettings;
    procedure SetDefaultValues;
    procedure SetControlColor(Control:TWinControl; TextColor__,BackgroundColor__:TColor);
    procedure SetFormColors;
    procedure ShowStatus;
    function  TaskIndexOf(Sender: TObject): TMPlayerDirTask;
    procedure UpdateFileList;

  public
    { Public declarations }
    FormColors:TFormColors;

    function  LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    function  SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
  end;

var
  MPlayerForm: TMPlayerForm = nil;

implementation

{$R *.DFM}

uses MPlayer,SokUtil_,Text_,Main_,Help_;

const
  MAX_MUSIC_PLAYER_HISTORY_ITEMS   = 25;
  MUSIC_PLAYER_FORM_INIFILE_SECTION= 'MusicPlayerForm'; // don't localize
  TAG_BUTTON                       = 1; // all panels working as buttons are tagged with this number
  TIMER_INTERVAL_MILLI_SECONDS     = 500;

procedure TMPlayerForm.FormCreate(Sender: TObject);
var i:Integer; s:String; Task:TMPlayerDirTask;
begin
  StatusBar1.Font.Assign(Self.Font);
  PanelStop.Hint   :=BAR+Text_.MusicPlayerText[Succ(2*Ord(MPlayer1_.btStop))];

  for i:=0 to Pred(ComponentCount) do
      if      Components[i] is TPanel       then with Components[i] as TPanel       do begin OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TButton      then with Components[i] as TButton      do begin OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TRadioButton then with Components[i] as TRadioButton do begin OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TComboBox    then with Components[i] as TComboBox    do begin OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TGroupBox    then with Components[i] as TGroupBox    do begin OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TListBox     then with Components[i] as TListBox     do begin OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end
      else if Components[i] is TStatusBar   then with Components[i] as TStatusBar   do begin OnMouseMove:=FormMouseMove; OnMouseUp:=FormMouseUp; end;

//  Music          :=TMusicManager.Create(Self);
//  MPlayer        :=TMPlayer     .Create(nil,PanelDummy);

  s:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
  for Task:=Low(TaskInfo) to High(TaskInfo) do with TaskInfo[Task] do begin
      if Task=dtCD then Dir:=''
      else Dir:=s;
      Filter:=''; FilterIndex:=1;
      end;

  SetDefaultValues;
  FormResize(Sender);
  //LoadSettings;
  MusicFileLoaded:=False;
  ActiveControl:=ListBox1;
end;

procedure TMPlayerForm.FormDestroy(Sender: TObject);
begin
  //SaveSettings;
  if (MainForm=nil) or (Music  <>MainForm.Music  ) then Music.Free;
  if (MainForm=nil) or (MPlayer<>MainForm.MPlayer) then MPlayer.Free;
  MPlayerForm:=nil; // the shut down procedure depends on this
end;

procedure TMPlayerForm.ApplicationActivate(Sender: TObject);
begin
  if MainForm.Music<>nil then MainForm.Music.OnActivateApplication;
  FormActivate(Sender); // to make the right colors;
end;

procedure TMPlayerForm.ApplicationDeActivate(Sender: TObject);
begin
  FormDeactivate(Sender);  // reset color for active control
  if MainForm.Music<>nil then MainForm.Music.OnDeactivateApplication;
end;

procedure TMPlayerForm.FormActivate(Sender: TObject);
var i,j,VisibleLineCount:Integer; s:String;
begin
  MainForm.FormDeactivate(Sender);

  Application.OnActivate   := ApplicationActivate;
  Application.OnDeactivate := ApplicationDeactivate;

  Application.OnHint       :=DisplayHint;
  Application.OnMessage    :=ApplicationOnMessage;

  SetFormColors;
  StatusBar1.SizeGrip      :=IsWindowsDefaultColorBtnFace(StatusBar1.Color) or
                             (ColorToRGB(StatusBar1.Color)=ColorToRGB(clBlack ));

  Timer1.Interval          := TIMER_INTERVAL_MILLI_SECONDS;
  Timer1.OnTimer           := Timer1Timer;

  if MainForm<>nil then begin
     Music:=MainForm.Music; MPlayer:=MainForm.MPlayer;
     end;

  if Music<>nil then begin
     Music.Enabled:=True; Music.FirstTime:=False;
     Music.Volume64Kibi:=Max(0,Music.MasterVolume64Kibi);
     with ScrollBarVolume do Position:=(Max-Min)*Music.Volume64Kibi div 65535;
     if   Music.Playing then ScrollBarPosition.Position:=Music.PositionMilliSeconds;

     if (TaskInfo[dtCD].Dir='') and DirectoryExists(Music.CDDrive+':\') then begin
        TaskInfo[dtCD].Dir:=Music.CDDrive+COLON;
        ComboBoxCDEnter(nil);
        end;

     s:=MusicFilesText+' ('+Music.MusicFileTypeFilter+')|'+
        Music.MusicFileTypeFilter+BAR+AllFilesFilterText;
     if TaskInfo[dtCD].Filter<>s then begin // initialize filters
        with TaskInfo[dtCD      ] do Filter:=s;
        with TaskInfo[dtFolder  ] do Filter:=s;
        with TaskInfo[dtPlayList] do Filter:=PlayListsText +' (*.'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+')|*'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+BAR+AllFilesFilterText;
        with TaskInfo[dtNew     ] do Filter:=TaskInfo[dtPlayList].Filter;
        with TaskInfo[dtSaveAs  ] do Filter:=TaskInfo[dtPlayList].Filter;
        with TaskInfo[dtAdd     ] do Filter:=TaskInfo[dtFolder].Filter;
        with TaskInfo[dtDelete  ] do Filter:=PlayListsText +' (*.'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+')|*'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+
                                       BAR+TaskInfo[dtAdd].Filter;
        end;
     end;

  LastFocusedControl:=nil; FormMouseMove(ActiveControl,[],0,0);
  ActiveControl:=ListBox1;

  if Music<>nil then begin
     if DirectoryExists(StrWithoutTrailingPathDelimiter(Music.MusicFilePath)) then
        AddItemOrMoveItemToTopOfComboBox(ComboBoxFolder,StrWithoutTrailingPathDelimiter(Music.MusicFilePath),False);
     if (Music.PlayListFileName<>'') and FileExists(Music.PlayListFileName) then
        AddItemOrMoveItemToTopOfComboBox(ComboBoxPlayList,Music.PlayListFileName,False);
     MusicSourceToRadioButton(Music.MusicSource);
     end;

  RefreshData(True);

  if Music<>nil then with Music do begin
     if Player<>nil then begin
        EditPlay.Text:=FileCtrl.MinimizeName(Player.FileName,Canvas,EditPlay.Width);
        if PlayList<>nil then with ListBox1 do begin
           VisibleLineCount:=Max(1,ClientHeight div ItemHeight);
           if MusicSource=msPlayList then
                i:=PlayList.IndexOf(Player.FileName)
           else i:=FileList.Items.IndexOf(ExtractFileName(Player.FileName));
           if i>=0 then
              try
                ActiveControl:=PanelExit;
                //ListBox1.Items.BeginUpdate;
                if (i<TopIndex) or (i>=TopIndex+VisibleLineCount) then begin
                   j:=0; while i<=j+VisibleLineCount do Inc(j,VisibleLineCount);
                   TopIndex:=j;
                   end;
                ItemIndex:=i;
              finally
                //ListBox1.Items.EndUpdate;
                ActiveControl:=ListBox1;
              end;
           for i:=0 to Pred(Items.Count) do Selected[i]:=False;
           if (Items.Count>0) and (ItemIndex>=0) then Selected[ItemIndex]:=True;
           Invalidate;
           end;

        end;
     InitScrollBarPosition;
     MusicFileLoaded:=(Player<>nil) and ModeOk(Player.Mode) and (Player.Mode<>mpNotReady);
     Music.CreatePlayer(MPlayerNotify);
     EnableDisableButtons;
     end;
end;

procedure TMPlayerForm.FormDeactivate(Sender: TObject);
begin
//Timer1.Enabled:=False;
  FormMouseMove(nil,[],0,0);
  ActiveControl:=ListBox1; {otherwise, if a combobox has focus, its color will be wrong next time the form is opened}
end;

procedure TMPlayerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled:=False;
  if (Music<>nil) and (MainForm<>nil) and
     (Music=MainForm.Music) then with MainForm.Music do
     if Player<>nil then Player.OnNotify:=MainForm.Music.PlayerNotify;
  FormMouseMove(nil,[],0,0);
end;

procedure TMPlayerForm.FormResize(Sender: TObject);
var i:Integer;
begin
  i:=ListBox1.Left+ComboBoxCD.Width+PanelBrowseMap.Width+16;
  if ClientWidth<i then ClientWidth:=i
  else begin
     i:=PanelLeft.Top+GroupBoxMenu.Top+GroupBoxMenu.Height+StatusBar1.Height+16;
     if ClientHeight<i then ClientHeight:=i
     else begin
        with GroupBoxPlayer       do Width:=Self.ClientWidth-2*Left;
        with PanelPlay            do Left :=GroupBoxPlayer.Width-Width-LabelPlay.Left;
        with PanelStop            do Left :=PanelPlay.Left;
        with EditPlay             do Width:=PanelPlay.Left-Left-LabelPlay.Left;
        with ScrollBarPosition    do begin Left :=EditPlay.Left +1;
                                           Width:=EditPlay.Width-2;
                                     end;
//      with ScrollBarVolume      do Left :=PanelPlay.Left;
        with ScrollBarVolume      do Left :=PanelNew.Left+PanelNew.Width-Width;

//      with ScrollBarVolume      do Left :=EditPlay.Left+EditPlay.Width-Width;
//      with ScrollBarPosition    do Width:=ScrollBarVolume.Left-Left-LabelPlay.Left;

        with GroupBoxSource       do Width:=GroupBoxPlayer.Width;
        with PanelBrowseMap       do Left :=PanelPlay.Left;
        with PanelBrowsePlayList  do Left :=PanelBrowseMap.Left;
        with ComboBoxFolder       do Width:=PanelBrowseMap.Left-Left-RadioButtonFolder.Left;
        with ComboBoxPlayList     do Width:=ComboBoxFolder.Width;
        with ListBox1             do begin Width:=GroupBoxPlayer.Left+GroupBoxPlayer.Width-Left;
                                           Height:=Self.ClientHeight-Top-StatusBar1.Height-GroupBoxSource.Left;
                                     end;
        if Left>Screen.DeskTopLeft+Screen.DeskTopWidth -30 then Left:=Screen.DeskTopLeft+Screen.DeskTopWidth -30;
        if Top >Screen.DeskTopTop +Screen.DeskTopHeight-30 then Top :=Screen.DeskTopTop +Screen.DeskTopHeight-30;
        end;

     end;

end;

procedure TMPlayerForm.DisplayHint(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := GetLongHint(Application.Hint);
end;

procedure TMPlayerForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if              Key=VK_ESCAPE then Close
  else if         Key=VK_F1 then PanelHelpClick(Sender)
  else if         Key=Ord(SPACE) then PanelPlayPauseClick(Sender)
  else if         Key=VK_F6 then
          if      Shift=[ssCtrl] then
                  PanelStopClick(Sender)
          else    PanelPlayPauseClick(Sender)
  else if Shift=[ssAlt] then
          if      (Key=Ord(ACCEL_CHAR_PLAY)) or
                  (Key=Ord(ACCEL_CHAR_PAUSE)) then PanelPlayPauseClick(Sender)
          else if (Key=Ord(ACCEL_CHAR_CLOSE_MPLAYER_FORM)) or
                  (Key=Ord(ACCEL_CHAR_EXIT)) then Close
          else if Key=Ord(ACCEL_CHAR_STOP) then PanelStopClick(Sender)
          else if Key=Ord(ACCEL_CHAR_HELP) then
                  PanelHelpClick(Sender)
          else if (Key=Ord(ACCEL_CHAR_PLAY)) or
                  (Key=Ord(ACCEL_CHAR_PAUSE)) then PanelPlayPauseClick(Sender)
          else
  else;
end;

procedure TMPlayerForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if TWinControl(Sender)<>LastFocusedControl then with FormColors do begin
     if LastFocusedControl<>nil then begin
        SetControlColor(LastFocusedControl,ButtonTextColor,ButtonColor);
        if LastFocusedControl is TPanel then with LastFocusedControl as TPanel do
           if BevelOuter=bvLowered then BevelOuter:=bvRaised;
        end;
     LastFocusedControl:=TWinControl(Sender);
     if LastFocusedControl<>nil then SetControlColor(LastFocusedControl,FocusedButtonTextColor,FocusedButtonColor);
     end;
end;

procedure TMPlayerForm.PanelExitClick(Sender: TObject);
begin
  Close;
end;

function  TMPlayerForm.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var i:Integer; Section:String;
begin  // LoadSettingsFromIniFile;
  Result:=True;
  Section:=MUSIC_PLAYER_FORM_INIFILE_SECTION;

  LoadComboBoxFromIniFile(IniFile,Section+' - '+ComboBoxFolder.Name,MAX_MUSIC_PLAYER_HISTORY_ITEMS,True ,False,True,False,ComboBoxFolder);
  LoadComboBoxFromIniFile(IniFile,Section+' - '+ComboBoxPlayList.Name,MAX_MUSIC_PLAYER_HISTORY_ITEMS,False,True,True,False,ComboBoxPlayList);
  with ComboBoxFolder   do if Items.Count>0 then AddItemOrMoveItemToTopOfComboBox(ComboBoxFolder  ,Items[0],False);
  with ComboBoxPlayList do if Items.Count>0 then AddItemOrMoveItemToTopOfComboBox(ComboBoxPlayList,Items[0],False);
  for i:=0 to Pred(ComboBoxFolder  .Items.Count) do SetComboBoxDropDownWidth(ComboBoxFolder  ,i,False);
  for i:=0 to Pred(ComboBoxPlayList.Items.Count) do SetComboBoxDropDownWidth(ComboBoxPlayList,i,False);

  LoadFormColorsFromIniFile(IniFile,Section,FormColors);
end;

function  TMPlayerForm.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
var Section,SectionFolder,SectionPlayList:String;
begin // SaveSettingsToIniFile;
  try    Section        :=MUSIC_PLAYER_FORM_INIFILE_SECTION;
         SectionFolder  :=Section+' - '+ComboBoxFolder  .Name;
         SectionPlayList:=Section+' - '+ComboBoxPlayList.Name;

         SaveComboBoxToIniFile(IniFile,SectionFolder  ,MAX_MUSIC_PLAYER_HISTORY_ITEMS,ComboBoxFolder);
         SaveComboBoxToIniFile(IniFile,SectionPlayList,MAX_MUSIC_PLAYER_HISTORY_ITEMS,ComboBoxPlayList);
         SaveFormColorsToIniFile(IniFile,Section,FormColors);
         Result:=True;
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

procedure TMPlayerForm.MusicSourceToRadioButton(MusicSource:TMusicSource);

  procedure SetColor(RadioButton:TRadioButton);
  begin
    with RadioButton do with FormColors do
      if   Checked then
           begin Font.Color:=HighlightedTextColor; end
      else begin Font.Color:=BackgroundTextColor; end;
  end;

begin  // MusicSourceToRadioButton
  RadioButtonCD      .Checked:=MusicSource=msCD;
  RadioButtonFolder  .Checked:=MusicSource=msFolder;
  RadioButtonPlayList.Checked:=MusicSource=msPlayList;

  SetColor(RadioButtonCD);
  SetColor(RadioButtonFolder);
  SetColor(RadioButtonPlayList);

  if      RadioButtonCD      .Checked then begin
          ComboBoxGroupSetColors(ComboBoxCD);
          end
  else if RadioButtonFolder  .Checked then begin
          ComboBoxGroupSetColors(ComboBoxFolder);
          end
  else if RadioButtonPlayList.Checked then begin
          ComboBoxGroupSetColors(ComboBoxPlayList);
          end
  else    ComboBoxGroupSetColors(nil);
end;

procedure TMPlayerForm.RadioButtonMusicSourceClick(Sender: TObject);
var oMusicSource:TMusicSource;
begin
  oMusicSource:=Music.MusicSource;

  if      Sender=RadioButtonCD       then Music.MusicSource:=msCD
  else if Sender=RadioButtonFolder   then Music.MusicSource:=msFolder
  else if Sender=RadioButtonPlayList then Music.MusicSource:=msPlayList;
  MusicSourceToRadioButton(Music.MusicSource);

  if (oMusicSource<>Music.MusicSource) and
     (Sender<>nil) then // for safety, always update, unless 'Sender=nil'
     UpdateFileList;
end;

procedure TMPlayerForm.RadioButtonMusicSourceDblClick(Sender: TObject);
begin //
  if (Music<>nil) and (Music.PlayList<>nil) and
     (Music.PlayList.Count>0) and Music.OpenPrevNextTrack(0) then begin
     InitScrollBarPosition;
     Music.Play;
     end;
  EnableDisableButtons;
end;


function TMPlayerForm.TaskIndexOf(Sender:TObject):TMPlayerDirTask;
begin
  if      Sender=PanelBrowseMap      then Result:=dtFolder
  else if Sender=PanelBrowsePlayList then Result:=dtPlayList
  else if Sender=PanelNew            then Result:=dtNew
  else if Sender=PanelSaveAs         then Result:=dtSaveAs
  else if Sender=PanelAdd            then Result:=dtAdd
  else if Sender=PanelDeleteFiles    then Result:=dtDelete
  else if Sender=RadioButtonCD       then Result:=dtCD
  else if Sender=RadioButtonFolder   then Result:=dtFolder
  else if Sender=RadioButtonPlayList then Result:=dtPlayList
  else if Sender=ComboBoxCD          then Result:=dtCD
  else if Sender=ComboBoxFolder      then Result:=dtFolder
  else if Sender=ComboBoxPlayList    then Result:=dtPlayList
  else Result:=dtFolder;
end;

function TMPlayerForm.ExecuteOpenDialog(Sender:TObject):Boolean;
var i:Integer; Task:TMPlayerDirTask; s:String; //Buf:array[0..Windows.MAX_PATH] of WChar;
begin
  if Music<>nil then with OpenDialog1 do begin
     Timer1.Enabled:=False;
     Task:=TaskIndexOf(Sender);
     Title:=Self.Caption+' - '+MusicPlayerDirTaskText[Task];
     if   Task in [dtNew,dtSaveAs] then
          TaskInfo[Task].Dir:=TaskInfo[dtPlayList].Dir;
     if   DirectoryExists(TaskInfo[Task].Dir) then
          InitialDir:=TaskInfo[Task].Dir
     else InitialDir:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
     Filter:=TaskInfo[Task].Filter;
     FilterIndex:=TaskInfo[Task].FilterIndex;
     FileName:='';
     if   Task in  [dtFolder,dtNew,dtSaveAs] then
          Options:=Options-[ofFileMustExist]
     else Options:=Options+[ofFileMustExist];
     if   Task in  [dtAdd,dtDelete] then
          Options:=Options+[ofAllowMultiSelect]
     else Options:=Options-[ofAllowMultiSelect];
     if   Task in [dtNew,dtSaveAs] then FileName:=PlayListNewFileName(InitialDir)
     else if Task=dtFolder then OpenDialog1FolderChange(Self);
     if Task in [dtAdd,dtDelete] then
        StatusBar1.Panels[1].Text:=GetLongHint(SelectMultipleFilesHint);

//   if   Task in [dtMap] then begin
          //Result:=SelectDirectory(s,[],0);
          //Result:=SelectDirectory(Title,StringToWideChar(InitialDir,Buf,SizeOf(Buf) div 2),s);
//        end
//   else Result:=Execute;

     Result:=Execute;

     StatusBar1.Panels[1].Text:='';

     TaskInfo[Task].FilterIndex:=FilterIndex;

     RefreshData(Result=False);

     if Result then begin
        s:=StrWithoutTrailingPathDelimiter(ExtractFilePath(FileName));
        if DirectoryExists(s) then TaskInfo[Task].Dir:=s;

        case Task of
          dtCD      : begin Music.MusicSource:=msCD; RadioButtonMusicSourceClick(ComboBoxCD);
                            AddItemOrMoveItemToTopOfComboBox(ComboBoxCD,TaskInfo[dtCD].Dir,True);
                      end;
          dtFolder  : begin Music.MusicSource:=msFolder; RadioButtonMusicSourceClick(ComboBoxFolder);
                            AddItemOrMoveItemToTopOfComboBox(ComboBoxFolder,TaskInfo[dtFolder].Dir,True);
                      end;
          dtPlaylist: begin Music.MusicSource:=msPlayList; RadioButtonMusicSourceClick(ComboBoxPlayList);
                            AddItemOrMoveItemToTopOfComboBox(ComboBoxPlayList,FileName,True);
                      end;
          dtNew,
          dtSaveAs  : if FileExists(FileName) and
                         (Application.MessageBox(
                           PChar(Format(FileExistsText__,[FileName])+NL+NL+OverwriteItText),
                           PChar(Title),
                           MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)
                          <> IDYES) then UpdateFileList
                       else begin
                         if      Task=dtNew then s:=''
                         else if Music.MusicSource=msCD     then s:=Music.CDDrive+COLON
                         else if Music.MusicSource=msFolder then s:=StrWithoutTrailingPathDelimiter(Music.MusicFilePath)
                         else s:='';

                         Music.MusicSource:=msPlayList; // avoid that 'RadioButtonMusicSourceClick' updates filelistbox
                         RadioButtonMusicSourceClick(nil); // kludge: 'nil' avoids that filelist is updated
                         if Task=dtNew then Music.PlayList.Clear;
                         if PlayListSaveToFile(FileName,s,Music.PlayList) then begin
                            AddItemOrMoveItemToTopOfComboBox(ComboBoxPlayList,FileName,True);
                            end
                         else UpdateFileList;
                         end;
          dtAdd     : if Music.PlayList<>nil then begin
                         for i:=Pred(Files.Count) downto 0 do
                             if Music.PlayList.IndexOf(Files[i])=-1 then begin // reverse order = order the user selected the files
                                Music.PlayList.Add(Files[i]);
                                ListBox1.Items.Add(Music.PrettyTrackName(Files[i]));
                                end;
                         FlushPlayList;
                         end;

          dtDelete  : begin
                        if   Files.Count=1 then
                             if   FileExists(Files[0]) then
                                  s:=DeleteFileQuestionText+NL+' "'+Files[0]+'" ?'
                             else s:=''
                        else begin s:=DeleteFilesQuestionText+NL+
                                      ' "'+Files[0]+'"'+NL+
                                      ' "'+Files[1]+'"';
                                   if Files.Count>2 then s:=s+NL+'... ?'
                                   else s:=s+' ?';
                             end;
                        if (s<>'') and
                           (Application.MessageBox(PChar(s+NL+NL+NoUndoText),PChar(Application.Title+' - '+DeleteFilesText),MB_ICONQUESTION+MB_YESNO+MB_DEFBUTTON2)=IDYES) then
                           for i:=0 to Pred(Files.Count) do
                               if FileExists(Files[i]) then
                                  if (not DeleteFile(PChar(Files[i]))) then
                                     MessageDlg(Format(TEXT_FILE_DELETE_FAILED_FORMAT,[Files[i]]),mtInformation,[mbOK],0);
                        RefreshData(True);
                      end;
        end; // case
        end;
     end
  else Result:=False;
end;

procedure TMPlayerForm.PanelBrowseClick(Sender: TObject);
begin
  if Sender is TPanel then with Sender as TPanel do with FormColors do
     if not (Font.Color=GrayedButtonTextColor) then ExecuteOpenDialog(Sender);
end;

procedure TMPlayerForm.AddItemOrMoveItemToTopOfComboBox(var ComboBox:TComboBox;
            const Item:String; UpdateFileList__:Boolean);
begin
  with ComboBox do begin
    if (Items.Count=0) or (not StrEqual(Item,Items[0])) then
       Misc_.AddItemOrMoveItemToTopOfComboBox(ComboBox,MAX_MUSIC_PLAYER_HISTORY_ITEMS,AbbreviatedFilePath(Item,MainForm.MyDocumentsFolder),False);

    if           Items.Count>0 then
         if      ComboBox=ComboBoxCD       then begin
                 TaskInfo[dtCD].Dir:=ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder);
                 if (TaskInfo[dtCD].Dir<>'') and (Music<>nil) then
                    Music.CDDrive:=TaskInfo[dtCD].Dir[1];
                 end
         else if ComboBox=ComboBoxFolder   then begin
                 TaskInfo[dtFolder].Dir:=ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder);
                 if Music<>nil then Music.MusicFilePath:=TaskInfo[dtFolder].Dir;
                 end
         else if ComboBox=ComboBoxPlayList then begin
                 TaskInfo[dtPlayList].Dir:=StrWithoutTrailingPathDelimiter(ExtractFilePath(ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder)));
                 if Music<>nil then Music.PlayListFileName:=ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder);
                 end
         else
    else if      ComboBox=ComboBoxFolder   then begin
                 TaskInfo[dtFolder].Dir:=StrWithoutTrailingPathDelimiter(MainForm.Music.WindowsMediaPath);
                 if TaskInfo[dtFolder].Dir='' then
                    TaskInfo[dtFolder].Dir:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
                 if Music<>nil then Music.MusicFilePath:='';
                 end
         else if ComboBox=ComboBoxPlayList then begin
                 TaskInfo[dtPlayList].Dir:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
                 if Music<>nil then Music.PlayListFileName:='';
                 end;

    if UpdateFileList__ then UpdateFileList;
    end;
end;

procedure TMPlayerForm.ComboBoxChange(Sender: TObject);
begin
  if Sender is TComboBox then with Sender as TComboBox do
     if (Items.Count>0) and (ItemIndex>=0) then
        AddItemOrMoveItemToTopOfComboBox(TComboBox(Sender),ExpandedFilePath(Items[ItemIndex],MainForm.MyDocumentsFolder),True);
end;

procedure TMPlayerForm.ComboBoxGroupSetColors(Sender:TComboBox);
begin
  with FormColors do begin
    with ComboBoxCD       do begin Font.Color:=WindowTextColor;  Color:=WindowColor; end;
    with ComboBoxFolder   do begin Font.Color:=WindowTextColor;  Color:=WindowColor; end;
    with ComboBoxPlayList do begin Font.Color:=WindowTextColor;  Color:=WindowColor; end;
    if (Sender<>nil) and  (ActiveControl<>Sender) then
       with Sender        do begin Font.Color:=FocusedWindowTextColor; Color:=FocusedWindowColor; end;
    end;
end;

procedure TMPlayerForm.ComboBoxCDEnter(Sender: TObject);
begin
  GetDrivesOfType({DRIVE_REMOVABLE}DRIVE_CDROM,DRIVE_CDROM,ComboBoxCD);
  if ComboBoxCD.Items.Count=0 then ComboBoxCD.Clear; // necessary to clear text;
  if Music<>nil then ComboBoxCD.ItemIndex:=ComboBoxCD.Items.IndexOf(Music.CDDrive+':');
  if Sender<>nil then RadioButtonMusicSourceClick(RadioButtonCD);
end;

procedure TMPlayerForm.ComboBoxFolderEnter(Sender: TObject);
var i:Integer; s:String;
begin
  with ComboBoxFolder do begin
    for i:=Pred(Items.Count) downto 0 do begin
        s:=ExpandedFilePath(Items[i],MainForm.MyDocumentsFolder);
        if   DirectoryExists(s) or
             (GetDriveType(PChar(UpCase(s[1])+':\'))<>DRIVE_FIXED) then //
        else Items.Delete(i);
        end;
    if Items.Count=0 then Clear; // necessary to clear text;
    end;
  if Sender<>nil then RadioButtonMusicSourceClick(RadioButtonFolder);
end;

procedure TMPlayerForm.ComboBoxPlaylistEnter(Sender: TObject);
var i:Integer; s:String;
begin
  with ComboBoxPlayList do begin
    for i:=Pred(Items.Count) downto 0 do begin
        s:=ExpandedFilePath(Items[i],MainForm.MyDocumentsFolder);
        if   FileExists(s) or
             (GetDriveType(PChar(UpCase(s[1])+':\'))<>DRIVE_FIXED) then //
        else Items.Delete(i);
        end;
    if Items.Count=0 then Clear; // necessary to clear text;
    end;
  if Sender<>nil then RadioButtonMusicSourceClick(RadioButtonPlayList);
end;

procedure TMPlayerForm.OpenDialog1FolderChange(Sender: TObject);
begin
  with OpenDialog1 do
    if not (ofFileMustExist in Options) then begin
       // FileName:='_ _ _'; // so user can exit the dialog without selecting a file;
       // unfortunately this doesn't work for all drive-types (CD-Audio);
       // hence, the user is required to select a file in the directory: ...
       FileName:='';
       end;
end;

procedure TMPlayerForm.ComboBoxMusicSourceExit(Sender: TObject);
begin
  if Sender is TComboBox then ComboBoxGroupSetColors(TComboBox(Sender));
end;

procedure TMPlayerForm.ApplicationOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if   (Msg.Message = WM_MOUSEWHEEL) then
       if Msg.WParam>=0 then begin
          if (Msg.hwnd=ListBox1.Handle) and
             (ListBox1.ItemIndex>0) then begin
             ListBox1.Selected[ListBox1.ItemIndex]:=False;
             ListBox1.ItemIndex:=Pred(ListBox1.ItemIndex);
             ListBox1.Selected[ListBox1.ItemIndex]:=True;
             Handled:=True;
             end;
          end
       else begin
          if (Msg.hwnd=ListBox1.Handle) and
             (ListBox1.ItemIndex>=0) and
             (ListBox1.ItemIndex<Pred(ListBox1.Items.Count)) then begin
             ListBox1.Selected[ListBox1.ItemIndex]:=False;
             ListBox1.ItemIndex:=Succ(ListBox1.ItemIndex);
             ListBox1.Selected[ListBox1.ItemIndex]:=True;
             Handled:=True;
             end;
          end
  else Handled:=False;
end;

function  TMPlayerForm.FlushPlayList:Boolean;
begin
  if (Music<>nil) and (Music.MusicSource=msPlayList) then with Music do begin
     if (PlayListFileName='') or
        (not DirectoryExists(StrWithoutTrailingPathDelimiter(ExtractFilePath(PlayListFileName)))) then
        PlayListFileName:=PlayListNewFileName(StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath));
     Result:=(PlayListFileName<>'') and
             PlayListSaveToFile(PlayListFileName,'',PlayList);
     end
  else Result:=False;
end;

procedure TMPlayerForm.UpdateFileList;
var i:Integer;
begin
  Timer1.Enabled:=False;
  if   (Music<>nil) and Music.UpdateFileList(Music.MusicSource) then
       try    ListBox1.Items.BeginUpdate;
              try
                ListBox1.Items.Clear;
                for i:=0 to Pred(Music.PlayList.Count) do
                    ListBox1.Items.Add(Music.PrettyTrackName(Music.PlayList.Strings[i]));
                if (Music.MusicSource=msPlayList) and
                   (Music.PlayList.Count>0) then
                   TaskInfo[dtAdd].Dir:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Music.PlayList.Strings[Pred(Music.PlayList.Count)]));
              finally
                ListBox1.Items.EndUpdate;
              end;

       except on E:Exception do begin
                 Error(E.Message,Application.Title);
                 ListBox1.Items.Clear;
                 end;
       end
  else ListBox1.Items.Clear;
  if ListBox1.Items.Count=0 then ListBox1.ItemIndex:=-1;
//if Screen.ActiveForm=Self then ActiveControl:=ListBox1;
  EnableDisableButtons;
end;

procedure TMPlayerForm.RefreshData(UpdateFileList__:Boolean);

  function  CurrentItem(var ComboBox:TComboBox):String;
  begin
    with ComboBox do begin
      if   (ItemIndex<0) and (Items.Count>0) then ItemIndex:=0;
      if   (ItemIndex>=0) and (Items.Count>0)  then Result:=Items[ItemIndex]
      else Result:='';
      end;
  end;

begin // RefreshData
  Timer1.Enabled:=False;
  ComboBoxCDEnter      (nil);
  ComboBoxFolderEnter  (nil);
  ComboBoxPlayListEnter(nil);
  AddItemOrMoveItemToTopOfComboBox(ComboBoxCD      ,CurrentItem(ComboBoxCD      ),False);
  AddItemOrMoveItemToTopOfComboBox(ComboBoxFolder  ,CurrentItem(ComboBoxFolder  ),False);
  AddItemOrMoveItemToTopOfComboBox(ComboBoxPlayList,CurrentItem(ComboBoxPlayList),False);
  if UpdateFileList__ then UpdateFileList;
  EnableDisableButtons;
end;

procedure TMPlayerForm.EnableDisableButtons;
var FileNameOk,Playing:Boolean;

  procedure SetColor(Panel:TPanel; Enabled__:Boolean);
  begin
    with FormColors do begin
      if   Enabled__ then
           Panel.Font.Color:=ButtonTextColor
      else Panel.Font.Color:=GrayedButtonTextColor;
      if   Panel=LastFocusedControl then
           SetControlColor(Panel,FocusedButtonTextColor,FocusedButtonColor)
      else SetControlColor(Panel,ButtonTextColor ,ButtonColor);
      end;
  end;

begin // EnableDisableButtons;
  Timer1.Enabled:=False;
  Playing:=(Music<>nil) and (Music.Playing);
  FileNameOk:=(Music<>nil) and (Music.Player<>nil) and
              (Music.Player.FileName<>'') and
              FileExists(Music.Player.FileName);
  if FileNameOk then begin
     EditPlay.Text:=FileCtrl.MinimizeName(Music.Player.FileName,Canvas,EditPlay.Width);
     if Playing then MusicFileLoaded:=True;
     end
  else begin
     MusicFileLoaded:=False;
     if (Music<>nil) and (Music.Player<>nil) then Music.Player.FileName:='';
     Music.TrackFileName:='';
     EditPlay.Text:='';
     end;

  LabelPlay.Enabled:=Playing;

  ScrollBarPosition.Enabled:=MusicFileLoaded;
  if not FileNameOk then ScrollBarPosition.Position:=0;

  ScrollBarVolume  .Enabled:=Music<>nil;
  LabelVolume      .Enabled:=ScrollBarVolume.Enabled;

  if Playing then begin
     PanelPlay.Caption:=    Text_.MusicPlayerText[     2*Ord(MPlayer1_.btPlayPause) ];
     PanelPlay.Hint   :=BAR+Text_.MusicPlayerText[Succ(2*Ord(MPlayer1_.btPlayPause))];
     PanelStop.Enabled:=True;
     end
  else begin
     PanelPlay.Caption:=    Text_.MusicPlayerText[     2*Ord(MPlayer1_.btPlay) ];
     PanelPlay.Hint   :=BAR+Text_.MusicPlayerText[Succ(2*Ord(MPlayer1_.btPlay))];
     PanelStop.Enabled:=False;
     end;

  SetColor(PanelPlay  ,Playing or FileNameOk);
  SetColor(PanelStop  ,Playing);
  SetColor(PanelSaveAs,ListBox1.Items.Count>0);
  SetColor(PanelAdd   ,(Music.MusicSource=msPlayList) and (Music.PlayListFileName<>''));
  SetColor(PanelRemove,(Music.MusicSource=msPlayList) and (ListBox1.SelCount>0));
  SetColor(PanelClear ,(Music.MusicSource=msPlayList) and (ListBox1.Items.Count>0));

  FormMouseMove(nil,[],0,0); // to update colors on screen

  if (Music<>nil) and (Music.MusicSource=msPlayList) then begin
     ListBox1.Hint:=SelectMultipleTracksHint;
     end
  else begin
     ListBox1.Hint:=SelectSingleTrackHint;
     end;
  ShowStatus;
  Timer1.Enabled:=True; // timer on, because the application tries to load tracks from the cd-drive when a disc is inserted
end;

procedure TMPlayerForm.ListBox1Click(Sender: TObject);
begin
  EnableDisableButtons;
  with ListBox1 do
    if (ItemIndex>=0) and
       (Items.Count>0) and
       (Music<>nil) and
       (not Music.Playing) and
       (Music.PlayList<>nil) and
       (ItemIndex<Music.PlayList.Count) and
       (Music.Player<>nil) then begin
       Music.TrackNo:=Succ(ItemIndex);
       Music.Player.FileName:=Music.FullFileName(ItemIndex);
       MusicFileLoaded:=False;
       EditPlay.Text:=FileCtrl.MinimizeName(Music.Player.FileName,Canvas,EditPlay.Width);
       EnableDisableButtons;
       end;
end;

procedure TMPlayerForm.ListBox1DblClick(Sender: TObject);
begin
  if (Music<>nil) and Music.Playing then Music.Stop(True);
  ListBox1Click(Sender);
  PanelPlayPauseClick(Sender);
end;

procedure TMPlayerForm.PanelRemoveClick(Sender: TObject);
var i:Integer;
begin
  if PanelRemove.Font.Color<>FormColors.GrayedButtonTextColor then begin
     with ListBox1 do
       for i:=Pred(Items.Count) downto 0 do
           if Selected[i] then begin
              Items.Delete(i); Music.PlayList.Delete(i);
              end;
     if (Music<>nil) and (Music.MusicSource<>msPlayList) and (ListBox1.Items.Count>0) then begin
        Music.PlayListFileName:=MainForm.ApplicationDataPath+PlayListFileNameText+PLAYLIST_FILE_EXT;
        Music.MusicSource:=msPlayList;
        RadioButtonMusicSourceClick(RadioButtonPlayList);
        AddItemOrMoveItemToTopOfComboBox(ComboBoxPlayList,Music.PlayListFileName,False);
        end;
     FlushPlayList;
     EnableDisableButtons;
     end;
end;

procedure TMPlayerForm.PanelClearClick(Sender: TObject);
begin
  if (PanelClear.Font.Color<>FormColors.GrayedButtonTextColor) and
     (Application.MessageBox(PChar(PlayListClearText+NL+NL+NoUndoText),PChar(Caption),MB_YESNO+MB_DEFBUTTON2)=ID_YES) then begin
     ListBox1.Clear; Music.PlayList.Clear;
     FlushPlayList;
     EnableDisableButtons;
     end;
end;

procedure TMPlayerForm.PanelPlayPauseClick(Sender: TObject);
var s:String;
begin
  if (Music<>nil) and (Music.Player<>nil) and
     (PanelPlay.Font.Color<>FormColors.GrayedButtonTextColor) then
     if Music.Playing then Music.Stop(True)
     else begin
        s:=Music.Player.FileName;
        if   FileExists(s) then
             if MusicFileLoaded or Music.LoadFromFile(s) then begin
                if not MusicFileLoaded then begin
                   MusicFileLoaded:=True;
                   ScrollBarPosition.Position:=0;
                   end;
                InitScrollBarPosition;

                Music.Play;

                if   (Music.Player<>nil) and (Music.Player.Mode=mpPlaying) then
                     ;//StatusBar1.Panels[1]:=MediaPlayerStateText[Music.Player.Mode];
                end
             else begin
                EnableDisableButtons;
                Error(Format(OpenFileFailedShortText__,[s]){+NL+NL+E.Message},MusicPlayerTitleText);
                end
        else Error(Format(FileNotFoundText__,[s]),Caption);
        end;
  EnableDisableButtons;
end;

procedure TMPlayerForm.MPlayerNotify(Sender: TObject);
var i:Integer; s:String;
begin
  if Music<>nil then with Music do begin
     Timer1.Enabled:=False;
     SleepEx(50,False); // seems necessary to ensure that 'Mode' is updated
     if Playing and (Player<>nil) and (FileList<>nil) and
        ModeOk(Player.Mode) and
        ((Player.Mode=mpStopped) // seems not to be 100% reliable, hence, try to look at position too
         or
         ((PositionMilliSeconds>=LengthInMilliSeconds-10)
          and
          (LengthInMilliSeconds>=10)
         )
        ) then begin
        s:=Player.FileName;
        if   MusicSource=msPlayList then i:=PlayList.IndexOf(s)
        else if (AnsiCompareText(ExtractFilePath(s),StrWithTrailingPathDelimiter(FileList.Directory))=0) and
                (PlayList.IndexOf(ExtractFileName(s))>=0) then i:=0
             else i:=-1;
        if i>=0 then Music.PlayerNotify(Sender) // play next track
        else FileListOk:=False; // reload file list next time the timer is triggered
        MusicFileLoaded:=(Music<>nil) and Music.PlayerIsPlaying(Player);
        end;
     Playing:=(Music<>nil) and Music.PlayerIsPlaying(Player);

     if Player<>nil then begin
        Player.Notify:=True;
        EditPlay.Text:=FileCtrl.MinimizeName(Music.Player.FileName,Canvas,EditPlay.Width);
        end
     else EditPlay.Text:='';
     InitScrollBarPosition;
     EnableDisableButtons;
     end;
end;

procedure TMPlayerForm.ListBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      Key=VK_RETURN then ListBox1DblClick(Sender)
  else if Key=VK_ESCAPE then Close;
end;

procedure TMPlayerForm.Timer1Timer(Sender: TObject);
begin
  if (Music<>nil) and (Timer1.Enabled) then begin
     if   Music.Playing then begin
          ScrollBarPosition.Position:=Music.PositionMilliSeconds;
          ShowStatus;
          end;

     if   Music.MasterVolume64Kibi<>Music.Volume64Kibi then begin
          Music.Volume64Kibi:=Music.MasterVolume64Kibi;
          with ScrollBarVolume do Position:=(Max-Min)*Music.Volume64Kibi div 65535;
          end;

     if   Music.FileListOk then
          if   not DirectoryExists(Music.FileList.Directory) then begin
               Music.FileListOk:=False;
               RefreshData(True);
               end
          else
     else if Music.CheckIfFileAppeared then RefreshData(True);
     end;
end;

procedure TMPlayerForm.ScrollBarPositionScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if (Music<>nil) and MusicFileLoaded then with ScrollBarPosition do begin
     Timer1.Enabled:=False;
     if      ScrollCode=scPageDown  then ScrollPos:=SokUtil_.Min(Max,ScrollPos+SmallChange*2)
     else if ScrollCode=scPageUp    then ScrollPos:=SokUtil_.Max(0  ,ScrollPos-SmallChange*2);
     //Music.Stop(True);
     Music.PositionMilliSeconds:=SokUtil_.Min(ScrollPos,SokUtil_.Max(0,Abs(Music.LengthInMilliSeconds)-1000));
     //Music.Play;
     Position:=Music.PositionMilliSeconds;
     Timer1.Enabled:=True;
     end;
end;

procedure TMPlayerForm.InitScrollBarPosition;
begin
  if Music<>nil then with ScrollBarPosition do begin
     Max:=SokUtil_.Max(1,Abs(Music.LengthInMilliSeconds));
     SmallChange:=SokUtil_.Max(1,Max div 20);
     end;
end;

procedure TMPlayerForm.SetDefaultValues;
begin
  with FormColors do begin
    BackgroundColor         :=$EBCE87; // skyblue //$ff901E; // dodgerblue
    BackgroundTextColor     :=clBlack;
    HighlightedTextColor    :=clBlue;             //$2FFFAD; // greenyellow
    ButtonColor             :=$FF8000;            //EBCE87; // skyblue
    ButtonTextColor         :=clWhite;
    FocusedButtonColor      :=$00D7FF; // gold
    FocusedButtonTextColor  :=clBlue;
    GrayedButtonColor       :=clSilver;
    GrayedButtonTextColor   :=clGray;
    WindowColor             :=$CDFAFF; // lemonchiffon
    WindowTextColor         :=clBlack;
    FocusedWindowColor      :=$2FFFAD; // greenyellow
    FocusedWindowTextColor  :=clBlack;
    end;
end;

procedure TMPlayerForm.SetControlColor(Control:TWinControl; TextColor__,BackgroundColor__:TColor);
begin
  with FormColors do
    if      Control is TPanel                                then with Control as TPanel       do
            if Caption<>''                                   then
               if Font.Color=GrayedButtonTextColor           then  Color:=GrayedButtonColor
               else                                          begin Font.Color:=TextColor__; Color:=BackgroundColor__; end
            else
    else if Control is TRadioButton                          then with Control as TRadioButton do
            if   Checked and (BackgroundColor__=ButtonColor) then
                 begin Font.Color:=HighlightedTextColor;     Color:=BackgroundColor; end
            else if BackgroundColor__=FocusedButtonColor     then
                 begin Font.Color:=TextColor__;              Color:=BackgroundColor__; end
            else begin Font.Color:=BackgroundTextColor;      Color:=BackgroundColor; end
    else if Control is TComboBox                             then with Control as TComboBox    do
                     begin Font.Color:=TextColor__;          Color:=BackgroundColor__; end;
end;

procedure TMPlayerForm.SetFormColors;
var i:Integer;
begin
  with FormColors do begin
    MakeUniqueGrayedButtonTextColor(FormColors);
    
    FormMouseMove(nil,[],0,0);
      for i:=0 to Pred(ComponentCount) do
        if Components[i] is TPanel then with Components[i] as TPanel do
           if Tag=TAG_BUTTON then begin
              Font.Color:=ButtonTextColor; Color:=ButtonColor;
              end;

    with Self                do begin Font.Color:=BackgroundTextColor;  Color:=BackgroundColor; end;
    with PanelPlayer         do begin Font.Color:=BackgroundTextColor;  Color:=BackgroundColor; end;
    with PanelSource         do begin Font.Color:=BackgroundTextColor;  Color:=BackgroundColor; end;
    with PanelLeft           do begin Font.Color:=BackgroundTextColor;  Color:=BackgroundColor; end;

    with LabelPlay           do begin Font.Color:=HighlightedTextColor; Color:=BackgroundColor; end;

    with RadioButtonCD       do begin Font.Color:=BackgroundTextColor;  Color:=BackgroundColor; end;
    with RadioButtonFolder   do begin Font.Color:=BackgroundTextColor;  Color:=BackgroundColor; end;
    with RadioButtonPlayList do begin Font.Color:=BackgroundTextColor;  Color:=BackgroundColor; end;

    with ComboBoxCD          do begin Font.Color:=WindowTextColor;      Color:=WindowColor;     end;
    with ComboBoxFolder      do begin Font.Color:=WindowTextColor;      Color:=WindowColor;     end;
    with ComboBoxPlayList    do begin Font.Color:=WindowTextColor;      Color:=WindowColor;     end;
    with ListBox1            do begin Font.Color:=WindowTextColor;      Color:=WindowColor;     end;
    with EditPlay            do begin Font.Color:=clBtnText;            Color:=clBtnFace;       end;
    end;
end;

procedure TMPlayerForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then Close;
end;

procedure TMPlayerForm.ShowStatus;
begin
  if   MusicFileLoaded and (Music<>nil) then with Music do
       StatusBar1.Panels[0].Text:=PositionToString(ScrollBarPosition.Position)+SLASH+PositionToString(LengthInMilliseconds)
  else StatusBar1.Panels[0].Text:='';
end;

procedure TMPlayerForm.PanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TPanel then with Sender as TPanel do with FormColors do
     if Font.Color<>GrayedButtonTextColor then begin
        BevelOuter:=bvLowered; Repaint;
        end;
end;

procedure TMPlayerForm.PanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TPanel then with Sender as TPanel do
     BevelOuter:=bvRaised;
  if Button=mbRight then Close;
end;

procedure TMPlayerForm.RadioButtonMusicSourceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (Sender is TRadioButton) then with Sender as TRadioButton do
     if Checked then RadioButtonMusicSourceClick(Sender); // otherwise 'OnClick' is only triggered when selection changes
end;

procedure TMPlayerForm.PanelHelpClick(Sender: TObject);
var oOnMessage:TMessageEvent;
begin
  if HelpForm<>nil then begin
     oOnMessage:=Application.OnMessage;
     Application.OnMessage:=nil;
     try     HelpForm.ShowModal;
     finally Application.OnMessage:=oOnMessage;
             FormMouseMove(nil,[],0,0); // to update colors on screen
     end;
     end;
end;

procedure TMPlayerForm.ScrollBarVolumeScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Music<>nil then with ScrollBarVolume do begin
     if      ScrollCode=scPageDown  then ScrollPos:=SokUtil_.Min(Max,ScrollPos+SmallChange*2)
     else if ScrollCode=scPageUp    then ScrollPos:=SokUtil_.Max(0  ,ScrollPos-SmallChange*2);
     Music.Volume64Kibi:=ScrollPos*65535 div (Max-Min);
     StatusBar1.Panels[1].Text:=SoundVolumeText+SPACE+Music.Volume64KibiToString(Music.Volume64Kibi);
     end;
end;

procedure TMPlayerForm.PanelStopClick(Sender: TObject);
begin //
  if (Music<>nil) and Music.Playing then begin
     MainForm.MPlayer.Stop;
     ScrollBarPosition.Position:=0;
     EnableDisableButtons;
     end;
end;

end.

