unit Edit_; // this simple and ugly editor is obsolete; YASC uses the editor on the 'Tools' form instead 

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Spin, Grids, Buttons,
  SokFile_,Game_,IniFile_;

type
  TDrawingTool = (dtNone,dtWall,dtFloor,dtBox,dtTarget,dtPlayer,dtClear);

  TEditForm = class(TForm)
    StatusBar1: TStatusBar;
    PanelTop: TPanel;
    PanelBtn: TPanel;
    BtnOk: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    PanelMenu: TPanel;
    Panel7: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    SpinEditWidth: TSpinEdit;
    SpinEditHeight: TSpinEdit;
    SpeedButtonWall: TSpeedButton;
    SpeedButtonFloor: TSpeedButton;
    SpeedButtonBox: TSpeedButton;
    SpeedButtonTarget: TSpeedButton;
    SpeedButtonPlayer: TSpeedButton;
    SpeedButtonClear: TSpeedButton;
    SpeedButtonNone: TSpeedButton;
    Label3: TLabel;
    Label4: TLabel;
    SpeedBtnNew: TSpeedButton;
    SpeedBtnOpen: TSpeedButton;
    SpeedBtnSave: TSpeedButton;
    SpeedBtnSaveAs: TSpeedButton;
    SpeedBtnHelp: TSpeedButton;
    SpeedBtnExit: TSpeedButton;
    ScrollBox1: TScrollBox;
    DrawGrid1: TDrawGrid;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    SpeedBtnUndo: TSpeedButton;
    SpeedBtnRedo: TSpeedButton;
    Panel2: TPanel;
    Label5: TLabel;
    SpeedBtnLeft: TSpeedButton;
    SpeedBtnRight: TSpeedButton;
    SpeedBtnUp: TSpeedButton;
    SpeedBtnDown: TSpeedButton;
    Label6: TLabel;
    Label7: TLabel;
    SpeedBtnRotateCounterClockwise: TSpeedButton;
    SpeedBtnRotateClockwise: TSpeedButton;
    SpeedBtnFlipVertically: TSpeedButton;
    SpeedFlipHorizontally: TSpeedButton;
    Bevel2: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpinEditWidthChange(Sender: TObject);
    procedure SpinEditHeightChange(Sender: TObject);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SpeedButtonToolClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DrawGrid1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGrid1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SpeedBtnNewClick(Sender: TObject);
    procedure SpeedBtnOpenClick(Sender: TObject);
    procedure SpeedBtnSaveClick(Sender: TObject);
    procedure SpeedBtnSaveAsClick(Sender: TObject);
    procedure SpeedBtnHelpClick(Sender: TObject);
    procedure SpeedBtnExitClick(Sender: TObject);
    procedure SpeedBtnMoveClick(Sender: TObject);
    procedure DrawGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ScrollBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedBtnRotateClick(Sender: TObject);
    procedure SpeedBtnUndoClick(Sender: TObject);
    procedure SpeedBtnRedoClick(Sender: TObject);
    procedure SpeedBtnMirrorClick(Sender: TObject);
    procedure SpeedButtonDrawingToolMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    DragPoint :TPoint;
    PlayerPos :TColRow;
    DrawingTool:TDrawingTool;
    DrawingToolGlyphsOk:Boolean;
    function  MakeNewFileName:String;
    function  Save(Sender: TObject):Boolean;
    procedure ShowHint(Sender: TObject);
  public
    { Public declarations }
    Board:TBoard;
    CursorColor:TColor;
    FileName:String;
    Modified:Boolean;
    Refresh :Boolean;
    function  CreateDrawingToolGlyph(SpeedButton__:TSpeedButton; Color__:TColor):Boolean;
    function  CreateDrawingToolGlyphs:Boolean;
    procedure ShowStatus;
    procedure ShowTitle;
    procedure InvalidateDrawingToolGlyphs;
    function  IsALegalLevel:Boolean;
    function  LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function  SaveLevel:Boolean;
    function  SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;

  end;

var
  EditForm: TEditForm = nil;

implementation

uses Text_,Misc_,Pack_,SokUtil_,Main_,Var_, Options_,BitMap_;

{$R *.DFM}

const
  DrawingToolToSquareColorType:array[TDrawingTool] of TSQuareColor =
  (FloorColor,WallColor,FloorColor,BoxColor,GoalColor,PlayerColor,FloorColor);

  EDITOR_INIFILE_SECTION        = 'EditorForm'; // don't localize

procedure TEditForm.FormCreate(Sender: TObject);
begin
  Caption:=Application.Title+' - '+Caption;
  with SpinEditWidth do begin
       MinValue:=MIN_BOARD_WIDTH;
       MaxValue:=MAX_BOARD_WIDTH;
       end;
  with SpinEditHeight do begin
       MinValue:=MIN_BOARD_HEIGHT;
       MaxValue:=MAX_BOARD_HEIGHT;
       end;
  SpeedButtonNone  .Tag:=Ord(dtNone);
  SpeedButtonWall  .Tag:=Ord(dtWall);
  SpeedButtonFloor .Tag:=Ord(dtFloor);
  SpeedButtonBox   .Tag:=Ord(dtBox);
  SpeedButtonTarget.Tag:=Ord(dtTarget);
  SpeedButtonPlayer.Tag:=Ord(dtPlayer);
  SpeedButtonClear .Tag:=Ord(dtClear);

  FileName:='';

  DrawingTool:=dtNone; SpeedButtonNone.Down:=True; InvalidateDrawingToolGlyphs;
  CursorColor:=clYellow;

  Left:=Max(0,(Screen.Width -Width ) div 2);
  Top :=Max(0,(Screen.Height-Height) div 2);  
end;

procedure TEditForm.FormDestroy(Sender: TObject);
begin
  EditForm:=nil; // kludge: 'MainForm' needs this during shutdown
end;

procedure TEditForm.BtnOkClick(Sender: TObject);
begin
  if   Modified then
       if IsALegalLevel then begin
          SaveLevel;
          ActiveControl:=BtnOk; ModalResult:=mrOk; Close; ModalResult:=mrOk;
          end
       else
  else BtnCancelClick(Sender)
end;

procedure TEditForm.BtnCancelClick(Sender: TObject);
begin
  ActiveControl:=BtnCancel; Modified:=False;
  ModalResult:=mrCancel; Close; ModalResult:=mrCancel;
end;

procedure TEditForm.FormResize(Sender: TObject);
begin
  ScrollBox1.HorzScrollBar.Position:=0;
  ScrollBox1.VertScrollBar.Position:=0;
  with DrawGrid1 do begin
    Width :=(DefaultColWidth +GridLineWidth)*ColCount;
    Height:=(DefaultRowHeight+GridLineWidth)*RowCount;
    Left  :=Max(4,((ScrollBox1.ClientWidth -Width ) div 2));
    Top   :=Max(4,((ScrollBox1.ClientHeight-Height) div 2));
    end;
  with Bevel1 do begin
    Width :=DrawGrid1.Width +8;
    Height:=DrawGrid1.Height+8;
    Left  :=DrawGrid1.Left  -4;
    Top   :=DrawGrid1.Top   -4;
    end;
  if Left>Screen.Width-30 then Left:=Screen.Width-30;

  ShowStatus;
end;

procedure TEditForm.ShowHint(Sender: TObject);
begin
  StatusBar1.Panels[1].Text:=GetLongHint(Application.Hint);
end;

procedure TEditForm.FormActivate(Sender: TObject);
var x,y:Integer;
begin
  ModalResult:=mrNone;
  Application.OnHint:=ShowHint;

  MainForm.Status.Hint:='';
  StatusBar1.Panels[1].Text:='';

  //DrawingTool:=dtNone; SpeedButtonNone.Down:=True;

  DrawGrid1.ColCount  :=MainForm.Game.BoardWidth;
  DrawGrid1.RowCount  :=MainForm.Game.BoardHeight;
  SpinEditWidth.Value :=MainForm.Game.BoardWidth;
  SpinEditHeight.Value:=MainForm.Game.BoardHeight;
  PlayerPos           :=MainForm.Game.PlayerStartPos;

  for x:=1 to MAX_BOARD_WIDTH do
      for y:=1 to MAX_BOARD_HEIGHT do Board[x,y]:=FLOOR;
  for x:=1 to MainForm.Game.BoardWidth do
      for y:=1 to MainForm.Game.BoardHeight do
          Board[x,y]:=MainForm.Game.StartBoard[x,y];

  FileName            :=MainForm.Game.FileName;
  if FileName='' then FileName:=MakeNewFileName;
  ShowTitle;

  Modified:=False; ShowStatus; Refresh:=False;
  if not DrawingToolGlyphsOk then DrawingToolGlyphsOk:=CreateDrawingToolGlyphs;

  if   IsDefaultColorBtnFace(PanelTop.Color) then
       PanelTop.Font.Color:=clBlue
  else PanelTop.Font.Color:=clBtnText;
  Label3.Font.Color:=clBtnText;
  Label4.Font.Color:=clBtnText;
  Label5.Font.Color:=clBtnText;
  Label6.Font.Color:=clBtnText;
  Label7.Font.Color:=clBtnText;
  DrawGrid1.Color:=MainForm.Game.Color[FloorColor];

  FormResize(Sender);
  DrawGrid1.Show; DrawGrid1.Invalidate; DrawGrid1.SetFocus;
end;

procedure TEditForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      Key=Ord(ACCEL_CHAR_NEW)      then SpeedBtnNewClick   (Sender)
  else if Key=Ord(ACCEL_CHAR_OPEN)     then SpeedBtnOpenClick  (Sender)
  else if Key=Ord(ACCEL_CHAR_SAVE)     then SpeedBtnSaveClick  (Sender)
  else if Key=Ord(ACCEL_CHAR_SAVE_AS)  then SpeedBtnSaveAsClick(Sender)
  else if Key=Ord(ACCEL_CHAR_EXIT)     then SpeedBtnExitClick  (Sender)
  else if Key=VK_F1                    then SpeedBtnHelpClick  (Sender)
  else if Key=VK_F12                   then
          if   WindowState=wsNormal    then
               WindowState:=wsMaximized
          else WindowState:=wsNormal
  else if Key=VK_ESCAPE                then
          if Modified                  then //
          else                              BtnCancelClick     (Sender)
  else;
end;

procedure TEditForm.SpinEditWidthChange(Sender: TObject);
begin
  if SpinEditWidth.Value<>DrawGrid1.ColCount then
     if   RangeCheck(SpinEditWidth.Value,SpinEditWidth.MinValue,SpinEditWidth.MaxValue) then begin
          DrawGrid1.ColCount:=SpinEditWidth.Value;
          Modified:=True;
          FormResize(Sender);
          end
     else SpinEditWidth.Value:=DrawGrid1.ColCount;
end;

procedure TEditForm.SpinEditHeightChange(Sender: TObject);
begin
  if SpinEditHeight.Value<>DrawGrid1.RowCount then
     if   RangeCheck(SpinEditHeight.Value,SpinEditHeight.MinValue,SpinEditHeight.MaxValue) then begin
          DrawGrid1.RowCount:=SpinEditHeight.Value;
          Modified:=True;
          FormResize(Sender);
          end
     else SpinEditHeight.Value:=DrawGrid1.RowCount;
end;

procedure TEditForm.ShowStatus;
begin
  with StatusBar1.Panels[0] do Text:=OkChangedText[Modified or MainForm.Modified];
  SpeedBtnSave.Enabled:=Modified or MainForm.Modified;
  SpeedBtnRotateCounterClockwise.Enabled:=
    (SpinEditWidth .Value<=MAX_BOARD_HEIGHT) and
    (SpinEditHeight.Value<=MAX_BOARD_WIDTH);
  SpeedBtnRotateClockwise.Enabled:=SpeedBtnRotateCounterClockwise.Enabled;
end;

procedure TEditForm.DrawGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol,ARow:Integer;
begin
  DragPoint.x:=0; DragPoint.y:=0;
  if (Button=mbLeft) and (DrawingTool=dtNone) then begin
     DrawGrid1.MouseToCell(X,Y,ACol,ARow);
     Inc(ACol); Inc(ARow);
     if (Board[ACol,ARow] and (WALL+BOX+GOAL+PLAYER))<>0 then begin
        DragPoint:=Point(ACol,ARow);
        DrawGrid1.BeginDrag(False);
        end;
     end
  else if (Button=mbRight) and (not Modified) then BtnCancelClick(Sender);
end;

procedure TEditForm.DrawGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FormMouseUp(Sender,Button,Shift,X,Y);
end;

procedure TEditForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if DrawGrid1.Dragging then  begin
     DragPoint.x:=0; DragPoint.y:=0;
     DrawGrid1.EndDrag(False);
     end;
  if (Button=mbRight) and (not Modified) then
     BtnCancelClick(Sender);
end;

procedure TEditForm.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  MainForm.DrawGrid1DrawCell(DrawGrid1,ACol,ARow,Rect,State);
  if gdSelected in State then with DrawGrid1.Canvas do begin
     Brush.Color:=CursorColor;
     FrameRect(Rect);
     FrameRect(Classes.Rect(Rect.Left+1,Rect.Top+1,Rect.Right-1,Rect.Bottom-1));
     end;
end;

procedure TEditForm.DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
                                        var CanSelect: Boolean);
const MASK=WALL+BOX+GOAL+PLAYER+FLOOR;
var i,j:Integer; Tool:TDrawingTool;
begin
  if DrawGrid1.Dragging then begin
     DragPoint.x:=0; DragPoint.y:=0;
     DrawGrid1.EndDrag(False);
     end;

  CanSelect:=True;
  Inc(ACol); Inc(ARow);
  j:=Board[ACol,ARow] and MASK;
  i:=j;

  if   Sender=SpeedButtonClear then Tool:=dtClear
  else Tool:=DrawingTool;
  case Tool of
    dtNone      :;
    dtWall      : begin if (i and PLAYER)<>0 then begin
                           PlayerPos.x:=0;
                           Modified:=True;
                           end;
                        Board[ACol,ARow]:=WALL;
                  end;
    dtFloor     : begin if (i and WALL)<>0 then
                           Board[ACol,ARow]:=FLOOR;
                  end;
    dtBox       : begin if (i and WALL)<>0 then i:=FLOOR;
                        if (i and PLAYER)<>0 then begin
                           PlayerPos.x:=0;
                           Dec(i,PLAYER);
                           Modified:=True;
                           DrawGrid1.Invalidate;
                           end;
                        Board[ACol,ARow]:=i or BOX;
                  end;
    dtTarget    : begin if (i and WALL)<>0 then i:=FLOOR;
                        Board[ACol,ARow]:=i or GOAL;
                  end;
    dtPlayer    : begin if (PlayerPos.x<>ACol) or (PlayerPos.y<>ARow) then begin
                           if (PlayerPos.x>0) and (PlayerPos.y>0) then
                              Dec(Board[PlayerPos.x,PlayerPos.y],PLAYER);
                           Modified:=True;
                           DrawGrid1.Invalidate;
                           end;
                        PlayerPos.x:=ACol; PlayerPos.y:=ARow;
                        if (i and WALL)<>0 then i:=FLOOR;
                        Board[ACol,ARow]:=(i and (not BOX)) or PLAYER;
                  end;
    dtClear     : begin if (i and PLAYER)<>0 then begin
                           PlayerPos.x:=0;
                           Dec(i,PLAYER);
                           DrawGrid1.Invalidate;
                           end;
                        if ((i and (BOX+GOAL))<>0) or
                           ((i and WALL)<>0) then i:=FLOOR;
                        Board[ACol,ARow]:=i;
                  end;
  end; // case;
  if (Board[ACol,ARow] and MASK)<>j then Modified:=True;
  ShowStatus;
end;

procedure TEditForm.SpeedButtonToolClick(Sender: TObject);
begin
  if (Sender is TSpeedButton) then with Sender as TSpeedButton do
     if (GroupIndex=1) and
        (Ord(Tag)>=Ord(Low (TDrawingTool))) and
        (Ord(Tag)<=Ord(High(TDrawingTool))) then
        DrawingTool:=TDrawingTool(Tag);
end;

function  TEditForm.IsALegalLevel:Boolean;
var i,j,k,W,H,BoxCount,GoalCount:Integer; s:String;
    B:TBoard; P:TColRow;
begin
  B:=Board; P:=PlayerPos; W:=SpinEditWidth.Value; H:=SpinEditHeight.Value;
  TrimBoard(MIN_BOARD_WIDTH,MIN_BOARD_HEIGHT,B,W,H,P);

  BoxCount:=0; GoalCount:=0;
  for i:=1 to W do
      for j:=1 to H do begin
          k:=B[i,j];
          if (k and BOX )<>0 then Inc(BoxCount);
          if (k and GOAL)<>0 then Inc(GoalCount);
          end;
  Result:=IsALegalBoard(B,W,H,BoxCount,GoalCount,P,True,0,s);
  if not Result then Error(s+NL+NL+PleaseCorrectText,Caption);
end;

function TEditForm.SaveLevel:Boolean;
var W,H:Integer; s:String;
    B:TBoard; P:TColRow;
begin // precondition: the level is legal
  Result:=Modified;
  if Modified then begin
     B:=Board; P:=PlayerPos; W:=SpinEditWidth.Value; H:=SpinEditHeight.Value;
     TrimBoard(MIN_BOARD_WIDTH,MIN_BOARD_HEIGHT,B,W,H,P);

     s:=MainForm.Game.Name;
     if (s='') or IsANewFileName(FileName) then
        s:=TEXT_LEVEL;

     MainForm.Game.Clear;
     MainForm.Game.SetName(s);
     MainForm.Game.FileName    :=FileName;
     if   StrEqual(MainForm.Game.Name,TEXT_LEVEL) then begin           {special: if name = 'Level' then use filename as display-name}
          MainForm.Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(MainForm.Game.FileName);
          if IsBlank(MainForm.Game.DisplayName) then MainForm.Game.DisplayName:=MainForm.Game.Name;
          end
     else MainForm.Game.DisplayName:=ExtractFileNameWithoutPathAndExtension(MainForm.Game.Name);
     MainForm.Game.Board       :=B;
     MainForm.Game.BoardWidth  :=W;
     MainForm.Game.BoardHeight :=H;
     MainForm.Game.PlayerPos   :=P;
     MainForm.Game.CalculateInternalData;
     MainForm.Modified:=True;
     Modified:=False;
     if VarForm<>nil then VarForm.Clear;
     end;
end;

procedure TEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=True;
  if (ActiveControl<>BtnCancel) and // not 100% correct; 'BtnCancel' may be focused, but not activated
     Modified then
     case Application.MessageBox(PChar(ChangedText),
                                 PChar(Caption),
                                 MB_YESNOCANCEL+MB_ICONQUESTION) of
         IDYES    : begin CanClose:=IsALegalLevel;
                          if CanClose then begin
                             SaveLevel;
                             ModalResult:=mrOk;
                             end;
                    end;
         IDNO     : Modified:=False;
         IDCANCEL : CanClose:=False;
     end; // case
end;

procedure TEditForm.DrawGrid1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=Source=DrawGrid1;
end;

procedure TEditForm.DrawGrid1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var i,j,k,ACol,ARow:Integer;
begin
  if (Sender=DrawGrid1) and (Source=DrawGrid1) and
     (DragPoint.x<>0) then begin
     DrawGrid1.MouseToCell(X,Y,ACol,ARow);
     Inc(ACol); Inc(ARow);
     if (ACol<>DragPoint.x) or (ARow<>DragPoint.y) then begin
        i:=Board[DragPoint.x,DragPoint.y];
        j:=Board[ACol,ARow];
        k:=0;

        if ((i and WALL)<>0) and ((j and WALL)=0) then begin    // wall
           i:=FLOOR;
           j:=WALL;
           if (PlayerPos.x=ACol) and (PlayerPos.y=ARow) then
              PlayerPos.x:=0;
           Modified:=True;
           end;

        if ((i and BOX)<>0) and ((j and BOX)=0) then begin       // box
           k:=i and GOAL;
           i:=(i and (not (BOX+GOAL))); // move box and goal square in separate steps
           j:=(j and GOAL) or (BOX+FLOOR);
           if (PlayerPos.x=ACol) and (PlayerPos.y=ARow) then
              PlayerPos.x:=0;
           Modified:=True;
           end;

        if ((i and GOAL)<>0) and ((j and GOAL)=0) and
           ((i and PLAYER)= 0) then begin                        // goal square
           Dec(i,GOAL);
           j:=(j and (BOX+PLAYER)) or (GOAL+FLOOR);
           Modified:=True;
           end;

        if (i and PLAYER)<>0 then begin                          // player
           Dec(i,PLAYER);
           j:=(j and GOAL) or (PLAYER+FLOOR);
           PlayerPos.x:=ACol; PlayerPos.y:=ARow;
           Modified:=True;
           end;

        Board[DragPoint.x,DragPoint.y]:=i+k;
        Board[ACol       ,ARow       ]:=j;

        DrawGrid1.Col:=Pred(ACol); DrawGrid1.Row:=Pred(ARow);
        DrawGrid1.Invalidate;

        ShowStatus;
        end;
     end;
end;

procedure TEditForm.SpeedBtnNewClick(Sender: TObject);
var i,j:Integer;
begin
  if MainForm.CloseLevel(Self) then begin
     Modified:=False;
     FileName:=MakeNewFileName;
     ShowTitle;

     for i:=0 to MAX_BOARD_WIDTH+1 do
         for j:=0 to MAX_BOARD_HEIGHT+1 do
             Board[i,j]:=WALL;
     for i:=2 to Pred(SpinEditWidth.Value) do
         for j:=2 to Pred(SpinEditHeight.Value) do
             Board[i,j]:=FLOOR;
     PlayerPos.x:=0; PlayerPos.y:=0;

     DrawGrid1.Show;
     DrawGrid1.Invalidate;

     DragPoint:=Point(0,0);

     if Sender<>MainForm then Modified:=True;
     ShowStatus;
     end;
end;

procedure TEditForm.SpeedBtnOpenClick(Sender: TObject);
var oModified,oMainFormModified:Boolean;
begin
  oModified:=Modified; oMainFormModified:=MainForm.Modified;
  try
    if   MainForm.Open(Self) then
         FormActivate(Sender)
    else begin Modified:=oModified;
               MainForm.Modified:=oMainFormModified;
               ShowStatus; MainForm.ShowStatus;
               ShowTitle;
               DrawGrid1.Show;
         end;
  finally
    Application.OnHint:=ShowHint;
  end;
end;

procedure TEditForm.SpeedBtnSaveClick(Sender: TObject);
begin
  if SpeedBtnSave.Enabled and
     (Modified or (MainForm.Modified)) then
     if   IsANewFileName(FileName) then
          Save(MainForm.BtnSaveAs)
     else Save(Self); // 'Self: don't ask before overwriting an existing file
end;

procedure TEditForm.SpeedBtnSaveAsClick(Sender: TObject);
begin
  Save(MainForm.BtnSaveAs); // 'MainForm.BtnSaveAs': ask whether to overwrite existing file
end;

function  TEditForm.Save(Sender: TObject):Boolean;
var oModified:Boolean; oFileName:String;
begin
  oModified:=Modified or MainForm.Modified;
  oFileName:=MainForm.Game.FileName;
  MainForm.Game.FileName:=FileName;
  Result:=IsALegalLevel and MainForm.Save(Sender);
  if   Result then begin
       Refresh:=Refresh or oModified;
       FileName:=MainForm.Game.FileName;
       end
  else MainForm.Game.FileName:=oFileName;
  ShowTitle;
end;

procedure TEditForm.SpeedBtnHelpClick(Sender: TObject);
begin
  MainForm.BtnHelpClick(Sender);
end;

procedure TEditForm.SpeedBtnExitClick(Sender: TObject);
begin
  BtnOkClick(Sender);
end;

procedure TEditForm.ShowTitle;
begin
  if   FileName='' then
       Caption:=Application.Title+' - '+LevelEditorText
  else Caption:=Application.Title+' - '+LevelEditorText+' - '+VisualFileName(FileName);
end;

procedure TEditForm.SpeedBtnMoveClick(Sender: TObject);
var a,a1,b,b1,i,j,dx,dy:Integer; t:Array[0..MAX_BOARD_WIDTH+MAX_BOARD_HEIGHT] of Integer;
begin
  if      Sender=SpeedBtnUp    then begin dx:= 0; dy:=-1; a:=1; b:=SpinEditHeight.Value; end
  else if Sender=SpeedBtnDown  then begin dx:= 0; dy:= 1; a:=SpinEditHeight.Value; b:=1; end
  else if Sender=SpeedBtnLeft  then begin dx:=-1; dy:= 0; a:=1; b:=SpinEditWidth .Value; end
  else if Sender=SpeedBtnRight then begin dx:= 1; dy:= 0; a:=SpinEditWidth. Value; b:=1; end
  else                              begin dx:= 0; dy:= 0; a:=0; b:=0; end;

  if dx+dy<>0 then begin
     a1:=a; b1:=b;
     if dx=0 then begin
        for j:=1 to SpinEditWidth.Value do t[j]:=Board[j,a1];
        for i:=1 to SpinEditHeight.Value do begin
            for j:=1 to SpinEditWidth.Value do Board[j,a]:=Board[j,a-dy];
            Dec(a,dy);
            end;
        for j:=1 to SpinEditWidth.Value do Board[j,b1]:=t[j];
        end
     else begin
        for j:=1 to SpinEditHeight.Value do t[j]:=Board[a1,j];
        for i:=1 to SpinEditWidth.Value do begin
            for j:=1 to SpinEditHeight.Value do Board[a,j]:=Board[a-dx,j];
            Dec(a,dx);
            end;
        for j:=1 to SpinEditHeight.Value do Board[b1,j]:=t[j];
        end;

     for i:=1 to SpinEditWidth.Value do
         for j:=1 to SpinEditHeight.Value do
             if (Board[i,j] and PLAYER)<>0 then with PlayerPos do begin x:=i; y:=j; end;
     Modified:=True;
     ShowStatus;
     DrawGrid1.Show; DrawGrid1.Invalidate; DrawGrid1.SetFocus;
     end;
end;

procedure TEditForm.DrawGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var b:Boolean;
begin
  if Key=VK_DELETE then with DrawGrid1 do begin
     DrawGrid1SelectCell(SpeedButtonClear,Col,Row,b);
     DrawGrid1.Invalidate;
     end;
end;

procedure TEditForm.ScrollBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbRight) and (not Modified) then BtnCancelClick(Sender);
end;

function  TEditForm.MakeNewFileName:String;
var s:String;
begin
  s:=MainForm.Game.LastValidFileName;
  if IsAnIniFileSectionFileName(MainForm.Game.LastValidFileName) then
     s:=ExtractIniFileName(s);
  Result:=ExtractFilePath(s);
  if Result='' then Result:=ExtractFilePath(Application.ExeName);
  Result:=Result+TITLE_ILLEGAL_FIRST_CHARACTER+NewText+TITLE_ILLEGAL_FIRST_CHARACTER;
end;

procedure TEditForm.SpeedBtnRotateClick(Sender: TObject);
var i,W,H,Count,Col,Row,oCol,oRow:Integer; oTool:TDrawingTool; P:TColRow; B:TBoard;
begin
  oTool:=DrawingTool; DrawingTool:=dtNone;

  if Sender=SpeedBtnRotateCounterClockwise then Count:=3
  else Count:=1;

  for  i:=1 to Count do begin
       oCol:=DrawGrid1.Col; oRow:=DrawGrid1.Row;
       B:=Board; P:=PlayerPos; W:=SpinEditWidth.Value; H:=SpinEditHeight.Value;
       PlayerPos.x:=Succ(H-P.y); PlayerPos.y:=PlayerPos.x;
       SpinEditWidth.Value:=H; SpinEditHeight.Value:=W; // updates 'DrawGrid1.ColCount' and 'DrawGrid.RowCount' too
       for Col:=1 to MAX_BOARD_WIDTH do
           for Row:=1 to MAX_BOARD_HEIGHT do Board[Col,Row]:=FLOOR;
       for Col:=1 to W do
           for Row:=1 to H do
               Board[Succ(H-Row),Col]:=B[Col,Row];
       DrawGrid1.Row:=oCol; DrawGrid1.Col:=Pred(H-oRow);
       end;

  DrawGrid1.Show;
  DrawGrid1.Invalidate;

  DragPoint:=Point(0,0);

  DrawingTool:=oTool;
  Modified:=True;
  ShowStatus;
end;

procedure TEditForm.SpeedBtnMirrorClick(Sender: TObject);
var i,j,W,H,Col,Row:Integer; oTool:TDrawingTool; B:TBoard; Transformation2D:TTransformation2D;
begin
  oTool:=DrawingTool; DrawingTool:=dtNone;

  if   Sender=SpeedBtnFlipVertically then
       Transformation2D:=t2DFlipVertically
  else Transformation2D:=t2DFlipHorizontally;

  B:=Board; W:=SpinEditWidth.Value; H:=SpinEditHeight.Value;
  CalculateTransformation2D(Transformation2D,PlayerPos.x,PlayerPos.y,W,H,Col,Row);
  PlayerPos.x:=Col; PlayerPos.y:=Row;

  for Col:=1 to W do
      for Row:=1 to H do begin
          CalculateTransformation2D(Transformation2D,Col,Row,W,H,i,j);
          Board[i,j]:=B[Col,Row];
          end;

  CalculateTransformation2D(Transformation2D,Succ(DrawGrid1.Col),Succ(DrawGrid1.Row),W,H,Col,Row);
  DrawGrid1.Col:=Pred(Col); DrawGrid1.Row:=Pred(Row);

  DrawGrid1.Show;
  DrawGrid1.Invalidate;

  DragPoint:=Point(0,0);

  DrawingTool:=oTool;
  Modified:=True;
  ShowStatus;

end;

procedure TEditForm.SpeedBtnUndoClick(Sender: TObject);
begin // not implemented
  SpeedBtnUndo.Enabled:=not SpeedBtnUndo.Enabled;
  SpeedBtnRedo.Enabled:=not SpeedBtnUndo.Enabled;
end;

procedure TEditForm.SpeedBtnRedoClick(Sender: TObject);
begin // not implemented
  SpeedBtnRedo.Enabled:=not SpeedBtnRedo.Enabled;
  SpeedBtnUndo.Enabled:=not SpeedBtnRedo.Enabled;
end;

procedure TEditForm.SpeedButtonDrawingToolMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var SquareColor:TSquareColor; 
begin
  if (Button=mbRight) and (Sender is TSpeedButton) and
     (TDrawingTool(TSpeedButton(Sender).Tag) in [dtWall,dtFloor,dtBox,dtTarget,dtPlayer]) then
     with Sender as TSpeedButton do begin
       SquareColor:=DrawingToolToSquareColorType[TDrawingTool(Tag)];
       OptionsForm.ColorDialog1.Color:=MainForm.Game.Color[SquareColor];
       if OptionsForm.ColorDialog1.Execute then begin
          MainForm.Game.Color[SquareColor]:=OptionsForm.ColorDialog1.Color;
          if SquareColor=BoxColor then
             MainForm.Game.Color[BoxOnGoalColor]:=OptionsForm.ColorDialog1.Color;
          DrawGrid1.Invalidate;

          CreateDrawingToolGlyph(TSpeedButton(Sender),MainForm.Game.Color[SquareColor]);
          if Sender=SpeedButtonFloor then
             CreateDrawingToolGlyph(SpeedButtonTarget,MainForm.Game.Color[GoalColor]);
          end;
     end;
end;

function TEditForm.CreateDrawingToolGlyph(SpeedButton__:TSpeedButton; Color__:TColor):Boolean;
var B:TBitMap;
begin
  Result:=False; B:=nil;
  with SpeedButton__ do
    if Glyph<>nil then
       try     if (Glyph.Width=0) or (Glyph.Height=0) then
                  try    Glyph.Width:=SpeedButtonNone.Glyph.Width;
                         Glyph.Height:=SpeedButtonNone.Glyph.Height;
                  except on E:Exception do Msg(E.Message,TEXT_APPLICATION_TITLE,MB_OK);
                  end;

               if (Glyph.Width<>0) and (Glyph.Height<>0) and
                  BitMapCreate(B,SpeedButton__.Glyph.Width,SpeedButton__.Glyph.Height) then
                  with B do with Canvas do begin
                    Pen.Color:=clBlack;
                    MoveTo(Pred(Width),1); LineTo(Pred(Width),Height);
                    MoveTo(1,Pred(Height)); LineTo(Width,Pred(Height));
                    Pen.Color:=clWhite;
                    MoveTo(0,0); LineTo(0,Height);
                    MoveTo(0,0); LineTo(Width,0);
                    Pixels[Pred(Width),1]:=clWhite;

                    Brush.Style:=bsSolid;
                    if   SpeedButton__<>SpeedButtonTarget then
                         Brush.Color:=Color__
                    else Brush.Color:=MainForm.Game.Color[FloorColor];
                    FillRect(Rect(1,1,Pred(Width),Pred(Height)));

                    if   Graphics.ColorToRGB(Brush.Color)<>Graphics.ColorToRGB(clRed) then
                         Pixels[0,Pred(Height)]:=clRed
                    else Pixels[0,Pred(Height)]:=clYellow;

                    if   SpeedButton__=SpeedButtonTarget then begin
                         Pen.Color:=Color__; Pen.Width:=1;
                         Ellipse(2,2,Width-2,Height-2);
                         end;

                    Glyph.Assign(B);
                    Result:=True;
                    end;
       finally B.Free;
       end;
end;

function TEditForm.CreateDrawingToolGlyphs:Boolean;
begin
  with MainForm.Game do
    Result:=CreateDrawingToolGlyph(SpeedButtonWall  ,Color[WallColor  ]) and
            CreateDrawingToolGlyph(SpeedButtonFloor ,Color[FloorColor ]) and
            CreateDrawingToolGlyph(SpeedButtonBox   ,Color[BoxColor   ]) and
            CreateDrawingToolGlyph(SpeedButtonTarget,Color[GoalColor  ]) and
            CreateDrawingToolGlyph(SpeedButtonPlayer,Color[PlayerColor]);
end;

procedure TEditForm.InvalidateDrawingToolGlyphs;
begin
  DrawingToolGlyphsOk:=False;
  if Screen.ActiveForm=Self then
     DrawingToolGlyphsOk:=CreateDrawingToolGlyphs;
end;

function  TEditForm.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var SquareType:TSquareColor;
begin
  Result:=True;

  for SquareType:=Low(SquareType) to High(SquareType) do with MainForm.Game do
      Color[SquareType]:=TColor(IniFile.ReadInteger(EDITOR_INIFILE_SECTION,EditorColorText[SquareType],Color[SquareType]));
  CursorColor:=TColor(IniFile.ReadInteger(EDITOR_INIFILE_SECTION,'Cursor',CursorColor));
end;

function  TEditForm.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
var SquareType:TSquareColor;
begin
  Result:=True;

  for SquareType:=Low(SquareType) to High(SquareType) do with MainForm.Game do
      IniFile.WriteInteger(EDITOR_INIFILE_SECTION,EditorColorText[SquareType],Color[SquareType]);
  IniFile.WriteInteger(EDITOR_INIFILE_SECTION,'Cursor',CursorColor);
end;

end.

