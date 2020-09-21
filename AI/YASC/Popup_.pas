unit Popup_;
{A quick-and-dirty menu using an external panel filled with panels
 that work as buttons.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Grids,
  Misc_,IniFile_;

type
  TColorPopupMenu = class(TObject)
  private
    { Private declarations }
    fFocusedControl:TWinControl;
    fMouseHasBeenOverMenu:Boolean;
    fVisible:Boolean;
    MouseOverSoundEnabled:Boolean;
    Name: String;
    OnClick:TNotifyEvent;
    Panel: TPanel;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure SetControlColor(Control: TWinControl; TextColor__,
                BackgroundColor__: TColor);
    procedure SetDefaultValues;
    procedure SetFormColors;

  public
    { Public declarations }

    FormColors:TFormColors;
    MouseDownClick:Boolean;

    constructor Create(Panel: TPanel; const Name:String; OnClick:TNotifyEvent);

    procedure   CalculateWidthAndHeight;
    procedure   EnableDisableButton(Panel:TPanel);
    procedure   EnableDisableButtons; virtual;
    procedure   Hide;
    function    LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    function    SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure   Show;

    property    FocusedControl:TWinControl read fFocusedControl;
    property    MouseHasBeenOverMenu:Boolean read fMouseHasBeenOverMenu;
    property    Visible:Boolean read fVisible;
  end;

implementation

uses SokUtil_,Main_,Sound_,Snapshots_;

const
  TAG_BUTTON                = 1; // all panels working as buttons are tagged with this number

constructor TColorPopupMenu.Create(Panel: TPanel; const Name:String; OnClick:TNotifyEvent);
var i:Integer;
begin
  Self.Name:=Name; Self.OnClick:=OnClick; Self.Panel:=Panel; fVisible:=False;
  for i:=0 to Pred(Panel.ControlCount) do with Panel do
      if      Controls[i] is TPanel       then with Controls[i] as TPanel       do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
      else if Controls[i] is TButton      then with Controls[i] as TButton      do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
      else if Controls[i] is TRadioButton then with Controls[i] as TRadioButton do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
      else if Controls[i] is TComboBox    then with Controls[i] as TComboBox    do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
      else if Controls[i] is TGroupBox    then with Controls[i] as TGroupBox    do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
      else if Controls[i] is TListBox     then with Controls[i] as TListBox     do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
//    else if Controls[i] is TStringGrid  then with Controls[i] as TStringGrid  do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
      else if Controls[i] is TImage       then with Controls[i] as TImage       do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
//    else if Controls[i] is TMemo        then with Controls[i] as TMemo        do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end
      else if Controls[i] is TStatusBar   then with Controls[i] as TStatusBar   do begin OnMouseDown:=MouseDown; OnMouseMove:=MouseMove; OnMouseUp:=MouseUp; end;

  SetDefaultValues;
  CalculateWidthAndHeight;
end;


procedure TColorPopupMenu.CalculateWidthAndHeight;
var i,W,H:Integer;
begin
  // calculate menu width and height
  MainForm.Image1.Tag:=LongInt(Addr(Panel));
  MainForm.Image1.Canvas.Font.Assign(Panel.Font);
  W:=0; H:=0;
  for i:=0 to Pred(Panel.ControlCount) do with Panel do
      if      Controls[i] is TPanel       then with Controls[i] as TPanel do
              if Tag=TAG_BUTTON then begin
                 W:=Max(W,MainForm.Image1.Canvas.TextWidth(Caption));
                 Inc(H,Height);
                 end;
  Inc(W,16);
  Panel.ClientWidth :=W+Panel.BevelWidth;
  Panel.ClientHeight:=H+Panel.BevelWidth;
  for i:=0 to Pred(Panel.ControlCount) do with Panel do
      if      Controls[i] is TPanel       then with Controls[i] as TPanel do
              if Tag=TAG_BUTTON then Width:=W;
  Panel.Tag:=Panel.ClientWidth; // use 'Tag' to store the ideal panel width
end;

procedure TColorPopupMenu.Show;
begin
  if MainForm.MPlayer<>nil then MainForm.MPlayer.Hide;

  if MainForm.Status <>nil then MainForm.Status.Hint:='';
  MouseOverSoundEnabled:=False;
  SetFormColors;
  fFocusedControl:=nil; MouseMove(Screen.ActiveControl,[],0,0);
  EnableDisableButtons;
  if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
  Panel.Show; fVisible:=True; fMouseHasBeenOverMenu:=False;
end;

procedure TColorPopupMenu.Hide;
begin
  if Panel.Visible then begin
     MouseMove(nil,[],0,0);
     Panel.Hide;
     fVisible:=False; fMouseHasBeenOverMenu:=False;
     if MainForm.Status<>nil then MainForm.Status.Hint:='';
     end;
end;

procedure TColorPopupMenu.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //MouseOverSoundEnabled:=False;
  fMouseHasBeenOverMenu:=True;
  if      Button=mbLeft then begin
          if Sender is TPanel then with Sender as TPanel do
             if Font.Color<>FormColors.GrayedButtonTextColor then begin
                BevelOuter:=bvLowered;
                Repaint;
                if MouseDownClick and Assigned(Self.OnClick) then begin
                   if MainForm.Sound.Enabled then
                   if   MouseOverSoundEnabled then
                        MainForm.Sound.Play(stMenuSelect)
                   else MouseOverSoundEnabled:=True;
                   Self.OnClick(Sender);
                   end;
                end;
          end
  else if Button=mbRight then
          Hide;
end;

procedure TColorPopupMenu.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if TWinControl(Sender)<>FocusedControl then with FormColors do begin
     if FocusedControl<>nil then begin
        SetControlColor(FocusedControl,ButtonTextColor,ButtonColor);
        if FocusedControl is TPanel then with FocusedControl as TPanel do
           if BevelOuter=bvLowered then BevelOuter:=bvRaised;
        end;
     fFocusedControl:=TWinControl(Sender);
     if FocusedControl<>nil then SetControlColor(FocusedControl,FocusedButtonTextColor,FocusedButtonColor);
     if (Sender is TPanel) and
        (TPanel(Sender).Tag=TAG_BUTTON) and
        (TPanel(Sender).Font.Color<>FormColors.GrayedButtonTextColor) and
        MainForm.Sound.Enabled then
        if   MouseOverSoundEnabled and Self.Visible then
             MainForm.Sound.Play(stMenuOver)
        else MouseOverSoundEnabled:=True;
     end;
  if Screen.Cursor<>DEFAULT_CURSOR then Screen.Cursor:=DEFAULT_CURSOR;
  if MainForm.Menu.ItemIndex>=0 then MainForm.Menu.ItemIndex:=-1;
  if Sender<>nil then fMouseHasBeenOverMenu:=True;
end;

procedure TColorPopupMenu.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseMove(Sender,Shift,X,Y);
  if (FocusedControl<>nil) and
     (FocusedControl is TPanel) then with FocusedControl as TPanel do
     if BevelOuter=bvLowered then begin
        BevelOuter:=bvRaised;
        if (not MouseDownClick) and (Button=mbLeft) and Assigned(Self.OnClick) and
           PtInRect(Classes.Rect(0,0,Width,Height),Point(X,Y)) then begin
           if MainForm.Sound.Enabled then
                if   MouseOverSoundEnabled then
                     MainForm.Sound.Play(stMenuSelect)
                else MouseOverSoundEnabled:=True;
           Self.OnClick(Sender);
           end;
        end;
  if Button=mbRight then Hide;
end;

procedure TColorPopupMenu.SetDefaultValues;
begin
  SetDefaultFormColors(FormColors);
  MouseDownClick:=True;
end;

procedure TColorPopupMenu.SetFormColors;
var i:Integer;
begin
  with Panel do with FormColors do begin
    MakeUniqueGrayedButtonTextColor(FormColors);

    MouseMove(nil,[],0,0);
    for i:=0 to Pred(ControlCount) do
        if Controls[i] is TPanel then with Controls[i] as TPanel do
           if Tag=TAG_BUTTON then begin
              Font.Color:=ButtonTextColor; Color:=ButtonColor;
              end;

    Font.Color:=BackgroundTextColor; Color:=BackgroundColor;
    end;
end;

procedure TColorPopupMenu.SetControlColor(Control:TWinControl; TextColor__,BackgroundColor__:TColor);
begin
  with FormColors do
    if      Control is TPanel                                then with Control as TPanel       do
            if Tag=TAG_BUTTON                                then
               if Font.Color=GrayedButtonTextColor           then  Color:=GrayedButtonColor
               else                                          begin Font.Color:=TextColor__; Color:=BackgroundColor__; end
            else
    else if Control is TStringGrid                           then with Control as TStringGrid  do
            if Tag=TAG_BUTTON                                then
                 begin Font.Color:=TextColor__;              Color:=BackgroundColor__; end
            else
    else if Control is TRadioButton                          then with Control as TRadioButton do
            if   Checked and (BackgroundColor__=ButtonColor) then
                 begin Font.Color:=HighlightedTextColor;     Color:=BackgroundColor; end
            else if BackgroundColor__=FocusedButtonColor     then
                 begin Font.Color:=TextColor__;              Color:=BackgroundColor__; end
            else begin Font.Color:=BackgroundTextColor;      Color:=BackgroundColor; end
    else if Control is TComboBox                             then with Control as TComboBox    do
                     begin Font.Color:=TextColor__;          Color:=BackgroundColor__; end;
//  else if Control is TEdit                                 then with Control as TEdit        do
//                   begin Font.Color:=TextColor__;          Color:=BackgroundColor__; end;
end;

function TColorPopupMenu.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
begin
  Result:=LoadFormColorsFromIniFile(IniFile,Name,FormColors);
end;

function TColorPopupMenu.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
begin
  Result:=SaveFormColorsToIniFile(IniFile,Name,FormColors);
end;

procedure TColorPopupMenu.EnableDisableButton(Panel:TPanel);
begin
  with FormColors do begin
    if   Panel.Enabled then
         if   Panel.Font.Color =GrayedButtonTextColor then
              Panel.Font.Color:=ButtonTextColor
         else
    else begin
      Panel.Font.Color:=GrayedButtonTextColor;
      Panel.Enabled:=True; {all 'buttons' are enabled, but inactive ones appear 'grayed'}
      end;
    if   Panel=FocusedControl then
         SetControlColor(Panel,FocusedButtonTextColor,FocusedButtonColor)
    else SetControlColor(Panel,ButtonTextColor,ButtonColor);
    end;
end;

procedure TColorPopupMenu.EnableDisableButtons;
var i:Integer;
begin
  for i:=0 to Pred(Panel.ControlCount) do with Panel do
      if (Controls[i] is TPanel) and
         (TPanel(Controls[i]).Tag=TAG_BUTTON) then
         EnableDisableButton(TPanel(Controls[i]));
  MouseMove(nil,[],0,0); // to update colors on the screen
end;

end.

