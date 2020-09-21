unit Help_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  IniFile_;

function SafeLoadFromFile(const Strings:TStrings;
                          const FileName,FormatErrorText,ErrorCaption:string):Boolean;

const
  TAB_INDEX_SOKOBAN_HELP  = 0;
  TAB_INDEX_RELEASE_NOTES = 1;

type
  TPageItem = record
    Color:TColor;
    FileName:String;
    CursorPosition:Integer;
    RichEditMask:Cardinal;
    TopRow:Integer;
    end;

  THelpForm = class(TForm)
    Panel1: TPanel;
    LabelTitle: TLabel;
    RichEdit1: TRichEdit;
    Panel2: TPanel;
    BtnClose: TButton;
    PageControl1: TPageControl;
    TabSheetHelp: TTabSheet;
    TabSheetReleaseNotes: TTabSheet;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure RichEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure RichEdit1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure RichEdit1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
  private
    { Private declarations }
    PageItems:array[TAB_INDEX_SOKOBAN_HELP..TAB_INDEX_RELEASE_NOTES] of TPageItem;
    IsKeyDown:Boolean;
    procedure ScrollLines(DeltaLines:Integer);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    function  LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    procedure OnFontChange;
    function  SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure ShowPage(TabSheet:TTabSheet);
  end;

var
  HelpForm: THelpForm = nil;

implementation

uses RichEdit,ShellApi,Text_,Misc_,SokUtil_,Main_;

{$R *.DFM}

const
  HELP_FORM_INIFILE_SECTION = 'HelpForm'; // don't localize
  TEXT_FONT_SIZE            = 10; // text in 'RichEdit1'; precondition: must match the text sizes used in the files 'Sokoban.rtf' and 'Releases.rtf'

function SafeLoadFromFile(const Strings:TStrings;
                          const FileName,FormatErrorText,ErrorCaption:string):Boolean;
var i:Integer;
begin
  Result:=False;
  if FileExists(FileName) then
     try  i:=(FileSize(FileName) div 10)*12;
          if (Strings=HelpForm.RichEdit1.Lines) and (i>HelpForm.RichEdit1.Tag) then begin
             HelpForm.RichEdit1.Tag:=i;
             HelpForm.RichEdit1.Perform(EM_EXLIMITTEXT,0,i);
             end;
          Strings.LoadFromFile(FileName); Result:=True;
     except
       on E:Exception do
          Application.MessageBox(PChar(E.Message),PChar(FileName),MB_OK+MB_ICONERROR);
     end
   else Application.MessageBox(
          PChar(Format(FormatErrorText,[FileName])),PChar(ErrorCaption),MB_OK);
end;

procedure THelpForm.FormResize(Sender: TObject);
begin
  BtnClose.Left:=(ClientWidth-BtnClose.Width) div 2;
  if Left>Screen.DesktopLeft+Screen.DeskTopWidth -40 then Left:=Screen.DesktopLeft+Screen.DeskTopWidth -40;
  if Top >Screen.DesktopTop +Screen.DeskTopHeight-40 then Top :=Screen.DesktopTop +Screen.DeskTopHeight-40;
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  OnFontChange;
  if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]; // minimize: it may be impossible for the user to bring the application back on the screen again (a Microsoft Windows operating system bug when the general Windows text size settings is > 100%)
  Caption:=Application.Title+' - '+Caption; IsKeyDown:=False; RichEdit1.Tag:=0; // 'RichEdit.Tag' = text capacity
  FormResize(Sender);

  FillChar(PageItems,SizeOf(PageItems),0);
  PageItems[TAB_INDEX_SOKOBAN_HELP ].Color   :=RGB(255,255,204);
  PageItems[TAB_INDEX_SOKOBAN_HELP ].FileName:=MainForm.ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,RTF_FILE_EXT));
  PageItems[TAB_INDEX_RELEASE_NOTES].Color   :=RGB(000,128,192); // RGB( 64,128,128); // teal
  PageItems[TAB_INDEX_RELEASE_NOTES].FileName:=MainForm.ApplicationDataPath+RELEASE_NOTES_FILENAME;

  PageItems[TAB_INDEX_RELEASE_NOTES].RichEditMask:=SendMessage(RichEdit1.Handle, EM_GETEVENTMASK, 0, 0) or ENM_LINK;
  PageItems[TAB_INDEX_SOKOBAN_HELP ].RichEditMask:=PageItems[TAB_INDEX_RELEASE_NOTES].RichEditMask;
end;

procedure THelpForm.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure THelpForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key<>SIMULATED_KEYBOARD_EVENT_KEY then
     IsKeyDown:=True;
end;

procedure THelpForm.RichEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key<>SIMULATED_KEYBOARD_EVENT_KEY then begin
     if IsKeyDown then
        if      (Key=VK_ESCAPE) or (Key=VK_RETURN) or (Key=Ord(SPACE)) or (Key=VK_BROWSER_BACK) then
                BtnCloseClick(Sender);

     IsKeyDown:=False;
     end;
end;

procedure THelpForm.FormActivate(Sender: TObject);
begin
  MainForm.FormDeactivate(Sender);
  MainForm.Status.Hint:=''; IsKeyDown:=False;
  if   (PageControl1.ActivePage<>TabSheetHelp) or
       (RichEdit1.Lines.Count=0) then  // '0': first time, or something went wrong the last time the program tried to load the helpfile
       ShowPage(TabSheetHelp);
end;

procedure THelpForm.FormDeactivate(Sender: TObject);
begin //
end;

procedure THelpForm.RichEdit1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollLines(MOUSE_WHEEL_UP_DOWN_SCROLL_LINES);
  Handled:=True;
end;

procedure THelpForm.RichEdit1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollLines(-MOUSE_WHEEL_UP_DOWN_SCROLL_LINES);
  Handled:=True;
end;

procedure THelpForm.ScrollLines(DeltaLines:Integer);
begin
  if RichEdit1<>nil then RichEdit1.Perform(EM_LINESCROLL,0,DeltaLines);
end;

procedure THelpForm.RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then BtnCloseClick(Sender);
end;

procedure THelpForm.FormPaint(Sender: TObject);
begin
  LabelTitle.Font.Color:=ApplicationHiglightedTextColor;
end;

procedure THelpForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caHide;
end;

procedure THelpForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ActiveControl<>RichEdit1 then IsKeyDown:=False;
  if      (Key=Ord(ACCEL_CHAR_CLOSE)) and
          (not (ssCtrl in Shift)) then // 'Ctrl+C' = clipboard copy
          Close
  else if ((Key=Ord(ACCEL_CHAR_HELP)) or (Key=VK_F1)) then
          if   (PageControl1.ActivePage<>TabSheetHelp) then
               ShowPage(TabSheetHelp)
          else 
  else if (Key=Ord(ACCEL_CHAR_RELEASE_NOTES)) and (PageControl1.ActivePage<>TabSheetReleaseNotes) then
          ShowPage(TabSheetReleaseNotes)
  else if Key=VK_F12 then
          if   WindowState=wsNormal then
               WindowState:=wsMaximized
          else WindowState:=wsNormal;
end;

procedure THelpForm.PageControl1Change(Sender: TObject);
var s:String;
begin
 with PageControl1.ActivePage do
   if (TabIndex>=Low(PageItems)) and (TabIndex<=High(PageItems)) then with PageItems[TabIndex] do begin
      RichEdit1.Color:=Color;
      SendMessage(RichEdit1.Handle, EM_SETEVENTMASK, 0, RichEditMask);
      SendMessage(RichEdit1.Handle, EM_AUTOURLDETECT, Integer(TabIndex=TAB_INDEX_SOKOBAN_HELP) or Integer(TabIndex=TAB_INDEX_RELEASE_NOTES), 0);

      if   (TabIndex=TAB_INDEX_SOKOBAN_HELP) and
           (MainForm.HelpFileName<>'') and
           FileExists(MainForm.HelpFileName) then
           s:=MainForm.HelpFileName
      else s:=FileName;
      if   SafeLoadFromFile(RichEdit1.Lines,s,FileNotFoundText__+NL+NL+PleaseReinstallText,Application.Title+' - '+HelpText) then begin
           //RichEdit1.SelStart:=CursorPosition;
           RichEdit1.Perform(EM_LINESCROLL,0,TopRow);
           end
      else RichEdit1.Clear;
      RichEdit1.Show;
      end;
  if RichEdit1.Visible and (Screen.ActiveForm=Self) then RichEdit1.SetFocus;
end;

procedure THelpForm.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
 with PageControl1.ActivePage do begin
   if (TabIndex>=Low(PageItems)) and (TabIndex<=High(PageItems)) then begin
      PageItems[TabIndex].CursorPosition:=RichEdit1.SelStart;
      PageItems[TabIndex].TopRow        :=RichEdit1.Perform(EM_GETFIRSTVISIBLELINE,0,0);
      end;
   AllowChange:=True;
   end;
end;

procedure THelpForm.ShowPage(TabSheet:TTabSheet);
var AllowChange:Boolean;
begin
  PageControl1Changing(nil,AllowChange);
  PageControl1.ActivePage:=TabSheet;
  PageControl1Change(nil);
end;

procedure THelpForm.WndProc(var Message: TMessage);
var
  p: TENLink;
  strURL: string;
begin
  if (Message.Msg = WM_NOTIFY) then
  begin
    if (PNMHDR(Message.lParam).code = EN_LINK) then
    begin
      p := TENLink(Pointer(TWMNotify(Message).NMHdr)^);
      if (p.Msg = WM_LBUTTONDOWN) then
      begin
        SendMessage(RichEdit1.Handle, EM_EXSETSEL, 0, Longint(@(p.chrg)));
        strURL := RichEdit1.SelText;
        ShellExecute(Handle, 'open', PChar(strURL), nil, nil, SW_SHOWNORMAL);
      end
    end
  end;

  inherited;
end;

function THelpForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
var Section:String;
begin // LoadSettingsFromIniFile;
  Section:=HELP_FORM_INIFILE_SECTION;
  Result :=LoadFontFromIniFile(IniFile,Section,'Window',Self.Font);
  OnFontChange;
end;

function THelpForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
var Section:String;
begin // SaveSettingsToIniFile;
  try    Section:=HELP_FORM_INIFILE_SECTION;
         Result:=SaveFontToIniFile(IniFile,Section,'Window',Self.Font);
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

procedure THelpForm.OnFontChange;
begin
  LabelTitle.Font.Name:=Self.Font.Name;
  LabelTitle.Font.Style:=Self.Font.Style;
  RichEdit1.Font.Size:=TEXT_FONT_SIZE;
end;

end.

