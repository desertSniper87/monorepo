unit Skins_;

///{$DEFINE WALL_CAP_QUADRANTS_ABCD}
///{$UNDEF WALL_CAP_QUADRANTS_ABCD}
{$IFNDEF WALL_CAP_QUADRANTS_ABCD}
  {$DEFINE WALL_CAP_QUADRANTS_DCBA}
{$ELSE}
  {$UNDEF  WALL_CAP_QUADRANTS_DCBA}
{$ENDIF}
interface

uses Classes,StdCtrls,Windows,Graphics,Misc_,IniFile_,SokUtil_,Open1_,GView_;

const
  DEFAULT_SKIN_SCRIPTS_DIRECTORY     = 'Skin Scripts';       // don't localize; must match installation package; full path: "{Application}\Skin Scripts"
  MAX_SCRIPT_FILE_SIZE               = 4*1024*1024;
  // 'MAX_SCRIPT_FILE_SIZE' used to be <= maximum capacity for 'TMemo.Text'
  // but in order to handle "Sokoban for Windows" scripts with embedded
  // base64-encoded images, this may not hold anymore on old Windows 95 computers;
  // Additionally, the limit serves as a sanity check to avoid loading overly large files

  SETTINGS_INIFILE_SECTION_SKINS     = 'Skins';

  SKIN_NAME_INDEX                    = 0;
  SKIN_SCRIPT_INDEX                  = 1;
  SKINS_CAPACITY                     = 25;

  KEY_RECTANGLE_SIDES                : array[TRectangleSide] of String
                                     = ('Top','Left','Bottom','Right');
  KEY_WALL_TYPE                      = 'Type (appropriate images required)';
  INIFILE_SECTION_WALL               = 'Graphics - Board - Figures - Wall';
  INIFILE_SECTION_WALL_TRIMMING      = INIFILE_SECTION_WALL + ' - Outer wall trimming, pixels';

type
  TSkinFileType                      = (sftUnknown,sftImageFile,sftIniFile,sftSettingsFile);
  TPixelCodeLineLengths              = array[TRectangleSide,0..9] of Integer;
  TSkinPixelCodes                    = record
    HasPixelCodes                    : Boolean;
    LineLengths                      : TPixelCodeLineLengths;
  end;
  TTileNoSet                         = set of 0..PLAYER_AND_BOX_FRAME_COUNT-1;

  TSkins = class
  private
    fLastSkinNo                      :Integer;
    fScriptsPath                     :String;                                   // property 'ScriptsPath'
    fSettingsScriptFileName          :String;                                   // property 'SettingsScriptFileName'
    fCommonSkinsScriptFileName       :String;                                   // property 'CommonSkinsScriptFileName'
    function  CalculateOuterWallTrimming(const BitMap__:TBitMap; Rect__:TRect; MaskPct__:Integer; var OuterWallTrimming__:TRect):Boolean;
    function  GetItems(Index__,NameOrScript__:Integer; ComboBox__:TComboBox):String;
  protected
    function  GetCaption:String;                                                // property 'Caption'
    function  GetDefaultSkinPath:String;                                        // property 'DefaultSkinPath';
    function  GetScriptFileName:String;                                         // property 'ScriptFileName'
    function  GetScriptImageFileName:String;                                    // property 'ScriptImageFileName'
    function  GetScripts(Index__,NameOrScript__:Integer):String;                // property 'Scripts'
    function  GetSkinFileName:String;                                           // property 'SkinFileName'
    function  GetSkinFileType:TSkinFileType;                                    // property 'SkinFileType';
    function  GetSkins  (Index__,NameOrScript__:Integer):String;                // property 'Skins'
    procedure SetLastSkinNo(LastSkinNo__:Integer);
    procedure SetScriptFileName(const FileName__:String);                       // property 'ScriptFileName'
    procedure SetScripts(Index__,NameOrScript__:Integer; const Value__:String); // property 'Scripts'
    procedure SetSettingsScriptFileName(FileName__:String);                     // property 'SettingsScriptFileName'
    procedure SetSkinFileName(const FileName__:String);                         // property 'SkinFileName'
    procedure SetSkins  (Index__,NameOrScript__:Integer; const Value__:String); // property 'Skins'
  public
    ScriptList:TStringList;   // filenames without path and extension; only used for sorting the scripts

    constructor Create;
    destructor  Destroy; override;

    function  AddScript(const FileName__:String):Integer;
    procedure ClearSkins;
    procedure ClearScripts;
//  function  Convert4x4WallSkinTo3x2WallSkin(SourceBitMap__:TBitMap; const FileName__:String):Boolean;
    procedure DeleteScript(Index__:Integer);
    function  FileFilter:String;
    function  FileNameSignature(const FileName__:String):String;
    function  GetPixelCodes(BitMap__:TBitMap; Col__,Row__,ColWidth__,RowHeight__:Integer; var SkinPixelCodes__:TSkinPixelCodes): Boolean;
    function  GuessColumnsAndRows(BitMap__:TBitMap;
                                  BitMapWidth__,BitMapHeight__:Integer;
                                  var ColCount__,RowCount__:Integer;
                                  var VariableColCount__,VariableRowCount__:Boolean):Boolean;
    function  IndexOfScript(const FileName__:String):Integer;
    function  Initialize:Boolean;
    function  IsASkinScriptFile(TextType__:TTextType):Boolean;
    function  IsASettingsFile(const Lines__:TStrings):Boolean;
    function  LoadFromFile(const FileName__,ScriptFileName__:String):Boolean;
    function  LoadNextSkinFromHistory:Boolean;
    function  LoadNextOrPreviousSkinFromFolder(Direction__:Integer):Boolean;
    function  LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function  LoadTextFromFile(const FileName__:String; TextType__:Open1_.TTextType; var ErrorMessage__:String):Boolean;
    function  MakeDirectories:Boolean;
    function  MakeWallFromCommonSkinSource(ColWidth__,RowHeight__:Integer;
                                           var SourceBitMap__:TBitMap; // kludge: resizes the source bitmap if tiles have an odd size
                                           var DestBitMap__:TBitMap;
                                           var WallType__:TWallType):Boolean;
    function  MakeWallFromMinimumTileSet(var BitMap__:TBitMap; var TileCount__:Integer; var TileNoSet__:TTileNoSet):Boolean;
    function  Parse(TextType__:TTextType; const FileName__:String; IsASettingsFile__:Boolean):Boolean;
    function  ReadColumnsAndRows(Lines__:TStrings;
                                              var ColCount__,RowCount__:Integer;
                                              var VariableColCount__,VariableRowCount__:Boolean;
                                              var ErrorText__:String):Boolean;
    function  SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    function  UpdateMenu:Boolean;

    property  Caption         :String read GetCaption;
    property  CommonSkinsScriptFileName:String read fCommonSkinsScriptFileName;
    property  DefaultSkinPath :String read GetDefaultSkinPath;
    property  LastSkinNo      :Integer read fLastSkinNo           write SetLastSkinNo;
    property  ScriptFileName  :String read GetScriptFileName      write SetScriptFileName;
    property  ScriptImageFileName:String read GetScriptImageFileName;
    property  Scripts[Index__,NameOrScriptIndex__:Integer]:String read  GetScripts write SetScripts;
    property  ScriptsPath     :String read fScriptsPath; // path without trailing path-delimiter
    property  SettingsScriptFileName:String read fSettingsScriptFileName write SetSettingsScriptFileName;
    property  SkinFileName    :String read GetSkinFileName        write SetSkinFileName;
    property  SkinFileType    :TSkinFileType read getSkinFileType;
    property  Skins  [Index__,NameOrScriptIndex__:Integer]:String read  GetSkins   write SetSkins;
  end;

{
 --------------------------------------------------------------------
 Exported functions
 --------------------------------------------------------------------
}

function  DrawSkinTitle( SkinBitMap__ : TBitMap; ColCount__, RowCount__ : Integer; const Title__ : String ) : Boolean;
function  SkinsPath:String; // path without trailing path-delimiter

implementation

uses SysUtils,
     {$WARNINGS OFF}
       FileCtrl, {unit 'FileCtrl' is specific to platform}
     {$WARNINGS ON}
     Menus,Controls,ComCtrls,Forms,
     Capture_,Res_,SokFile_,Text_,Game_,BitMap_,Pict_,Options_,Main_;

const
  // don't localize folder names, keys, values, and section names
  DEFAULT_SKIN_SUB_DIRECTORY         = 'Common Skins'; // full path: "{ApplicationDataPath}\Skins\Common Skins"
  INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT= 'Sokoban YASC Skin Script';
//INIFILE_SECTION_WALL_TRIMMING      = 'Graphics - Board - Figures - Wall - Outer wall trimming, pixels';
  KEY_ADD                            = 'Add';
  KEY_BACKGROUND_COLOR               = 'BackgroundColor';
  KEY_BASE64_DECODE                  = 'BaseSixtyFourDecode'; // the parser doesn't accept digits like '64' in identifiers
  KEY_BY                             = 'by';
  KEY_COLUMNS                        = 'Columns';
  KEY_COLOR_THEME                    = 'ColorTheme';
  KEY_COL_WIDTH                      = 'ColWidth';
  KEY_COMMON_SKIN_WALL               = 'CommonSkinWall';
  KEY_END_OF_LINE_COMMENT_SEPARATOR  = 'Comment separator';
  KEY_FILE_FILTER                    = 'FileFilter';
  KEY_FILE_NAME                      = FileNameText; // kludge: 'OptionsForm' uses this, hence, the filename key can be a localized text in contrast to all other keys
  KEY_FILE_NAME_EXT                  = 'Ext';
  KEY_FILE_NAME_FOLDER_SHORTNAME     = 'FolderShortName';  // e.g., FileName.FolderShortName('c:\xxx\yyy\filename.ext') = 'yyy'
  KEY_FILE_NAME_NAME                 = 'Name';             // e.g., FileName.FolderShortName('c:\xxx\yyy\filename.ext') = 'filename.ext'
  KEY_FILE_NAME_PATH                 = 'Path';
  KEY_FILE_NAME_SHORTNAME            = 'ShortName';        // e.g., FileName.FolderShortName('c:\xxx\yyy\filename.ext') = 'filename'
  KEY_FILE_NAME_SHORTNAME_TERMINATOR = ' - ';              // for filenames like "Skin name - John Doe"
  KEY_FILE_TYPE                      = 'FileType';
  KEY_FROM                           = 'from';
  KEY_IMAGE_FILE                     = 'ImageFile';
  KEY_IS_INTEGER                     = 'IsInteger';
  KEY_LEFT                           = 'Left';
  KEY_PIXEL_CODES                    = 'PixelCodes';
  KEY_REAL_TILE                      = 'RealTile';
  KEY_RECT                           = 'Rect';
  KEY_ROW_HEIGHT                     = 'RowHeight';
  KEY_ROWS                           = 'Rows';
  KEY_SECTION                        = 'Section';
  KEY_SELF                           = 'Self';
  KEY_SEPARATOR                      = 'Separator';
  KEY_SKIN_TYPE                      = 'SkinType';
  KEY_SUB_STRING                     = 'SubString';
  KEY_TILE                           = 'Tile';
  KEY_TILE_BASE                      = 'TileBase';
  KEY_TO                             = 'to';
  KEY_TOP                            = 'Top';
  KEY_UNTIL                          = 'until';
  KEY_VARYING                        = 'varying';
  KEY_WALL                           = 'Wall';
  KEY_WALL_CAP_CHAR_LOWERCASE        = 'c'; // must be lowercase of 'KEY_WALL_TOP_CHAR_UPPERCASE'
  KEY_WALL_CAP_CHAR_UPPERCASE        = 'C'; // must be uppercase of 'KEY_WALL_TOP_CHAR_LOWERCASE'
  SETTINGS_INIFILE_SECTION_SKIN_SCRIPTS
                                     = 'Skin Scripts';
//SETTINGS_INIFILE_SECTION_SKINS     = 'Skins';
  VALUE_FILE_TYPE_IMAGE_FILE         = 'Image';
  VALUE_FILE_TYPE_INI_FILE           = 'INI';
  VALUE_FILE_TYPE_SETTINGS_FILE      = 'SET';

function  DrawSkinTitle( SkinBitMap__ : TBitMap; ColCount__, RowCount__ : Integer; const Title__ : String ) : Boolean;
var i, j, x, y, W, H, LineSpacing, TextLength : Integer; TextSize : TSize; R : TRect; SL : TStringList;
begin
  try    SL := TStringList.Create;
         try     SL.Sorted := False;
                 W := SkinBitMap__.Width  div Max( 1, ColCount__ );
                 H := SkinBitMap__.Height div Max( 1, RowCount__ );
                 R := CellToRect( 2, 3, W, H );
                 TextLength := Length( Title__ );
                 i := 1;
                 for j:=1 to Succ( TextLength ) do
                     if ( j > TextLength ) or ( Title__[ j ] = SPACE ) then begin
                        while ( i < j ) and ( Title__[ i ] <= SPACE ) do Inc( i ); // left-trim
                        if i < j then SL.Add( Copy( Title__, i, j - i ) );
                        i := Succ( j ); // after the found space
                        end;

                  with SkinBitMap__ do with Canvas do begin
                       Font.Height                          := 8;
                       repeat
                         LineSpacing                        := 2;
                         repeat
                           Result                           := True;
                           Brush.Color                      := clDkGray;
                           Brush.Style                      := bsSolid;
                           FillRect( R );
                           Font.Name                        :='MS Sans Serif';
                           Font.Style                       := [];
                           Windows.SetBkMode ( Handle, Windows.TRANSPARENT );
                           y := 0;
                           for j := 0 to Pred( SL.Count )   do begin
                               TextSize                     := TextExtent( SL[ j ] );
                               x := ( W - TextSize.cx ) div 2;
                               if (x < 0) or
                                  ( Succ( x ) + TextSize.cx >  W ) or
                                  ( Succ( y ) + TextSize.cy >  H ) then
                                  Result:=False;
                               if Result then begin
                                  Font.Color                := clBlack;
                                  TextOut( R.Left + Succ( x ), R.Top + Succ( y ), SL[ j ] );
                                  Font.Color                := RGBToColor( __RGB( 240, 240, 240 ) ); // '240': don't use pure white. it's too bright
                                  TextOut(R.Left + x, R.Top + y, SL[ j ] );
                                  Inc( y, TextSize.cy + LineSpacing );
                                  end;
                               end;
                           Windows.SetBkMode ( Handle, Windows.OPAQUE );
                           Dec( LineSpacing );
                         until Result or ( LineSpacing      <  -1 );
                         Font.Height                        := Font.Height - 1;
                       until Result or ( Font.Height        <  6 );
                       if not Result then
                          FillRect( R );
                       end;
         finally SL.Free;
         end;
  except on E:Exception do Result:=Error(E.Message, '' );
  end;
end;

function  SkinsPath:String; // path without trailing path-delimiter
begin
  Result:=MainForm.ApplicationDataPath+StrRemoveCharacters(SkinsText,SPACE);
end;

constructor TSkins.Create;
var i:Integer; s:String;
begin
  ScriptList                   :=nil;
  ScriptList                   :=TStringList.Create; ScriptList.Duplicates:=dupAccept; ScriptList.Sorted:=True;

  fScriptsPath                 :=StrWithTrailingPathDelimiter(MainForm.ApplicationDataPath)+DEFAULT_SKIN_SCRIPTS_DIRECTORY;

  s                            :=StrWithTrailingPathDelimiter(ScriptsPath)+YASC_SETTINGS_RES_NAME+SKIN_SCRIPT_FILE_EXT;
  if FileExists(s) then
     fSettingsScriptFileName   :=s //  development version
  else
     fSettingsScriptFileName   :=StrWithTrailingPathDelimiter(ScriptsPath)+StrSubstitute(YASC_SETTINGS_RES_NAME,UNDERSCORE,SPACE,i)+SKIN_SCRIPT_FILE_EXT;

  s                            :=StrWithTrailingPathDelimiter(ScriptsPath)+COMMON_SKINS_RES_NAME+SKIN_SCRIPT_FILE_EXT;
  if FileExists(s) then
     fCommonSkinsScriptFileName:=s // development version
  else
     fCommonSkinsScriptFileName:=StrWithTrailingPathDelimiter(ScriptsPath)+StrSubstitute(COMMON_SKINS_RES_NAME,UNDERSCORE,SPACE,i)+SKIN_SCRIPT_FILE_EXT;

  LastSkinNo                   :=0;
end;

destructor TSkins.Destroy;
begin
  ScriptList.Free;
  Inherited;
end;

function  TSkins.AddScript(const FileName__:String):Integer;
var s:String;
begin
  Result:=-1;
  if FileExists(FileName__) then begin
     Result:=IndexOfScript(FileName__);
     if Result<0 then begin
        s:=ExtractFileNameWithoutPathAndExtension(FileName__);
        Result:=ScriptList.Add(s);
        if Result>=0 then begin
           OpenForm.SkinScriptsComboBox.Items.Insert(Result,''); // 'Scripts[,] := ...' updates existing items, hence, the item must be created first
           if (Result<OpenForm.SkinScriptsComboBox.Items.Count) and
              (Scripts[Result,SKIN_NAME_INDEX]='') then begin // 'True': 'Insert' seems to have succeeded
              Scripts[Result,SKIN_NAME_INDEX]:=s;
              Scripts[Result,SKIN_SCRIPT_INDEX]:=FileName__;
              SetComboBoxDropDownWidth(OpenForm.SkinScriptsComboBox,Result,False);
              OpenForm.SkinScriptsComboBox.ItemIndex:=Result;
              end
           else begin // drop the new script again
              ScriptList.Delete(Result);
              Result:=-1; Error(TEXT_TASK_FAILED,'');
              end;
           end
        else begin
           Result:=-1; Error(TEXT_TASK_FAILED,'');
          end;
        end
     else
        OpenForm.SkinScriptsComboBox.ItemIndex:=Result;
     end
  else Error(Format(TEXT_FILE_NOT_FOUND_FORMAT,[FileName__]),'');
end;

procedure TSkins.ClearSkins;
var i:Integer;
begin
  if (OpenForm<>nil) and (OpenForm.SkinsComboBox<>nil) then
     with OpenForm.SkinsComboBox do begin
       for i:=0 to Pred(Items.Count) do // clear skintype association for each skin
           if Items.Objects[i]<>nil then SokUtil_.TNode(Items.Objects[i]).Free;
       Clear;
       end;
  if (OptionsForm<>nil) and (OptionsForm.MenuItemRecentSkins<>nil) then
     with OptionsForm.MenuItemRecentSkins do
       while Count>2 do Delete(0); // leave static items only
end;

procedure TSkins.ClearScripts;
var i:Integer;
begin
  if (OpenForm<>nil) and (OpenForm.SkinScriptsComboBox<>nil) then
     with OpenForm.SkinScriptsComboBox do begin
       for i:=0 to Pred(Items.Count) do // clear skintype association for each skin
           if Items.Objects[i]<>nil then SokUtil_.TNode(Items.Objects[i]).Free;
       Clear;
       end;
  ScriptList.Clear;
end;
{
function  TSkins.Convert4x4WallSkinTo3x2WallSkin(SourceBitMap__:TBitMap; const FileName__:String):Boolean;
var ColWidth,RowHeight:Integer; DestBitMap:TBitMap;
begin
  Result:=False;
  if SourceBitMap__<>nil then with SourceBitMap__ do begin
     ColWidth:=Width div 4; RowHeight:=Height div 10;
     Result:=(ColWidth<>0) and (RowHeight<>0) and
             BitMapCreate(DestBitMap,ColWidth*4,RowHeight*8);
     if Result then with DestBitMap do with Canvas do
        try
          Brush.Color:=clNavy; FillRect(Rect(0,0,Width,Height));
          CopyRect(Rect(0,0,Width,2*RowHeight),SourceBitMap__.Canvas,Rect(0,0,Width,2*RowHeight));

          // 00: no wall neighbours, i.e., a single wall square
          CopyRect(CellToRect( 1,3,ColWidth,RowHeight),SourceBitMap__.Canvas,CellToRect(3,5,ColWidth,RowHeight));

          // 05: wall above and below
          CopyRect(CellToRect( 0,3,ColWidth,RowHeight),SourceBitMap__.Canvas,CellToRect(3,3,ColWidth,RowHeight));

          // 10: wall to the left and to the right
          CopyRect(CellToRect( 1,2,ColWidth,RowHeight),SourceBitMap__.Canvas,CellToRect(1,5,ColWidth,RowHeight));

          // 15: walls on all 4 sides
          CopyRect(CellToRect( 0,2,ColWidth,RowHeight),SourceBitMap__.Canvas,CellToRect(1,3,ColWidth,RowHeight));

          CopyRect(Rect(0,RowHeight*4,Width,RowHeight*8),SourceBitMap__.Canvas,Rect(0,RowHeight*6,Width,RowHeight*10));

          try    DestBitMap.SaveToFile(FileName__);
          except on E:Exception do Result:=Error(E.Message,TEXT_APPLICATION_TITLE);
          end;
        finally
          DestBitMap.Free;
        end;
     end;
end;
}
procedure TSkins.DeleteScript(Index__:Integer);
begin
  with OpenForm.SkinScriptsComboBox do
    if (Index__>=0) and (Index__<Items.Count) then begin
       if Items.Objects[Index__]<>nil then
          SokUtil_.TNode(Items.Objects[Index__]).Free;
       Items.Delete(Index__);
       if Index__<ScriptList.Count then ScriptList.Delete(Index__);
       if   Items.Count=0 then Clear
       else ItemIndex:=-1;
       end;
end;

function  TSkins.FileFilter:String;
begin
  Result:=''; InitializeIniFileReader;
  if OpenForm<>nil then
     Result:=ReadString(OpenForm.Texts[tSkinScript].Memo.Lines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_FILE_FILTER,'');
  if (Result='') or (System.Pos('|*.',Result)=0) then // no "|*.": the string doesn't appear to be a filter, hence, use default filter instead
     Result:=ImageFilesText+'  ('+IMAGE_FILES_FILTER+')'+BAR+IMAGE_FILES_FILTER;
end;

function TSkins.FileNameSignature(const FileName__:String):String;
var HashValue:THashTableHashKey;
begin
  HashValue:=StrHashValuePJW(AnsiUpperCase(FileName__));
  Result:=ExtractFileNameWithoutPathAndExtension(FileName__)+SPACE+
          IntToHex(Integer(HashValue shr 32       ),8)+'-'+
          IntToHex(Integer(HashValue and $ffffffff),8);
end;

function  TSkins.GetCaption:String;
begin // property 'Caption'
  Result:=Application.Title+' - '+SkinsText;
end;

function TSkins.GetDefaultSkinPath:String; // path without trailing path-delimiter
begin
  Result:=StrWithTrailingPathDelimiter(SkinsPath)+DEFAULT_SKIN_SUB_DIRECTORY;
end;

function  TSkins.GetItems(Index__,NameOrScript__:Integer; ComboBox__:TComboBox):String;
begin // properties 'Scripts {Name,FileName}' and 'Skins {FileName,ScriptFileName}'
  with ComboBox__ do
    if   (Index__>=0) and (Index__<Items.Count) then
         if        NameOrScript__ =SKIN_NAME_INDEX then Result:=Items[Index__]
         else if   (NameOrScript__=SKIN_SCRIPT_INDEX) and
                   (Items.Objects[Index__]<>nil) then
                   Result:=SokUtil_.TNode(Items.Objects[Index__]).Text
              else Result:=''
    else Result:='';
end;

function TSkins.GetPixelCodes(BitMap__:TBitMap; Col__,Row__,ColWidth__,RowHeight__:Integer; var SkinPixelCodes__:TSkinPixelCodes): Boolean;
var Count:Integer; R:TRect;

 function ScanLines(Canvas__:TCanvas; FromX__,FromY__,ToX__,ToY__:Integer; Side__:TRectangleSide; var LineLengths__:TPixelCodeLineLengths):Integer;
 var X,Y,DX,DY,Index,Length:Integer; Color:TColor;
 begin // precondition: the 2 points given by ['FromX__',FromY__'] and ['ToX__',ToY__'] are on a straight line; the last pixel at ['ToX__','ToY__'] is not included in the scan
   for Index:=Low(LineLengths__[Side__]) to High(LineLengths__[Side__]) do LineLengths__[Side__,Index]:=-1; // '-1' = unspecified
   Index:=0; DX:=ToX__-FromX__; DY:=ToY__-FromY__;
   if DX<>0 then DX:=DX div Abs(DX);
   if DY<>0 then DY:=DY div Abs(DY);
   if Abs(DX)+Abs(DY)=1 then with Canvas__ do begin
      Length:=0; X:=FromX__; Y:=FromY__; Color:=Pixels[X,Y];
      while (X<>ToX__) or (Y<>ToY__) do begin
        if Pixels[X,Y]=Color then // 'True': same pixel has the same color as the previous one
           Inc(Length)
        else begin // 'True': change of color; finish the current line and start a new one
           if Index<=High(LineLengths__[Side__]) then LineLengths__[Side__,Index]:=Length;
           Inc(Index); Length:=1; Color:=Pixels[X,Y];
           end;
        Inc(X,DX); Inc(Y,DY); // advance to the next pixel
        end;
      if (Length>0) and (Index<=High(LineLengths__[Side__])) then // 'True': finish the last line
         LineLengths__[Side__,Index]:=Length;
      end
   else raise Exception.Create(InternalErrorText+' Skins_.TSkins.GetPixelCodes.ScanLines: Points are not on a straight line');
   Result:=Succ(Index); // return the number of lines (it may exceed the number of stored lengths in 'LineLengths__')
 end;

 procedure GetLeftAndRightJustifiedLines(Count__:Integer; var LineLengths__:TPixelCodeLineLengths; Source__,Dest__:TRectangleSide; DestIndex__:Integer);
 begin // contrary to the '-1 = undefined' scheme, this function sets undefined values to 0
   LineLengths__[Dest__,DestIndex__      ]:=0; // initialize the two return values
   LineLengths__[Dest__,Succ(DestIndex__)]:=0;

   case Count__ of
     2  : // 2 lines in the row
          if   LineLengths__[Source__,0]<=LineLengths__[Source__,1] then
               LineLengths__[Dest__,DestIndex__      ]:=LineLengths__[Source__,0]  // left  line is shorter; assume it's the one that counts
          else LineLengths__[Dest__,Succ(DestIndex__)]:=LineLengths__[Source__,1]; // right line is shorter; assume it's the one that counts
     3  : // 3 lines in the row; the middle one is the gap between the 2 lines that count
          begin
            LineLengths__[Dest__,DestIndex__      ]:=LineLengths__[Source__,0];
            LineLengths__[Dest__,Succ(DestIndex__)]:=LineLengths__[Source__,2];
          end;
     else; // 1 unbroken line, i.e., no values in the row, or too many colors in the row
   end; // case
 end;

begin // 'Col__' and 'Row__' are tile indices (not pixel indices); the tile size is given by 'ColWidth__' and 'ColHeight__';
  with R do with SkinPixelCodes__ do begin
    R:=CellToRect(Col__,Row__,ColWidth__,RowHeight__); // calculate pixel co-ordinates of the rectangle
    Result:=(BitMap__<>nil)     and
            (Left< Pred(Right)) and (Top   < Pred(Pred(Bottom))) and // '< Pred(..)': the cell must be at least 2x3 pixels
            (Left>=0)           and (Right <=BitMap__.Width) and
            (Top >=0)           and (Bottom<=BitMap__.Height);
    if Result then begin
       // the bottom of the cell contains outer wall trimming information.
       // it's encoded as four colored spans on the two bottom rows.
       // Second-last row: A left justified colored line represents left
       // trimming, and a right justified line represents right trimming.
       // Last row: A left justified colored line represents top trimming,
       // and a right justified line represents bottom trimming.
       // The colored lines don't use the first and the last pixels in the
       // rows, thereby leaving the entire cell height free to use for
       // other information encoded along the left and the right side of
       // the cell.
       // first scan the second-last bottom line, using the top linelengths vector for temporary storage
       Count:=ScanLines(BitMap__.Canvas,Succ(Left) ,Bottom-2       ,Pred(Right),Bottom-2    ,rsTop   ,LineLengths);
       GetLeftAndRightJustifiedLines(Count,LineLengths,rsTop,rsBottom,0); // transfer the values to the bottom side values
       // then scan the last bottom line, using the top linelengths vector for temporary storage
       Count:=ScanLines(BitMap__.Canvas,Succ(Left) ,Pred(Bottom)   ,Pred(Right),Pred(Bottom),rsTop   ,LineLengths);
       GetLeftAndRightJustifiedLines(Count,LineLengths,rsTop,rsBottom,2); // transfer the values to the bottom side values
       for Count:=4 to High(LineLengths[rsBottom]) do LineLengths[rsBottom,Count]:=-1; // trimming values use index 0..3; the remaining color lines are undefined

       // finally scan the other sides, i.e., the top, left, and right sides
       ScanLines(BitMap__.Canvas,Left       ,Top            ,Right      ,Top         ,rsTop   ,LineLengths);
       ScanLines(BitMap__.Canvas,Left       ,Top            ,Left       ,Bottom      ,rsLeft  ,LineLengths);
       ScanLines(BitMap__.Canvas,Pred(Right),Top            ,Pred(Right),Bottom      ,rsRight ,LineLengths);
       end;

    SkinPixelCodes__.HasPixelCodes:=Result;
    end;
end;

function  TSkins.GetScriptFileName:String;
begin // property 'ScriptFileName'
  if   OpenForm<>nil then Result:=OpenForm.Texts[tSkinScript].FileName
  else Result:='';
end;

function  TSkins.GetScriptImageFileName:String;
var i:Integer; Section,Key:String;
begin // property 'ScriptImageFileName'
  if   OpenForm<>nil then begin
       Result:=ReadString(OpenForm.Texts[tSkinScript].Memo.Lines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_IMAGE_FILE,'');
       if Result<>'' then begin // 'True': the meta script for this skin type mentions an image file for this (scripted) skin type
          i:=System.Pos(PERIOD,Result); // if the name is a 'section.key' pair then split them
          Section:=Copy(Result,1,Pred(i));
          Key:=Copy(Result,Succ(i),MaxInt);
          Result:=ReadString(OpenForm.Texts[tTextFile].Memo.Lines,EQUAL,Section,Key,'');
          end;
       end
  else Result:='';

end;

function  TSkins.GetScripts(Index__,NameOrScript__:Integer):String;
begin // property 'Skins {Name,FileName}'
  Result:=GetItems(Index__,NameOrScript__,OpenForm.SkinScriptsComboBox);
end;

function  TSkins.GetSkinFileName:String;
begin // property 'SkinFileName'
  if OpenForm<>nil then Result:=OpenForm.CurrentFileName
  else Result:='';
end;

function  TSkins.GetSkinFileType:TSkinFileType;
var Value:String;
begin // property 'SkinFileType'
  if   OpenForm<>nil then begin
       if   OpenForm.Texts[tTextFile].FileName<>'' then Value:=VALUE_FILE_TYPE_INI_FILE
       else Value:=VALUE_FILE_TYPE_IMAGE_FILE;
       InitializeIniFileReader;
       Value:=ReadString(OpenForm.Texts[tSkinScript].Memo.Lines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_FILE_TYPE,Value);
       if        StrAnsiPosCI(VALUE_FILE_TYPE_IMAGE_FILE   ,Value)<>0 then Result:=sftImageFile
       else if   StrAnsiPosCI(VALUE_FILE_TYPE_SETTINGS_FILE,Value)<>0 then Result:=sftSettingsFile
            else Result:=sftIniFile;
       end
  else Result:=sftUnknown;
end;

function  TSkins.GetSkins(Index__,NameOrScript__:Integer):String;
begin // property 'Skins {FileName,ScriptFileName}'
  Result:=GetItems(Index__,NameOrScript__,OpenForm.SkinsComboBox);
end;

function TSkins.GuessColumnsAndRows(BitMap__:TBitMap;
                                    BitMapWidth__,BitMapHeight__:Integer;
                                    var ColCount__,RowCount__:Integer;
                                    var VariableColCount__,VariableRowCount__:Boolean):Boolean;
var ColWidth,RowHeight,StartColCount,OblongedTileColCount:Integer;
begin // the calculation assumes that the cells are squares, i.e., width = height, except that is also tries to identify "common format" skins with 4 x 4 tiles where width and height differ
  Result:=(ColCount__<=0) and (RowCount__<=0);
  if not Result then begin
     ColCount__:=Max(1,ColCount__); RowCount__:=Max(1,RowCount__);
     if      VariableColCount__ and VariableRowCount__ then begin
             if Assigned( BitMap__ ) and Assigned( CaptureForm ) and CaptureForm.ReadColumnsAndRows( Bitmap__, ColCount__, RowCount__ ) then
                Result:=True
             else begin
                StartColCount:=ColCount__; OblongedTileColCount:=0;
                repeat
                  while (RowCount__<=BitMapHeight__) and ((BitMapHeight__ mod RowCount__)<>0) do Inc(RowCount__);
                  RowHeight:=BitMapHeight__ div RowCount__;
                  ColCount__:=StartColCount;
                  while (not Result) and (ColCount__<=BitMapWidth__ ) do
                    if   (BitMapWidth__ mod ColCount__)=0 then begin
                         if ((BitMapWidth__ div ColCount__)=RowHeight) {'True': a square tile}
                            and
                            ( (RowHeight   <=256) {'<=256': tiles are probably not bigger than 256x256 pixels}
                              or
                              ( {check if it looks like a 1-row skin with very large tiles up to 512x512}
                                (RowCount__= 1)
                                and
                                (ColCount__>=7)
                                and
                                (RowHeight <=512)
                              )
                            ) then
                            Result:=True
                         else begin
                            {check if it looks like a 1-row skin with oblonged tiles, i.e., with width > height;}
                            {it's only a guess, of course, and it may categorize some skins wrongly}
                            if (RowCount__=1)
                               and
                               (ColCount__>=4)
                               and
                               (ColCount__<BitMapWidth__)
                               and
                               (BitMapHeight__<=32)
                               and
                               (OblongedTileColCount=0) then
                               OblongedTileColCount:=ColCount__; {remember this column count and use it unless there is a square tile}
                            Inc(ColCount__);
                            end;
                         end
                    else Inc(ColCount__);

                  if not Result then
                     if (RowCount__>1) or (OblongedTileColCount=0) then
                        Inc(RowCount__)
                     else begin
                        Result:=True;
                        ColCount__:=OblongedTileColCount;
                        end;
                until Result or (RowCount__>BitMapHeight__);

                if ( ( not Result )
                     or
                     ( ( ColCount__ >= 12 ) and ( RowCount__ >= 12 ) ) // 'True': a heuristic: the number of tiles seems unreasonably high
                   )
                   and
                   ( BitMapWidth__ >=64 ) and ( BitMapHeight__ >= 64 )
                   and
                   (   ( BitMapWidth__  mod ( BitMapWidth__  div 4 ) ) = 0 )
                   and
                   ( ( ( BitMapHeight__ mod ( BitMapHeight__ div 4 ) ) = 0 )
                     or
                     ( ( BitMapHeight__ mod ( BitMapHeight__ div 6 ) ) = 0 )
                   )
                   then begin // 'True' : assume the skin has 4 x 4 or 4 x 6 non-square tiles
                   ColCount__ := 4;
                   if   ( BitMapHeight__ mod ( BitMapHeight__ div 4 ) ) = 0 then
                        RowCount__ := 4
                   else RowCount__ := 6;
                   Result := True;
                   end;
                end;
             end
     else if VariableColCount__ then begin
             RowHeight:=BitMapHeight__ div RowCount__;
             Result:=(RowHeight>0) and ((BitMapWidth__ mod RowHeight)=0);
             if Result then
                ColCount__:=Max(1,BitMapWidth__  div RowHeight)
             else if (RowHeight>0) and ((BitMapHeight__ mod RowHeight)=0) then begin // 'True': the cell height is OK; try to find a column width matching the image width;
                     while (not Result) and (ColCount__<=BitMapWidth__ ) do
                       if   (BitMapWidth__ mod ColCount__)=0 then
                            Result:=True
                       else Inc(ColCount__)
                     end;
             end
     else if VariableRowCount__ then begin
             ColWidth:=BitMapWidth__ div ColCount__;
             Result:=(ColWidth>0) and ((BitMapHeight__ mod ColWidth)=0);
             if Result then RowCount__:=Max(1,BitMapHeight__ div ColWidth);
             end
     else Result:=True;
     if Result then begin
        VariableColCount__:=False; VariableRowCount__:=False;
        end;
     end;
end;

function  TSkins.IndexOfScript(const FileName__:String):Integer;
var i:Integer;
begin
  Result:=-1;
  for i:=0 to Pred(OpenForm.SkinScriptsComboBox.Items.Count) do
      if AnsiCompareText(FileName__,Scripts[i,SKIN_SCRIPT_INDEX])=0 then begin
         Result:=i; break;
         end;
end;

function TSkins.Initialize:Boolean;
begin
  // 'TSkins.Parse.Parse.Statements.Statement.Expression.Term' dispatches on
  // first character in the function-names;
  // if these names changes, then the case-dispatch must be modified too,
  // and so must the following guards against mismatces;
  Result:= (UpCase(KEY_ADD                  [1])='A') and
           (UpCase(KEY_BACKGROUND_COLOR     [1])='B') and
           (UpCase(KEY_BASE64_DECODE        [1])='B') and
           (UpCase(KEY_COLOR_THEME          [1])='C') and
           (UpCase(KEY_COLUMNS              [1])='C') and
           (UpCase(KEY_COL_WIDTH            [1])='C') and
           (UpCase(KEY_FILE_NAME            [1])='F') and
           (UpCase(KEY_IS_INTEGER           [1])='I') and
           (UpCase(KEY_PIXEL_CODES          [1])='P') and
           (UpCase(KEY_RECT                 [1])='R') and
           (UpCase(KEY_ROWS                 [1])='R') and
           (UpCase(KEY_ROW_HEIGHT           [1])='R') and
           (UpCase(KEY_SUB_STRING           [1])='S') and
           (UpCase(KEY_SECTION              [1])='S') and
           (UpCase(KEY_TILE                 [1])='T')
           ;
  if not Result then Error(InternalErrorText+': Skins',Application.Title);
end;

function TSkins.IsASkinScriptFile(TextType__:TTextType):Boolean;
var s:String;
begin
  Result:=False;
  if OpenForm<>nil then with OpenForm.Texts[TextType__].Memo do begin
     InitializeIniFileReader;
     s:=ReadString(Lines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_FILE_TYPE,'');
     Result:=(StrAnsiPosCI(VALUE_FILE_TYPE_IMAGE_FILE   ,s)+
              StrAnsiPosCI(VALUE_FILE_TYPE_INI_FILE     ,s)+
              StrAnsiPosCI(VALUE_FILE_TYPE_SETTINGS_FILE,s)) <> 0;
     end;
end;

function TSkins.IsASettingsFile(const Lines__:TStrings):Boolean;
begin
  Result:=(Lines__.Count>0) and
          (StrAnsiPosCI(LEFT_BRACKET+TEXT_APPLICATION_TITLE,Lines__[0])<>0) and
          (StrAnsiPosCI(SettingsText+RIGHT_BRACKET         ,Lines__[0])<>0);
end;

function  TSkins.LoadFromFile(const FileName__,ScriptFileName__:String):Boolean;
var s:String;

  function LoadFromFile(const FileName__:String):Boolean;
  var SkinFileType:TSkinFileType;
  begin
    Result:=False;
    if OpenForm<>nil then begin
       Result:=StrEqual(FileName__,OpenForm.CurrentFileName);
       if not Result then with OpenForm do begin
          CurrentFileName:=''; CloseText(tTextFile);
          Result:=((StrAnsiPosCI(ExtractFileExt(FileName__),IMAGE_FILES_FILTER)<>0)
                   and
                   LoadImage(FileName__)
                  )
                  or
                  LoadTextFromFile(FileName__,tTextFile);
          if Result then CurrentFileName:=FileName__;
          end;
       if Result then Result:=MakeDirectories;
       if Result then begin
          SkinFileType:=Self.SkinFileType;
          if   OpenForm.Texts[tTextFile].FileName='' then // 'True': the input file is an image
               if   SkinFileType=sftImageFile then // ok
               else Result:=Error(Format(SkinScriptExpectsAnINIFileText__,[ScriptFileName]),Caption)
          else // input file is a text-file
               if   SkinFileType=sftImageFile then
                    Result:=Error(Format(SkinScriptExpectsAnImageText__  ,[ScriptFileName]),Caption)
               else Result:=True;

          if Result then begin
             if IsASettingsFile(OpenForm.Texts[tTextFile].Memo.Lines) then begin
                OptionsForm.LoadDefaultValues;
                Result:=Parse(tSkinScript,FileName__,True);
                end
             else begin
                OptionsForm.LoadDefaultSkin(dsSeamlessWalls,MainForm.Sound.ResetSoundEffectsOnLoadingASkin);
                Result:=Parse(tSkinScript,FileName__,False);
                end;
             end;
          end;
       end;
  end;

begin // LoadFromFile
  if   FileExists(FileName__) then
       if   LoadTextFromFile(ScriptFileName__,tSkinScript,s) then
            if   LoadFromFile(FileName__) then with OpenForm.SkinsComboBox do begin
                 LastSkinNo:=AddItemOrMoveItemToTopOfComboBox(OpenForm.SkinsComboBox,SKINS_CAPACITY,FileName__,False);
                 if   (Items.Count<>0) and
                      (AnsiCompareText(Skins[0,SKIN_NAME_INDEX],FileName__)=0) then
                      try    Skins[0,SKIN_SCRIPT_INDEX]:=ScriptFileName__;
                             if   UpdateMenu then begin
                                  if Assigned( OptionsForm ) then
                                     // normally, 'OptionsForm.SaveData' clears the
                                     // most recently loaded skin title when the settings
                                     // have been modified, as they have been now that a skin
                                     // has been loaded.
                                     // override this behavior by adding a name path delimiter
                                     // as prefix to the skin title. 'OptionsForm.SaveData'
                                     // strips the prefix and save the rest as the most recently
                                     // loaded skin name.
                                     OptionsForm.MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified := FILE_NAME_PATH_DELIMITER + ExtractFileNameWithoutExt( FileName__ );
                                  Result:=True;
                                  end
                             else Result:=Error(TEXT_TASK_FAILED,'');
                      except on E:Exception do Result:=Error(E.Message,'');
                      end
                 else Result:=Error(TEXT_TASK_FAILED,'');
                 end
            else Result:=Error(TEXT_TASK_FAILED,'')
       else Result:=Error(s,'')
  else Result:=Error(Format(TEXT_FILE_NOT_FOUND_FORMAT,[FileName__]),'');
end;

function  TSkins.LoadNextSkinFromHistory:Boolean;
begin
  Result:=False;
  if (OpenForm<>nil) and (OptionsForm<>nil) then with OpenForm.SkinsComboBox do begin
     LastSkinNo:=Max(0,LastSkinNo+1);
     if LastSkinNo<Items.Count then begin
        OpenForm.CurrentFileName:='';
        Result:=LoadFromFile(Skins[LastSkinNo,SKIN_NAME_INDEX],Skins[LastSkinNo,SKIN_SCRIPT_INDEX]);
        end
     else begin
        if      OptionsForm.MenuItemDefaultSkinSeamlessWalls.Checked then OptionsForm.LoadDefaultSkin(dsSeamlessWalls,MainForm.Sound.ResetSoundEffectsOnLoadingASkin)
        else if OptionsForm.MenuItemDefaultSkinTiledWalls   .Checked then OptionsForm.LoadDefaultSkin(dsTiledWalls   ,MainForm.Sound.ResetSoundEffectsOnLoadingASkin)
        else OptionsForm.LoadDefaultSkin(dsBrickWalls,MainForm.Sound.ResetSoundEffectsOnLoadingASkin);
        LastSkinNo:=-1; Result:=True;
        end;
     end;
end;

function  TSkins.LoadNextOrPreviousSkinFromFolder(Direction__:Integer):Boolean;
var i:Integer;
    oShowErrorMessages:TShowErrorMessages;
    SkinFileName, SkinScriptFileName, FolderName, Mask, ErrorText:String;
begin
  Result:=False;
  if (OpenForm<>nil) and (OptionsForm<>nil) then
     try
       if OpenForm.SkinsComboBox.Items.Count>0 then begin
          // the most recenly loaded skin is at the top of the combo box
          SkinFileName      :=Skins[0,SKIN_NAME_INDEX];
          SkinScriptFileName:=Skins[0,SKIN_SCRIPT_INDEX];
          FolderName        :=ExtractFilePath(SkinFileName);
          Result            := (FolderName<>'') and
                               DirectoryExists(FolderName) and
                               (SkinScriptFileName<>'') and
                               FileExists(SkinScriptFileName);
          end;

       repeat
         if not Result then begin
            SkinFileName      :='';
            SkinScriptFileName:=CommonSkinsScriptFileName;
            FolderName        :=DefaultSkinPath;
            Result            := (FolderName<>'') and
                                 DirectoryExists(FolderName) and
                                 (SkinScriptFileName<>'') and
                                 FileExists(SkinScriptFileName);
            end;
         if Result then begin
            Result:=False;
            //OpenForm.DirectoryListBox1.Directory:=FolderName;
            //OpenForm.DirectoryListBox1.Update;

            Mask:='';
            if LoadTextFromFile(SkinScriptFileName,tSkinScript,ErrorText) then begin
               InitializeIniFileReader;
               Mask:=ReadString(OpenForm.Texts[tSkinScript].Memo.Lines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_FILE_FILTER,'');
               i:=System.Pos(BAR,Mask); // the filter format is "text|mask"
               if i>0 then
                  Mask:=Copy(Mask,Succ(i),MaxInt);
               end;
            if Mask='' then // 'True': no file mask specified for this skin format
               Mask:=IMAGE_FILES_FILTER; // assume that the skin format is based on image files

            OpenForm.FileListbox1.Mask:=Mask;
            OpenForm.FileListBox1.Directory:=FolderName;
            OpenForm.FileListBox1.Update;
            if OpenForm.FileListBox1.Items.Count>0 then begin
               i:=OpenForm.FileListBox1.Items.IndexOf(ExtractFileName(SkinFileName)); // returns '-1' if the script file name isn't a member of the list
               if (i=0) and (OpenForm.FileListBox1.Items.Count=1) then begin
                  // there is only one file in the folder. it's probably the
                  // skin the user has loaded. set the current skin folder to
                  // the default skin folder and look for skins here. without
                  // this behavior, the user would get stuck in a folder with
                  // just one skin, and the casual user would never understand
                  // why the "next/previous skin" feature had stopped working.
                  end
               else begin // try to load next/previous skin from the folder
                  Inc(i, Direction__);
                  if      i>=OpenForm.FileListBox1.Items.Count then
                          i:=0
                  else if i<0 then
                          i:=Pred(OpenForm.FileListBox1.Items.Count);
                  if (i>=0) and (i<OpenForm.FileListBox1.Items.Count) then begin
                     SkinFileName:=StrWithTrailingPathDelimiter(FolderName)+OpenForm.FileListBox1.Items[i];
                     if FileExists( SkinFileName) then begin
                        OpenForm.CurrentFileName:='';
                        oShowErrorMessages:=ShowErrorMessages;
                        try     ShowErrorMessages:=semNone; // suppress error messages
                                Result:=LoadFromFile(SkinFileName,SkinScriptFileName);
                        finally ShowErrorMessages:=oShowErrorMessages;
                        end;
                        end;
                     end;
                  end;
               end;
            end;
       until Result or                     // 'True': a new skin has been loaded
             (FolderName=DefaultSkinPath); // 'True': tried and failed to load a new skin from the default skin folder

       if not Result then
          Result:=LoadNextSkinFromHistory;
     except on E:Exception do Error(E.Message,'');
     end;
end;

function  TSkins.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
var i,j:Integer; s:String;
begin
  try    ClearSkins; ClearScripts;
         OpenForm.SkinScriptsComboBox.Tag:=0; // recalculate drop-down width

         // scripts ('SkinsComboBox' is used temporarily for storage)
         LoadComboBoxFromIniFile(IniFile,SETTINGS_INIFILE_SECTION_SKIN_SCRIPTS,MaxInt,False,True,True,False,OpenForm.SkinsComboBox);
         with OpenForm.SkinsComboBox do
           for i:=0 to Pred(Items.Count) do begin
               s:=Items[i];
               if (not FileExists(s)) then begin
                  s:=StrWithTrailingPathDelimiter(ScriptsPath)+ExtractFileName(s);
                  if FileExists(s) then Items[i]:=s;                // the skin script probably moved to the dedicated skin scripts folder
                  end;

               s:=ExtractFileNameWithoutPathAndExtension(Items[i]); // extract the filename, without path and extension
               j:=ScriptList.Add(s);
               if   j>=0 then ScriptList.Objects[j]:=Pointer(i)     // use 'Objects' to save the reference to the full filename
               else raise Exception.Create(TEXT_TASK_FAILED);
               end;
         for   i:=0 to Pred(ScriptList.Count) do begin              // fill in information in 'OpenForm.SkinScriptsComboBox'
               OpenForm.SkinScriptsComboBox.Items.Insert(i,'');     // 'Scripts[,] := ...' updates existing items, hence, the item must be created first
               if (i<OpenForm.SkinScriptsComboBox.Items.Count) and
                  (Scripts[i,SKIN_NAME_INDEX]='') then begin        // 'True': 'Insert' seems to have succeeded
                  Scripts[i,SKIN_NAME_INDEX  ]:=ScriptList.Strings[i];
                  Scripts[i,SKIN_SCRIPT_INDEX]:=OpenForm.SkinsComboBox.Items[Integer(ScriptList.Objects[i])];
                  SetComboBoxDropDownWidth(OpenForm.SkinScriptsComboBox,i,False);
                  end
               else break; // insertion failed
               end;
         for   i:=Pred(ScriptList.Count) downto OpenForm.SkinScriptsComboBox.Items.Count do
               ScriptList.Delete(i); // delete any items that weren't added to the combobox

         // skins
         LoadComboBoxFromIniFile(IniFile,SETTINGS_INIFILE_SECTION_SKINS,SKINS_CAPACITY,False,True,True,False,OpenForm.SkinsComboBox);
         for i:=Pred(OpenForm.SkinsComboBox.Items.Count) downto 0 do // load skintype association for each skin
             with OpenForm.SkinsComboBox do begin
               s:=IniFile.ReadString(SETTINGS_INIFILE_SECTION_SKINS,Items[i],'');
               if   (s='') and
                    ( MainForm.OldApplicationDataPath<>'') and
                    StrBeginsWith(Items[i],MainForm.ApplicationDataPath) then
                    s:=KeepDataPathUpToDate(IniFile.ReadString(SETTINGS_INIFILE_SECTION_SKINS,MainForm.OldApplicationDataPath+Copy(Items[i],Succ(Length(MainForm.ApplicationDataPath)),MaxInt),''));
               if   (s='') and
                    ( MainForm.OldPluginsPath<>'') and
                    StrBeginsWith(Items[i],MainForm.PluginsPath) then
                    s:=KeepPluginsPathUpToDate(IniFile.ReadString(SETTINGS_INIFILE_SECTION_SKINS,MainForm.OldPluginsPath+Copy(Items[i],Succ(Length(MainForm.PluginsPath)),MaxInt),''));
               if   s<>'' then begin
                    if FileExists(s) then
                       Skins[i,SKIN_SCRIPT_INDEX]:=s
                    else begin
                       s:=StrWithTrailingPathDelimiter(ScriptsPath)+ExtractFileName(s);
                       if   FileExists(s) then // the skin script probably moved to the dedicated skin-script folder
                            Skins[i,SKIN_SCRIPT_INDEX]:=s
                       else Items.Delete(i);
                       end;
                    end
               else Items.Delete(i);
               end;

         Result:=UpdateMenu;

  except on E:Exception do begin
            ClearSkins; ClearScripts;
            Result:=Error(E.Message,'Skins.LoadSettingsFromIniFile');
            end;
  end;
end;

function  TSkins.LoadTextFromFile(const FileName__:String; TextType__:Open1_.TTextType; var ErrorMessage__:String):Boolean;
begin
  Result:=False; ErrorMessage__:='';
  if   OpenForm<>nil then
       if   AnsiCompareText(FileName__,OpenForm.Texts[TextType__].FileName)=0 then
            Result:=True
       else if   FileExists(FileName__) then
                 if   FileSize(FileName__)<=MAX_SCRIPT_FILE_SIZE then
                      if   OpenForm.LoadTextFromFile(FileName__,TextType__) then
                           if   (TextType__=tSkinScript) and
                                (not IsASkinScriptFile(tSkinScript)) then
                                ErrorMessage__:=Format(FileNotALegalSkinScriptText__,[FileName__])
                           else Result:=True
                      else ErrorMessage__:=TEXT_TASK_FAILED
                 else ErrorMessage__:=Format(TEXT_FILE_TOO_LARGE_FORMAT,[FileName__])
            else ErrorMessage__:=Format(TEXT_FILE_NOT_FOUND_FORMAT,[FileName__])
  else ErrorMessage__:=TEXT_TASK_FAILED;
end;

function  TSkins.MakeDirectories:Boolean;
begin
  try
    if  not DirectoryExists(SkinsPath               ) then ForceDirectories(SkinsPath);
    if  not DirectoryExists(ScriptsPath             ) then ForceDirectories(ScriptsPath);
    if  not DirectoryExists(DefaultSkinPath         ) then ForceDirectories(DefaultSkinPath);
    if  not DirectoryExists(OptionsForm.SettingsPath) then ForceDirectories(OptionsForm.SettingsPath);
    Result:=True;
  except on E:Exception do Result:=Error(E.Message,'');
  end;
end;

function  TSkins.Parse(TextType__:TTextType; const FileName__:String; IsASettingsFile__:Boolean):Boolean;
type
    TTileInfo       = record
      HasColor      : Boolean;
      HasName       : Boolean;
      SizeRect      : TRect;
    end;
    TWorkBitMap     = record
      BitMap        : TBitMap;
      FileName      : String;
      LastRect      : TRect;
      PictureType   : TPictureType;
      TileCount     : Integer;
      TileNoSet     : TTileNoSet;
      WallType      : TWallType;
    end;
    TValueType      =(valNull,valText,valRect,valTile,valNumber);
    TValue          =record
      Text          :String;
      case ValueType:TValueType of
        valNull     :();
        valText     :();
        valRect     :(Rect:TRect);
        valTile     :({Rect:TRect});
        valNumber   :(Number:Integer);
    end;
    PValue          =^TValue;
var
  HasWallType,VariableColCount,VariableRowCount:Boolean; //StartTimeMS:TTimeMS;
  Ch:Char;
  FileNameSignature:String; oCursor:TCursor;
  TileInfo:array[TPictureType] of TTileInfo;
  SkinPixelCodes:TSkinPixelCodes;
  WorkBitMap:TWorkBitMap; Pict:TPict; TextLines:TStrings;
  Variables:array['A'..'Z'] of TValue;

// syntax:
// each line is parsed separately.
// < ... > is parameters to the production rule, e.g., "text < "." >".
// "if" + condition must be on a single line
// "else" and "endif" must be on lines of their own
//
// statements                 ::= statement*.
//   statement                ::= section | assignment | if | perform |
//                                getimagetext | end.
//     section                ::= "["  subsection   ("-" subsection)*  "]".
//       subsection           ::= text < "-"  "]" >.
//     assignment             ::= variable "=" expression.
//       variable             ::= text < "=" >.
//       expression           ::= "{" [ term ] "}" | text <>.
//         term               ::= "BackgroundColor" | "Columns" |
//                                "FileName" | "Rows" |
//                                add | tile | rect | isinteger | substring |
//                                baseSixtyFourDecode | pixelcodes | keyvalue.
//           add              ::= "add" [ "(" ] (string | number)* [ ")" ].
//           tile             ::= "tile" number [".." number] number [".." number].
//           rect             ::= "rect" number [number [number [number]]].
//           isinteger        ::= "isinteger" [ "(" ] expression [ ")" ].
//           stringplus       ::= "stringplus" (string | number)*.
//           substring        ::= "substring" string number [number].           // parameters: string, start-position, and optional length
//           baseSixtyFourDecode
//                            ::= "BaseSixtyFourDecode" string string [string]  // parameters: string, filename, and optional fileextension
//           pixelcodes       ::= "PixelCodes" "."                              // colored lines at the fringe of the rectangle with embedded settings in an image
//                                ( (number "." number)                         // read pixel codes in image cell [number, number]; numbers are 0-based
//                                  |
//                                  ( ("Top | "Left" | "Bottom" | "Right")      // side of the rectangle
//                                    "." number [number] ) ).                  // the first number is the 0-based line index; the second number is optional and if it's present it's added to the returned value
//
//           keyvalue         ::= [inifileSection "."] inifileKey.              // note that this definition is recursive, allowing multiple lookups
//             inifileSection ::= text < ".{}" >
//             inifileKey     ::= "{" keyvalue "}" | text < "}" >.
//     if                     ::= "if" expression [ ( "=" | "==") expression ] statement*
//                                [ "else" statement* ]
//                                "endif".
//     perform                ::= "perform" expression  // sub-routine must end with "end"
//     end                    ::= "end"
//     getimagetext           ::= "getimagetext" (tile | rect | number number number number) string*. parameters: rectangle and optional header line; the header line is the concatenation of one of more strings
//     text <terminators>     ::= (character character*) until a terminator is met.

  function  PictureIsLoaded(const FileName__:String; const Rect__:TRect):Boolean;
  begin
    Result:=(Pict<>nil) and
            StrEqual(FileName__,Pict.FileName) and
            (Pict.OrgBitMap<>nil) and
            (Pict.OrgBitMap.Width >=1) and
            (Pict.OrgBitMap.Height>=1) and
            IsEqualRects(Rect__,Pict.SourceRect);
  end;

  function  LoadPictureFromFile(PictureType__:TPictureType):Boolean;
  var W,H:Integer; FileName:String; Rect:TRect;
  begin
    FileName:=OptionsForm.PictureTypeFileName[PictureType__];
    Rect    :=OptionsForm.PictureTypeSection [PictureType__];
    Result  :=PictureIsLoaded(FileName,Rect);
    if (not Result) and (Pict<>nil) then
       if (FileName<>'') and FileExists(FileName) then begin
          Pict.SourceRect:=Rect;

          if ((FileName=OpenForm.CurrentImageFileName)
              and
              Pict.LoadFromBitMapRect(OpenForm.Image1.Picture.BitMap,Rect,Point(0,0),clBlack)
             )
             or
             Pict.LoadFromFile(FileName) then begin
             if Pict.OrgBitMap=nil then Pict.MakeOrgBitMapFromPict;
             Result:=(Pict.OrgBitMap       <>nil) and
                     (Pict.OrgBitMap.Width >=1)   and
                     (Pict.OrgBitMap.Height>=1);
             if Result and (PictureType__=ptWall) then with Pict.OrgBitMap do begin
                W:=Width; H:=Height;
                if Odd(W) then Inc(W);
                if Odd(H) then Inc(H);
                if (W<>Width) or (H<>Height) then
                   Pict.ResizeOriginal(W,H,aaNone);
                end;
             end
          else raise Exception.Create(Format(UnsupportedImageFormatErrorText__,[FileName]));
          end;
  end;

  function  CloseWorkBitMap:Boolean;
  var OuterWallTrimming:TRect;
  begin
    with WorkBitMap do
      try    if        (PictureType=ptWall) and
                       (TileCount>0) and
                       (TileCount<WALL_TILE_COUNT) then begin
                       MakeWallFromMinimumTileSet(BitMap,TileCount,TileNoSet);
                       end;

             if        (FileName='')
                       or
                       ((PictureType=ptWall) and (TileCount<WALL_TILE_COUNT div 2)
                       )
                       or
                       (((PictureType=ptPlayer) or (PictureType=ptPlayerOnGoal))
                        and
                        (TileCount<NUMBER_OF_DIRECTIONS-1)
                       ) then begin
                       FileName:=''; Result:=True;
                       end
             else if   BitMap<>nil then begin
                       BitMap.SaveToFile(FileName);
                       OptionsForm.PictureTypeFileName[PictureType]:=FileName; // update settings, i.e., use this new bitmap
                       OptionsForm.PictureTypeSection [PictureType]:=Rect(0,0,0,0);

                       if   (PictureType=ptWall) and
                            (WallType<>wtTiledWall) and
                            CalculateOuterWallTrimming(BitMap,
                                                       Rect(0,0,BitMap.Width div WALL_TILE_COUNT,BitMap.Height),
                                                       StrToInt(OptionsForm.SettingsString[stGraphicsBoardFiguresWallImageBackgroundColorTolerancePct]),
                                                       OuterWallTrimming) then
                            with OuterWallTrimming do begin
                              OptionsForm.UpdateSettings(-Ord(stGraphicsBoardFiguresWallOuterWallTrimmingLeft  ),IntToStr(Left  ),Self);
                              OptionsForm.UpdateSettings(-Ord(stGraphicsBoardFiguresWallOuterWallTrimmingTop   ),IntToStr(Top   ),Self);
                              OptionsForm.UpdateSettings(-Ord(stGraphicsBoardFiguresWallOuterWallTrimmingRight ),IntToStr(Right ),Self);
                              OptionsForm.UpdateSettings(-Ord(stGraphicsBoardFiguresWallOuterWallTrimmingBottom),IntToStr(Bottom),Self);
                              end;

                       if   BitMapResize(BitMap,1,1) then begin
                            FileName:=''; Result:=True;
                            end
                       else Result:=False;

                       if      PictureType=ptPlayer then
                               OptionsForm.UpdateSettings(-Ord(stPlayerDirectionAnimationEnabled),BooleanText [True    ],Self)
                       else if (PictureType=ptWall) and (not HasWallType) then
                               OptionsForm.UpdateSettings(-Ord(stGraphicsBoardFiguresWallType   ),WallTypeText[WallType],Self);

                       TileInfo[PictureType].HasName:=True;
                       end
                  else raise Exception.Create(TEXT_TASK_FAILED);
      except on E:Exception do Result:=Error(E.Message,'');
      end;
  end;

  function  OpenWorkBitMap(PictureType__:TPictureType; LoadFile__:Boolean; ColWidth__,RowHeight__:Integer):Boolean;
  begin
    with WorkBitMap do begin
      Result:=(FileName='') or (PictureType__=PictureType) or CloseWorkBitMap;
      if Result and ((FileName='') or (BitMap=nil)) then begin
         PictureType:=PictureType__; TileCount:=0; TileNoSet:=[]; WallType:=wtTiledWall;
         LastRect:=Rect(0,0,-1,-1);
         FileName:=StrWithTrailingPathDelimiter(OptionsForm.SettingsPath)+
                   TEXT_APPLICATION_TITLE_SHORT+' - '+
                   //FileNameSignature+'-'+
                   PICTURE_TYPE_NAME[PictureType]+
                   BMP_FILE_EXT;
         if LoadFile__ then begin
            if LoadPictureFromFile(PictureType__) then
               if   (ColWidth__<>MaxInt) and (RowHeight__<>MaxInt) then
                    if        (PictureType__=ptWall) and
                              BitMapResize(BitMap,ColWidth__*WALL_TILE_COUNT,RowHeight__) then
                              Result:=True
                    else if   BitMapResize(BitMap,ColWidth__,RowHeight__) then
                              Result:=True
                         else Result:=BitMapResize(BitMap,1,1)
               else if        (PictureType__=ptWall) and
                              BitMapResize(BitMap,Pict.OrgBitMap.Width*WALL_TILE_COUNT,Pict.OrgBitMap.Height) then
                              Result:=True
                    else if   BitMapResize(BitMap,Pict.OrgBitMap.Width,Pict.OrgBitMap.Height) then
                              Result:=True
                         else Result:=BitMapResize(BitMap,1,1)
            else Result:=BitMapResize(BitMap,1,1);
            end
         else Result:=BitMapResize(BitMap,1,1);
         end;
      end;
  end;

  function  Parse:Boolean;
  type
    TToken = (tokNull,tokChar,tokDigit,tokNumber,tokIdentifier,tokString,
              tokLazyColon,tokPeriod,tokEqual,tokHyphen,
              tokLeftParen,tokRightParen,
              tokLeftBracket,tokRightBracket,tokLeftBrace,tokRightBrace,
              tokDoubleQuote,
              tokSpace,
              tokIf,tokElse,tokEndIf,tokPerform,tokEnd,tokGetImageText);
  const
    TOKEN_TEXTS : array[TToken] of String = (
      'End-of-text','Character','Digit','Number','Identifier','String',
      '..','.','=','-',
      '(',')','[',']','{','}','"',
      '<SPACE>',
      'if','else','endif','perform','end','getImageText');

    RESERVED_WORDS_COUNT = 6;
    RESERVED_WORD_TOKENS : array[0..RESERVED_WORDS_COUNT] of TToken = // must be sorted in ascending order
      (tokNull,tokElse,tokEnd,tokEndIf,tokGetImageText,tokIf,tokPerform);

  type
    TReader         = record
      CharType      :array[Low(Char)..High(Char)] of TToken;
      Length        :Integer; // length of input string; for fast reference
      Lexeme        :String;  // current token
      Number        :Integer; // current token
      Position      :Integer; // position of current token in the input string 'Text'
      SignedNumbers :Boolean; // 'True': treat "-" as a sign rather than a character
      StartPosition :Integer; // current token start position
      Text          :String;  // input string being parsed
      Token         :TToken;  // current token
    end;

  var
    i,ColCount,RowCount,ColWidth,RowHeight,BitMapWidth,BitMapHeight,
    CurrentLineNo,StartLineNo,TileBase:Integer;
    Separator:Char;
    LastSection,LastKey,LastKeyValue,s,EndOfLineCommentSeparator:String;
    LastTextType:TTextType;
    RealTileRect:TRect;
    TreeViewRootNode:TTreeNode; Reader:TReader;

    procedure ClearErrors;
    begin
      OpenForm.ImageMemo.Lines.Clear;
    end;

    function  Error(const Error__:String):Boolean;
    begin // returns 'False' so it conveniently can be used to indicate failure
      OpenForm.ImageMemo.Lines.Add(Error__);
      Result:=False;
    end;

    procedure FindTopLevelTreeNodes(var TreeViewRootNode__:TTreeNode);
    var i:Integer;
    begin // note that this uses 'TTreeNode.Data', hence, it destroys the TreeView->GridRows links used on 'OptionsForm'
      TreeViewRootNode__:=nil;
      with OptionsForm.TreeView1 do
        for i:=0 to Pred(Items.Count) do with Items[i] do
            if Level=0 then begin
               Items[i].Data:=TreeViewRootNode;
               TreeViewRootNode:=Items[i]; // the nodes ends up in reverse order which actually is an advantage because most nodes are descendants of the last group
               end;
    end;

//  lexical analysis

    function Identifier:String;
    var i,j:Integer;
    begin  // only valid after 'NextToken' has parsed an identifier
      with Reader do
        if   Token<>tokNull then begin
             i:=StartPosition; j:=Pred(Position); // trim the string without calling 'Trim' (this is slightly more efficient because it avoids a memory-allocation)
             while (i<=j) and (CharType[Text[i]]=tokSpace) do Inc(i); // i: first non-space character
             while (i<=j) and (CharType[Text[j]]=tokSpace) do Dec(j); // j: last  non-space character
             Result:=System.Copy(Text,i,Succ(j-i));
             end
        else Result:='';
    end;

    function InitializeIniFileReader(TextType__:TTextType):Boolean;
    begin
      LastTextType:=TextType__; LastSection:=''; LastKey:=''; LastKeyValue:='';
      Result:=IniFile_.InitializeIniFileReader;
    end;

    procedure InitializeReader;
    var i:Integer; Ch:Char;
    begin
      with Reader do begin
        FillChar(Reader,SizeOf(Reader),0); SignedNumbers:=False;
        for Ch:=Low(CharType) to High(CharType) do CharType[Ch]:=tokChar;
        for Ch:='0'           to '9'            do CharType[Ch]:=tokDigit;
        for Ch:=Succ(Low(CharType)) to SPACE    do CharType[Ch]:=tokSpace; // note: ascii character-set assumed
        CharType[Low(Char)    ]:=tokNull;
        CharType[PERIOD       ]:=tokPeriod;
        CharType[EQUAL        ]:=tokEqual;
        CharType['-'          ]:=tokHyphen;
        CharType[DOUBLE_QUOTE ]:=tokDoubleQuote;
        CharType[LEFT_PAREN   ]:=tokLeftParen;
        CharType[RIGHT_PAREN  ]:=tokRightParen;
        CharType[LEFT_BRACKET ]:=tokLeftBracket;
        CharType[RIGHT_BRACKET]:=tokRightBracket;
        CharType[LEFT_BRACE   ]:=tokLeftBrace;
        CharType[RIGHT_BRACE  ]:=tokRightBrace;


        for i:=2 to RESERVED_WORDS_COUNT do
            if TOKEN_TEXTS[RESERVED_WORD_TOKENS[i]]<=TOKEN_TEXTS[RESERVED_WORD_TOKENS[Pred(i)]] then
               raise Exception.Create(InternalErrorText+': Skins_.Parser: Reserved words must be in ascending lexical order.');
        end;
    end;

    function LookupVariable(const Name__:String; var Value__:PValue):Boolean;
    var Ch:Char;
    begin // returns a pointer to the variable value when given a variable name 'A' .. 'Z'
      Result:=False;
      if Length(Name__)=1 then begin
         Ch:=UpCase(Name__[1]);
         if (Ch>=Low (Variables)) and
            (Ch<=High(Variables)) then begin
            Result:=True;
            Value__:=Addr(Variables[Ch]);
            end;
         end;
    end;

    function NextCharacter:Char;
    begin // note that 'NextCharacter' doesn't change 'Reader.StartPosition', only 'Reader.Position' and 'Reader.Token'
      with Reader do begin
        if   Position<=Length then begin
             Result:=Text[Position]; Token:=CharType[Result];
             Inc(Position);  // advance to next character
             end
        else begin Token:=tokNull; Result:=NULL_CHAR; end;
      end;
    end;

    function NextToken:TToken;
    var i,Left,Right,Middle:Integer; Ch:Char;
    begin // NextToken;
      with Reader do begin
        if   Position<=Length then begin
             Ch:=Text[Position]; Result:=CharType[Ch];

             while Result=tokSpace do begin // skip white space
               Inc(Position);
               if   Position<=Length then begin
                    Ch:=Text[Position]; Result:=CharType[Ch];
                    end
               else Result:=tokNull;
               end;

             StartPosition:=Position; // token start position

             case Result of
               tokNull   :; // end-of-text
               tokChar   : begin
                                 while (Position<=Length)
                                       and
                                       {
                                       ((CharType[Ch]=tokChar)
                                        or
                                        (CharType[Ch]=tokDigit)
                                        // allowing digits doesn't work because
                                        // ',' is a 'tokChar' and not an
                                        // independent token, hence, a
                                        // parameter-list like '1,2' would be
                                        // parsed as the number '1' followed by
                                        // the identifier ',2'
                                       )
                                       }
                                       (CharType[Ch]=tokChar)
                                       do begin
                                       Inc(Position);
                                       if   Position<=Length then Ch:=Text[Position]
                                       else break;
                                       end;

                                 // check if the identifier is a reserved word
                                 Lexeme:=System.Copy(Text,StartPosition,Position-StartPosition);
                                 Left:=1; Right:=RESERVED_WORDS_COUNT;
                                 repeat
                                   Middle:=(Left+Right) DIV 2; // this cannot overflow here, but generally, a binary lookup should be coded as "left + ( ( right - left ) div 2 )" to avoid overflows in "left + right"  
                                   i:=AnsiCompareText(Lexeme,TOKEN_TEXTS[RESERVED_WORD_TOKENS[Middle]]);
                                   if i<=0 then Right:=Middle-1;
                                   if i>=0 then Left :=Middle+1;
                                 until Left>Right;
                                 if   Pred(Left)<=Right then
                                      Result:=tokIdentifier
                                 else Result:=RESERVED_WORD_TOKENS[Middle];
                           end;
               tokDigit  : begin
                                 Result:=tokNumber; Number:=0;
                                 while (Position<=Length) and
                                       (CharType[Ch]=tokDigit) do begin
                                   if Number <= (High(Number) - Ord(Ch) + Ord('0')) div 10 then
                                      Number := 10*Number+Ord(Ch)-Ord('0')
                                   else begin
                                      Error(Format(NumberTooLargeText__,[TextLines[CurrentLineNo]]));
                                      while (Position<Length) and
                                            (CharType[Text[Succ(Position)]]=tokDigit) do
                                            Inc(Position); // skip overflowing digits
                                      end;
                                   Inc(Position);
                                   if   Position<=Length then Ch:=Text[Position]
                                   else break;
                                   end;
                           end;
               tokPeriod : if    (Position=Length) or (Text[Succ(Position)]<>PERIOD) then
                                 Inc(Position)
                            else begin Result:=tokLazyColon; Inc(Position,2);
                                 end;
               tokHyphen : if    SignedNumbers and (Position<Length) and
                                 (CharType[Text[Succ(Position)]]=tokDigit) then begin
                                 Inc(Position);
                                 Result:=NextToken; Number:=-Number; // may overflow
                                 end
                           else  Inc(Position);
               tokDoubleQuote
                         : begin repeat Inc(Position);
                                 until  (Position>Length) or (Text[Position]=DOUBLE_QUOTE);
                                 Result:=tokString;
                                 Lexeme:=Copy(Text,Succ(StartPosition),Position-Succ(StartPosition));
                                 if Position<=Length then Inc(Position); // 'True': skip the terminating double-quote
                           end;
               else        begin Inc(Position); // single-character token
                           end;
             end; // case
             end
        else Result:=tokNull;
        Token:=Result;
        end;
    end;

    function Match(Token__:TToken):Boolean;
    begin
      Result:=Token__=Reader.Token;
      if   Result then NextToken
      else raise Exception.Create(Format(TokenExpectedText__,[TOKEN_TEXTS[Token__],TextLines[CurrentLineNo]]));
    end;

    function Skip(Token__:TToken):Boolean;
    begin
      with Reader do begin
        Result:=Token__=Token;
        if Result then NextToken;
        end;
    end;

    function SkipUntil(Token__:TToken):Boolean;
    begin
      while (Reader.Token<>Token__) and (Reader.Token<>tokNull) do NextToken;
      Result:=Reader.Token<>tokNull;
    end;

    function StartReading(const Text__:String):TToken;
    begin
      with Reader do begin
        Text:=Text__; Length:=System.Length(Text); Position:=1;
        Result:=NextToken;
        end;
    end;

    function Text(const Terminators__:String):String;
    begin  // only valid after 'NextToken' has parsed an identifier
      if   Reader.Token<>tokNull then with Reader do begin
           while (Position<=Length) and
                 (AnsiPos(Text[Position],Terminators__)=0) do Inc(Position);
           Result:=Identifier; // the text begins with the identifier parsed by the previous call to 'NextToken'
           end
      else Result:='';
    end;

//  end lexical analysis

    function Statements(TreeViewParentNode__:TTreeNode; TreeViewParentNodeAbsoluteIndex__:Integer;
                        RecursionLevel__,StartLineNo__:Integer;
                        var LastFileName__:String):Boolean;

    const MAX_RECURSION_LEVEL=255;
    var LastPerformLineNo:Integer; LastPerformName:String;

      function TreeNodeLookup(const Name__:String; ParentNode__:TTreeNode):TTreeNode;
      begin
        if ParentNode__<>nil then
           if   ParentNode__.HasChildren then begin
                Result:=ParentNode__.GetFirstChild;
                while (Result<>nil) and
                      (AnsiCompareText(Result.Text,Name__)<>0) do
                      Result:=Result.GetNextSibling;
                end
           else Result:=nil // no children: fail
        else begin // toplevel node
           Result:=TreeViewRootNode;
           while (Result<>nil) and
                 (AnsiCompareText(Result.Text,Name__)<>0) do
                 Result:=TTreeNode(Result.Data);
           end;
      end;

      function Statement(RecursionLevel__:Integer):Boolean;
      type
        TVariableType   =(varNull,varSettings,varPicture,varColWidth,varRowHeight,varColCount,varRowCount,varColorTheme,varVariable);
        TVariable       =record
           Name         :String;
           VariableType :TVariableType; // the record isn't defined with a variant part because some fields are used for storing additional information}
           // case VariableType = varSettings
           AbsoluteIndex:Integer; // 'Node.AbsoluteIndex' uses a full traversal of the tree, hence, it's better to cache the number instead of repeating this very costly calculation
           Node         :TTreeNode;
           // case VariableType = varPicture
           PictureType  :TPictureType;
           Quadrants    :TQuadrantSet;
           TileNo       :Integer;
          end;

        function SectionStatement(var TreeViewParentNode__:TTreeNode;
                                  var TreeViewParentNodeAbsoluteIndex__:Integer):Boolean; forward;

        function Expression(const Variable__:TVariable; var Value__:TValue; ReadValue__:Boolean):Boolean;
        var StartPosition,Length:Integer;
        // if 'ReadValue__' is 'True' then the reader is set up to parse the (text) value of the 'Value__' parameter;
        // otherwise, the reader continue from its current state

          function Term:Boolean;
          var TreeViewNodeAbsoluteIndex:Integer; TreeViewNode:TTreeNode;

            function FileName:Boolean;
            var i:Integer; Qualifier:String;
            begin
              Result:=StrEqual(Reader.Lexeme,KEY_FILE_NAME);
              if Result then begin
                 Value__.Text:=OpenForm.CurrentFileName;
                 Value__.ValueType:=valText;
                 NextToken;
                 if Reader.Token=tokPeriod then begin
                    NextToken;
                    Qualifier:=Text('}');
                    if Match(tokIdentifier) then begin
                       if      StrEqual(Qualifier,KEY_FILE_NAME_PATH) then
                               Value__.Text:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Value__.Text))
                       else if StrEqual(Qualifier,KEY_FILE_NAME_NAME) then
                               Value__.Text:=ExtractFileNameWithoutPathAndExtension(Value__.Text)
                       else if StrEqual(Qualifier,KEY_FILE_NAME_SHORTNAME) then begin
                               Value__.Text:=ExtractFileNameWithoutPathAndExtension(Value__.Text);
                               i:=StrAnsiPosCI(KEY_FILE_NAME_SHORTNAME_TERMINATOR,Value__.Text);
                               if i<>0 then Value__.Text:=Trim(System.Copy(Value__.Text,1,Pred(i)));
                               end
                       else if StrBeginsWith(Qualifier,KEY_FILE_NAME_EXT) then
                               Value__.Text:=ExtractFileExt(Value__.Text)
                       else if StrEqual(Qualifier,KEY_FILE_NAME_FOLDER_SHORTNAME) then
                               Value__.Text:=ExtractFileName(StrWithoutTrailingPathDelimiter(ExtractFilePath(Value__.Text)))
                       else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_FILE_NAME,TextLines[CurrentLineNo]]));
                       end;
                    end;
                 end;
            end; // FileName

            function NextNumber(StopToken__:TToken):Boolean;
            var oRect:TRect;
            begin
              with Reader do begin
                while (Token<>tokNull) and
                      (Token<>StopToken__) and
                      (Token<>tokNumber) do begin
                      if   Token=tokLeftBrace then begin // evaluate '{..}'
                           oRect:=Value__.Rect;
                           Result:=Term;
                           if   Result then
                                if   Value__.ValueType=valNumber then begin
                                     Token:=tokNumber; Number:=Value__.Number;
                                     end
                                else if   Value__.ValueType=valText then // splice the text into the input-steam
                                          StartReading(Value__.Text+SPACE+Copy(Text,StartPosition,MaxInt))
                                     else NextToken
                           else Token:=tokNull;
                           Value__.Rect:=oRect;
                           end
                      else NextToken;
                      end;
                Result:=Token=tokNumber;
                end;
            end; // NextNumber

            function Tile:Boolean;
            var Count:Integer; oSignedNumbers:Boolean;
            begin
              oSignedNumbers:=Reader.SignedNumbers; Reader.SignedNumbers:=True;

              Result:=StrEqual(Reader.Lexeme,KEY_TILE);
              if Result and (NextToken=tokIdentifier) and
                 StrEqual(KEY_COMMON_SKIN_WALL,Text('.}')) then with Value__.Rect do begin
                 NextToken;
                 Left:=1; Top:=3; Right:=0; Bottom:=0; // top-left: default common skins tile layout, 1-based numbers
                 if   (ColWidth<>MaxInt) and (RowHeight<>MaxInt) then begin
                      if      (RowCount= 6) or (RowCount= 8) then begin // 6: common skin with player directions but without animations
                              Right:=3; Bottom:=4; // 3x2 format
                              end
                      else if (RowCount=10) or (RowCount=12) then begin // 10: common skin without wall cap ("old" YSokoban format); 12: including player-direction images
                              Right:=4; Bottom:=6; // 4x4 format
                              end;
                      end
                 else raise Exception.Create(Format('"{%s %s ..." : '+TEXT_NOT_IMPLEMENTED,[KEY_TILE,KEY_COMMON_SKIN_WALL]));
                 end
              else begin
                Count:=0;
                Result:=Result and NextNumber(tokRightBrace);
                if Result then with Value__.Rect do begin
                   Left:=Reader.Number; Right:=Left; Inc(Count);
                   if NextToken=tokLazyColon then begin // columns C1..C2
                      Result:=NextNumber(tokRightBrace);
                      if Result then begin
                         Right:=Reader.Number; NextToken; Inc(Count);
                         end;
                      end;
                   end;

                if Result and NextNumber(tokRightBrace) then with Value__.Rect do begin
                   Top:=Reader.Number; Bottom:=Top; Inc(Count);
                   if NextToken=tokLazyColon then begin // rows R1..R2
                      Result:=NextNumber(tokRightBrace);
                      if Result then begin
                         Bottom:=Reader.Number; NextToken; Inc(Count);
                         end;
                      end;
                   end;

                if Result and (Count=1) then with Value__.Rect do begin
                   // kludge: a 1-index "[index]" reference to a tile in the columns x rows matrix is 0-based
                   // as opposed to a normal 2-index "[columns,rows]" reference which is 'TileBase'-based
                   if (ColCount>0) and (RowCount>0) then begin
                      if Left<0 then Left:=Succ(ColCount*RowCount)+Left;
                      Top:=Succ(Left div ColCount); // row number
                      Left:=Succ(Left-Pred(Top)*ColCount); // column number
                      Right:=Left; Bottom:=Top;
                      end
                   else Result:=False;
                   end;
                end;

              if Result then with Value__.Rect do begin
                 if (ColWidth<>MaxInt) and (RowHeight<>MaxInt) then begin // image loaded
                    if Left  <0 then Left  :=TileBase+ColCount+Left  ;
                    if Top   <0 then Top   :=TileBase+RowCount+Top   ;
                    if Right <0 then Right :=TileBase+ColCount+Right ;
                    if Bottom<0 then Bottom:=TileBase+RowCount+Bottom;

                    if TileBase=0 then begin // adjust values to 1-based numbers
                       Inc(Left); Inc(Top); Inc(Right); Inc(Bottom);
                       end;

                    Result:=(Left>0) and (Right <=ColCount) and
                            (Top >0)      and (Bottom<=RowCount) and
                            (Left<=Right) and (Top<=Bottom);

                    if Result then begin
                       Left  :=Pred(Left)*ColWidth +RealTileRect.Left;
                       Right :=Right     *ColWidth +RealTileRect.Right;
                       Top   :=Pred(Top) *RowHeight+RealTileRect.Top;
                       Bottom:=Bottom    *RowHeight+RealTileRect.Bottom;
                       Value__.ValueType:=valRect;
                       end;
                    end
                 else begin // image not loaded, perform a syntactic check only and return the raw tile
                    Result:=(Left<=Right) and (Top<=Bottom);
                    if Result then begin
                       if TileBase=0 then begin // adjust values to 1-based numbers
                          Inc(Left); Inc(Top); Inc(Right); Inc(Bottom);
                          end;
                       Value__.ValueType:=valTile;
                       end;
                    end;
                 end;

              Reader.SignedNumbers:=oSignedNumbers;
            end; // Tile

            function Rect:Boolean;
            var Count:Integer;
            begin
              Count:=0;
              Result:=StrEqual(Reader.Lexeme,KEY_RECT);
              if Result then begin
                 with Value__.Rect do begin Left:=0; Top:=0; Right:=0; Bottom:=0; end;

                 if Result and (NextToken=tokIdentifier) and
                    StrEqual(KEY_COMMON_SKIN_WALL,Text('.}')) then with Value__ do begin
                    ValueType:=valRect; Text:=KEY_COMMON_SKIN_WALL; // blank text: normal rect; non-blank: special rect, at the moment the only special type is 'CommonSkinWall'
                    NextToken;
                    end
                 else begin
                    if Result and NextNumber(tokRightBrace) then with Value__.Rect do begin
                       repeat case Count of
                                0: Left  :=Reader.Number;
                                1: Top   :=Reader.Number;
                                2: Right :=Reader.Number;
                                3: Bottom:=Reader.Number;
                                else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_RECT,TextLines[CurrentLineNo]]));
                              end; // case
                              Inc(Count); NextToken;
                       until  (not NextNumber(tokRightBrace));

                       if (Count<=1)                         then Top   :=0;
                       if (Count<=2) and (ColWidth <>MaxInt) then Right :=Left+ColWidth +RealTileRect.Right; // 'RealTileRect' contains adjustment-values at this time, hence, they're simply added
                       if (Count<=3) and (RowHeight<>MaxInt) then Bottom:=Top +RowHeight+RealTileRect.Bottom;

                       Result:= (Left>=0) and (Right >=Left) and (Right <=BitMapWidth) and
                                (Top >=0) and (Bottom>=Top ) and (Bottom<=BitMapHeight);
                       if Result then with Value__ do begin
                          ValueType:=valRect; Text:=''; // blank text: normal rect; non-blank: special rect, at the moment the only special type is 'CommonSkinWall'
                          end
                       else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_RECT,TextLines[CurrentLineNo]]));
                       end;
                    end;
                 end;
            end; // Rect

            function IsInteger:Boolean;
            var i:Integer;
            begin
              Result:=StrEqual(Reader.Lexeme,KEY_IS_INTEGER);
              if Result then begin
                 NextToken;
                 Skip(tokLeftParen);

                 if        Reader.Token=tokLeftBrace then Result:=Term
                 else if   Reader.Token=tokNumber then
                           Value__.ValueType:=valNumber
                      else Result:=False;

                 if   Result and
                      ((Value__.ValueType=valNumber)
                       or
                       ((Value__.ValueType=valText)
                        and
                        (Value__.Text<>'')
                        and
                        SafeStrToInt(Value__.Text,False,i)
                       )
                      ) then
                      Value__.Text:=TEXT_YES
                 else Value__.Text:='';

                 Value__.ValueType:=valText;
                 Skip(tokRightParen);
                 end;
            end; // IsInteger

            function PixelCodes:Boolean;
            var i:Integer; oSignedNumbers:Boolean; Side:TRectangleSide; s:String;

              function  GetPixelCodes(CellCol__,CellRow__:Integer):Boolean;
              var B:TBitMap;
              begin
              SkinPixelCodes.HasPixelCodes:=False;
              if (OpenForm.CurrentImageFileName<>'') and
                 (OpenForm.Image1.Picture.BitMap<>nil) and
                 (OpenForm.Texts[tTextFile].FileName='') then begin
                 B:=OpenForm.Image1.Picture.BitMap;
                 Result:=(B<>nil) and
                         (ColWidth<>MaxInt) and (RowHeight<>MaxInt) and
                         Self.GetPixelCodes(B,CellCol__,CellRow__,ColWidth,RowHeight,SkinPixelCodes); // 0-based cell columns and rows
                 end
              else
                 //'PixelCodes' isn't implemented for dynamically loaded images, only for image-based skins
                 raise Exception.Create(KEY_PIXEL_CODES+': '+TEXT_NOT_IMPLEMENTED+NL+NL+TextLines[CurrentLineNo]);
              SkinPixelCodes.HasPixelCodes:=Result;
              end;

            begin // PixelCodes
              Result:=StrEqual(Reader.Lexeme,KEY_PIXEL_CODES);
              if Result then with Value__ do begin
                 NextToken; Result:=False; ValueType:=valNull; Text:='';
                 if Match(tokPeriod) then begin
                    if Reader.Token=tokNumber then with SkinPixelCodes do begin // 'True': get pixel codes from cell [x,y]
                       SkinPixelCodes.HasPixelCodes:=False;
                       i:=Reader.Number; // 'i' = 0-based cell column
                       NextToken;
                       if Match(TokPeriod) and (Reader.Token=tokNumber) and
                          GetPixelCodes(i,Reader.Number) then begin // Reader.Number' = 0-based cell row
                          Result:=True; NextToken;
                          ValueType:=valText; Text:=BooleanText[True];
                          end;
                       end
                    else
                        if SkinPixelCodes.HasPixelCodes then begin
                           s:=Reader.Lexeme;
                           if Match(tokIdentifier) then begin
                              Side:=Low(Side); Result:=StrEqual(s,KEY_RECTANGLE_SIDES[Side]);
                              while (not Result) and (Side<High(Side)) do begin
                                Inc(Side); Result:=StrEqual(s,KEY_RECTANGLE_SIDES[Side]);
                                end;
                              Result:=Result and Match(tokPeriod) and NextNumber(tokRightBrace) and
                                      (Reader.Number>=Low (SkinPixelCodes.LineLengths[Side])) and
                                      (Reader.Number<=High(SkinPixelCodes.LineLengths[Side]));
                              if Result then begin
                                 Number:=SkinPixelCodes.LineLengths[Side,Reader.Number];
                                 oSignedNumbers:=Reader.SignedNumbers; Reader.SignedNumbers:=True;
                                 NextToken;
                                 if NextNumber(tokRightBrace) then begin
                                    Inc(Number,Reader.Number);
                                    NextToken;
                                    end;
                                 ValueType:=valText;
                                 if Number>=0 then Text:=IntToStr(Number)
                                 else Text:=''; // unspecified lines has the value '-1'; return an empty string to indicate that the no value is available for this line index
                                 Reader.SignedNumbers:=oSignedNumbers;
                                 end;
                              end;
                           end
                        else begin
                           raise Exception.Create(Format(PixelCodesNotInitializedText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_PIXEL_CODES,TextLines[CurrentLineNo]]));
                           end;
                    end;
                 if not Result then raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_PIXEL_CODES,TextLines[CurrentLineNo]]));
                 end;
            end; // PixelCodes

            function SubString:Boolean;
            var StartPosition,Length:Integer; s:String;
            begin
              Result:=StrEqual(Reader.Lexeme,KEY_SUB_STRING);
              if Result then with Value__ do begin
                 NextToken;
                 Skip(tokLeftParen);

                 if   Reader.Token=tokLeftBrace then Result:=Term
                 else if   (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then begin
                           Value__.ValueType:=valText;
                           Value__.Text:=Reader.Lexeme;
                           NextToken;
                           end
                      else Result:=False;

                 if Result and (ValueType=valText) then begin
                    s:=Text;
                    if NextNumber(tokRightBrace) then begin
                       StartPosition:=Succ(Reader.Number); // 'Succ': Pascal-strings starts with index 1 (note that 'Succ' may wrap around)
                       NextToken;
                       if   NextNumber(tokRightBrace) then begin
                            Length:=Reader.Number;
                            NextToken;
                            end
                       else Length:=MaxInt;

                       Value__.ValueType:=valText;
                       if   StartPosition>=1 then
                            Value__.Text:=Copy(s,StartPosition,Max(0,Length))
                       else Value__.Text:='';
                       end
                    else
                       raise Exception.Create(Format(TokenExpectedText__,[TOKEN_TEXTS[tokNumber],TextLines[CurrentLineNo]]));
                    end
                 else Result:=False;

                 if not Result then
                    raise Exception.Create(Format(TokenExpectedText__,[TOKEN_TEXTS[tokString],TextLines[CurrentLineNo]]));

                 Skip(TokRightParen);
                 end;
            end; // SubString;

            function Add:Boolean;
            var ResultValue:TValue;
            begin // 'Add' adds numbers and concatenate strings, and returns the result as a string
              Result:=StrEqual(Reader.Lexeme,KEY_ADD);
              if Result then with Value__ do begin
                 NextToken;
                 Skip(tokLeftParen);

                 ResultValue.ValueType:=valNull;
                 ResultValue.Text:='';
                 ResultValue.Number:=0;

                 repeat
                   if        Reader.Token=tokLeftBrace then Result:=Term
                   else if   (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then begin
                             Value__.ValueType:=valText;
                             Value__.Text:=Reader.Lexeme;
                             NextToken;
                             end
                   else if   (Reader.Token=tokNumber) then begin
                             Value__.ValueType:=valNumber;
                             Value__.Number:=Reader.Number;
                             NextToken;
                             end;

                   if        Result then
                             if         Value__.ValueType=valNumber then begin
                                        if   ResultValue.ValueType<>valText then begin
                                             ResultValue.ValueType:=valNumber;
                                             Inc(ResultValue.Number,Value__.Number);
                                             end
                                        else ResultValue.Text:=ResultValue.Text+IntToStr(Value__.Number)
                                        end
                             else if    Value__.ValueType=valText then begin
                                        if   ResultValue.ValueType<>valNumber then begin
                                             ResultValue.ValueType:=valText;
                                             ResultValue.Text:=ResultValue.Text+Value__.Text;
                                             end
                                        else if   SafeStrToInt(Value__.Text,False,Value__.Number) then
                                                  Inc(ResultValue.Number,Value__.Number)
                                             else Result:=False;
                                        end
                                   else Result:=False;

                 until  (not Result) or (Reader.Token=tokRightParen) or (Reader.Token=tokRightBrace) or (Reader.Token=tokNull);

                 Skip(tokRightParen);

                 if     Result then begin
                        Value__.ValueType:=valText;
                        if   ResultValue.ValueType=valNumber then
                             Value__.Text:=IntToStr(ResultValue.Number)
                        else Value__.Text:=ResultValue.Text;
                        end
                 else   raise Exception.Create(Format(TokenExpectedText__,[TextOrNumberExpectedText,TextLines[CurrentLineNo]]));
                 end;
            end; // Add

            function BackgroundColor:Boolean;
            const DEFAULT_CORNER:TCornerType=ctBottomLeft;
            var s:String; DirectionToText:array[TDirection] of String;
                Color:TColor; R:TRect; B:TBitMap;
                PictureType:TPictureType; Corner:TCornerType;
                Direction:TDirection; DirectionSet:TDirectionSet;
            begin
              Result:=StrEqual(Reader.Lexeme,KEY_BACKGROUND_COLOR);
              if Result then begin
                 NextToken;
                 Corner:=DEFAULT_CORNER;

                 if (Reader.Token<>TokRightBrace) and (Reader.Token<>TokNull) then begin
                    for Direction:=Low(Direction) to High(Direction) do
                        DirectionToText[Direction]:=DIRECTION_TO_TEXT[Direction];
                    DirectionToText[Up]:=TopText;
                    DirectionToText[Down]:=BottomText;
                    DirectionSet:=[];
                    repeat
                      s:='';
                      if      Reader.Token=tokLeftBrace then begin
                              Result:=Term;
                              if Result and (Value__.ValueType=valText) then begin
                                 s:=Value__.Text; NextToken;
                                 end;
                              end
                      else if Reader.Token=tokIdentifier then begin
                              s:=Reader.Lexeme; NextToken;
                              end;
                      if s<>'' then
                         for Direction:=Low(DIRECTION_TO_TEXT) to High(DIRECTION_TO_TEXT) do
                             if StrEqual(s,DirectionToText[Direction]) then
                                if   OPPOSITE_DIRECTION[Direction] in DirectionSet then
                                     Result:=False
                                else Include(DirectionSet,Direction);
                    until (not Result) or (Reader.Token=tokRightBrace) or (Reader.Token=tokNull);

                    if      Left in   DirectionSet then
                            if        Down in DirectionSet then Corner:=ctBottomLeft
                            else if   Up in DirectionSet   then Corner:=ctTopLeft
                                 else Result:=False
                    else if Right in DirectionSet then
                            if        Down in DirectionSet then Corner:=ctBottomRight
                            else if   Up in DirectionSet   then Corner:=ctTopRight
                                 else Result:=False;
                    end;

                 if Result then with Value__ do begin
                    Text:=''; ValueType:=valText;
                    if   (Variable__.VariableType=varSettings) and (Variable__.Node<>nil) and
                         (SettingsEditor[TSettings(Variable__.AbsoluteIndex)]=seColor) and
                         OptionsForm.IsAPictureSetting(Variable__.AbsoluteIndex,PictureType) then begin

                         R:=Classes.Rect(0,0,0,0); B:=nil;
                         if (PictureType=WorkBitMap.PictureType) and
                            (WorkBitMap.FileName<>'') and
                            (RectWidth (WorkBitMap.LastRect)>0) and
                            (RectHeight(WorkBitMap.LastRect)>0)
                            then begin
                            R:=WorkBitMap.LastRect;
                            B:=WorkBitMap.BitMap;
                            end
                         else
                            try     if LoadPictureFromFile(PictureType) then with Pict do with OrgBitMap do begin
                                       R:=Classes.Rect(0,0,Width,Height);
                                       B:=OrgBitMap;
                                       end;
                            finally Pict.FileName:=''; Pict.SourceRect:=Classes.Rect(0,0,0,0); // ensure a reload of the full image if it's used again later
                            end;

                         if  (B<>nil) and (RectWidth(R)>0) and (RectHeight(R)>0) then with B do with R do begin
                             case Corner of
                               ctTopLeft           : Color:=Canvas.Pixels[Left,Top];
                               ctTopRight          : Color:=Canvas.Pixels[Pred(Right),0];
                               ctBottomLeft        : Color:=Canvas.Pixels[Left,Pred(Bottom)];
                               else {ctBottomRight}  Color:=Canvas.Pixels[Pred(Right),Pred(Bottom)];
                             end; // case
                             Text:=RGB_BGR(IntToHex(Graphics.ColorToRGB(Color),6));
                             end;

                         end
                    else Error(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_BACKGROUND_COLOR,TextLines[CurrentLineNo]]));
                    end;
                 end;
            end; // BackgroundColor

            function Base64Decode:Boolean;
            var EncodedString,FileName,FileExtension:String;
            begin
              Result:=StrEqual(Reader.Lexeme,KEY_BASE64_DECODE);
              if Result then with Value__ do begin
                 NextToken;
                 Skip(tokLeftParen);

                 EncodedString:=''; FileName:=''; FileExtension:='';

                 if   Reader.Token=tokLeftBrace then Result:=Term
                 else if   (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then begin
                           Value__.ValueType:=valText;
                           Value__.Text:=Reader.Lexeme;
                           NextToken;
                           end
                      else Result:=False;
                 if   Result and (ValueType=valText) and (Text<>'') then
                      EncodedString:=Text
                 else Result:=False;

                 if   Result then
                      if Reader.Token=tokLeftBrace then Result:=Term
                      else if   (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then begin
                                Value__.ValueType:=valText;
                                Value__.Text:=Reader.Lexeme;
                                NextToken;
                                end
                           else Result:=False;
                 if   Result and (ValueType=valText) then begin
                      FileName:=Text;
                      FileExtension:=ExtractFileExt(FileName);
                      end
                 else Result:=False;

                 if   Result then
                      if   Reader.Token=tokLeftBrace then Result:=Term
                      else if   (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then begin
                                Value__.ValueType:=valText;
                                Value__.Text:=Reader.Lexeme;
                                NextToken;
                                end
                           else begin Value__.ValueType:=valNull;
                                      Result:=Reader.Token=tokRightBrace;
                                end;
                 if   Result then
                      if   ValueType=valText then FileExtension:=Text
                      else Result:=ValueType=valNull;

                 if   Result then begin
                      if (FileExtension='') or (FileExtension[1]<>PERIOD) then
                         FileExtension:=PERIOD+FileExtension;
                      FileName:=StrWithTrailingPathDelimiter(OptionsForm.SettingsPath)+
                                TEXT_APPLICATION_TITLE_SHORT+' - '+
                                ExtractFileName(ChangeFileExt(FileName,FileExtension));
                      if Misc_.Base64DecodeToFile(EncodedString,FileName) then begin
                         Value__.ValueType:=valText;
                         Value__.Text:=FileName;
                         end
                      else begin Value__.ValueType:=valNull;
                                 raise Exception.Create('');
                           end;
                      end
                 else raise Exception.Create(Format(TokenExpectedText__,[TOKEN_TEXTS[tokString],TextLines[CurrentLineNo]]));

                 Skip(tokRightParen);
                 end;
            end; // Base64Decode

            function  ColorTheme:Boolean;
            var ColorTheme_:TColorTheme;
            begin
              Result:=StrEqual(Reader.Lexeme,KEY_COLOR_THEME);
              if Result then with Value__ do begin
                 NextToken;
                 Skip(tokEqual);
                 Skip(tokLeftParen);

                 if   Reader.Token=tokLeftBrace then Result:=Term
                 else if   (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then begin
                           Value__.ValueType:=valText;
                           Value__.Text:=Reader.Lexeme;
                           NextToken;
                           end
                      else Result:=False;
                 if   Result and (ValueType=valText) and (Text<>'') then begin
                      Result := False;
                      for ColorTheme_ := Low( ColorTheme_ ) to High( ColorTheme_ ) do
                          if StrEqual( Text, ColorThemeText[ ColorTheme_ ] ) then begin
                             Result := Assigned( OptionsForm );
                             if Result then
                                OptionsForm.SetColorTheme( ColorTheme_ );
                             end;
                      end;

                 Skip(tokRightParen);
                 end;
            end; // ColorTheme

            function Section:Boolean;
            var s:String;
            begin
              Result:=StrEqual(Reader.Lexeme,KEY_SECTION);
              if Result then with Value__ do begin
                 NextToken;
                 Skip(tokLeftParen);

                 if   Reader.Token=tokLeftBrace then Result:=Term
                 else if   (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then begin
                           Value__.ValueType:=valText;
                           Value__.Text:=Reader.Lexeme;
                           NextToken;
                           end
                      else Result:=False;
                 if   Result and (ValueType=valText) and (Text<>'') then
                 else Result:=False;

                 if   Result then begin
                      if (Text=LastSection) and (LastKey='') and (LastTextType=tTextFile) then
                         // caching the last lookup often saves time because
                         // the pattern "if {section.key} xxx = {section.key}"
                         // is a common script pattern
                         Value__.Text:=LastKeyValue
                      else begin
                         if   LastTextType<>tTextFile then InitializeIniFileReader(tTextFile);
                         if   ReadSection(OpenForm.Texts[tTextFile].Memo.Lines,Text,s) then begin
                              LastSection:=Text; LastKey:=''; LastKeyValue:=s;
                              Value__.Text:=s;
                              end
                         else Result:=False;
                         end;
                      end;

                 Skip(tokRightParen);
                 end;
            end; // Section

            function ColumnsAndRows:Boolean; // returns values for columns, rows, width, and height
            begin
              if      StrEqual(Reader.Lexeme,KEY_COLUMNS) then with Value__ do begin
                      ValueType:=valNumber; Number:=ColCount;
                      NextToken; Result:=True;
                      end
              else if StrEqual(Reader.Lexeme,KEY_ROWS) then with Value__ do begin
                      ValueType:=valNumber; Number:=RowCount;
                      NextToken; Result:=True;
                      end
              else if StrEqual(Reader.Lexeme,KEY_COL_WIDTH) then with Value__ do begin
                      ValueType:=valNumber; Number:=ColWidth;
                      NextToken; Result:=True;
                      end
              else if StrEqual(Reader.Lexeme,KEY_ROW_HEIGHT) then with Value__ do begin
                      ValueType:=valNumber; Number:=RowHeight;
                      NextToken; Result:=True;
                      end
              else Result:=False;
            end; // ColumnsAndRows

            function KeyValue(RecursionLevel__:Integer):Boolean;
            var i:Integer; SelfKeyValue:Boolean; Section,Key:String; VariableValue:PValue;
            begin
              Result:=RecursionLevel__<MAX_RECURSION_LEVEL;
              if   Result then begin
                   Section:='';

                   if   Reader.Token<>tokRightBrace then begin
                        Key:=Text('.{}'); NextToken;
                        end
                   else Key:='';

                   if   (Reader.Token=tokPeriod) and
                        StrEqual(Key,KEY_SELF) then begin
                        SelfKeyValue:=True;
                        Key:=''; NextToken;
                        if        Reader.Token=tokLeftBrace then begin
                                  Result:=Term;
                                  if Result then with Value__ do
                                     if ValueType=valText then Key:=Text
                                     else if ValueType=valNumber then Key:=IntToStr(Number)
                                     else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokIdentifier],TextLines[CurrentLineNo]]));
                                  end
                        else if   Reader.Token<>tokRightBrace then begin
                                  Key:=Text('.{}'); NextToken;
                                  end;
                        end
                   else SelfKeyValue:=False;

                   if   Reader.Token=tokPeriod then begin
                        Section:=Key; Key:=''; NextToken;
                        end;
                   if   (Key='') and (Reader.Token=tokIdentifier) then begin
                        Key:=Text('.{}'); NextToken;
                        end;
                   if   (Key='') and (Reader.Token=tokLeftBrace) then begin
                        NextToken;
                        Result:=KeyValue(Succ(RecursionLevel__)) and Match(tokRightBrace);
                        if Result then Key:=Value__.Text; // 'Value.Text': the return-value after the recursive invocation of 'KeyValue'
                        end;
                   if   Result and (Key<>'') then begin
                        Value__.ValueType:=valText;

                        if SelfKeyValue then begin
                           if (Section=LastSection) and (Key=LastKey) and (LastTextType=TextType__) then
                              Value__.Text:=LastKeyValue
                           else begin
                              if LastTextType<>TextType__ then InitializeIniFileReader(TextType__);
                              Value__.Text:=ReadString(TextLines,EQUAL,Section,Key,'');
                              if (Value__.Text='') and
                                 LookupVariable(Key,VariableValue) then with VariableValue^ do begin
                                 if      ValueType=valText   then Value__.Text:=Text
                                 else if ValueType=valNumber then begin
                                         Value__.Text:=IntToStr(Number);
                                         //Value__.ValueType:=valNumber;
                                         //Value__.Number:=Number;
                                         end
                                      else Result:=False;
                                 end;
                              LastSection:=Section; LastKey:=Key; LastKeyValue:=Value__.Text;
                              end;
                           end
                        else
                           if (Section=LastSection) and (Key=LastKey) and (LastTextType=tTextFile) then
                              // caching the last lookup often saves time because
                              // the following pattern occurs frequently in scripts:
                              //
                              // if {section.key}
                              //     xxx = {section.key}
                              // endif
                              //
                              Value__.Text:=LastKeyValue
                           else begin
                              if LastTextType<>tTextFile then InitializeIniFileReader(tTextFile);
                              Value__.Text:=ReadString(OpenForm.Texts[tTextFile].Memo.Lines,Separator,Section,Key,'');
                              if (Value__.Text='') and
                                 LookupVariable(Key,VariableValue) then with VariableValue^ do begin
                                 if      ValueType=valText   then Value__.Text:=Text
                                 else if ValueType=valNumber then begin
                                         Value__.Text:=IntToStr(Number);
                                         //Value__.ValueType:=valNumber;
                                         //Value__.Number:=Number;
                                         end
                                      else Result:=False;
                                 end;
                              LastSection:=Section; LastKey:=Key; LastKeyValue:=Value__.Text;
                              end;

                        if EndOfLineCommentSeparator<>'' then with Value__ do begin
                           i:=AnsiPos(EndOfLineCommentSeparator,Text);
                           if i<>0 then Text:=Trim(Copy(Text,1,Pred(i)));
                           end;
                        end;
                   end
              else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[ScriptTooDeeplyNestedText,TextLines[CurrentLineNo]]));
            end; // KeyValue

          begin // Term
            Result:=Match(tokLeftBrace);
            if Result then with Reader do
               if   Token=tokIdentifier then begin
                    Result:=False;
                    if Lexeme<>'' then
                       case UpCase(Reader.Lexeme[1]) of
                         // dispatch on the first character;
                         // when new functions are added, 'TSkins.Initialize'
                         // must be updated with guards against mismatches
                         // between names and the dispatch character
                         'A'  : Result:=Add;
                         'B'  : Result:=BackgroundColor or Base64Decode;
                         'C'  : Result:=ColumnsAndRows;
                         'F'  : Result:=FileName;
                         'I'  : Result:=IsInteger;
                         'P'  : Result:=PixelCodes;
                         'R'  : Result:=Rect or ColumnsAndRows;
                         'S'  : Result:=SubString or Section;
                         'T'  : Result:=Tile;
                       end; // case
                    if not Result then Result:=KeyValue(RecursionLevel__);
                    Result:=Result and Match(tokRightBrace);
                    end
               else if Token=tokLeftBrace then begin
                       if RecursionLevel__<MAX_RECURSION_LEVEL then begin
                          Inc(RecursionLevel__);
                          if Term and
                             (Value__.ValueType=valText) then begin
                             StartReading(LEFT_BRACE+Value__.Text+System.Copy(Reader.Text,Reader.StartPosition,MaxInt));
                             Result:=Term;
                             end
                          else
                             raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokLeftBrace],TextLines[CurrentLineNo]]));
                          Dec(RecursionLevel__);
                          end
                       else
                          raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[ScriptTooDeeplyNestedText,TextLines[CurrentLineNo]]));
                       end
                    else if   Skip(tokRightBrace) then with Value__ do begin // empty '{}'
                              ValueType:=valText; Text:='';
                              end
                         else if Token=tokLeftBracket then begin
                                 Result:=SectionStatement(TreeViewNode,TreeViewNodeAbsoluteIndex)
                                         and
                                         Match(tokRightBracket);
                                 if Result then with Value__ do begin
                                    ValueType:=valText;
                                    Text:=OptionsForm.SettingsString[TSettings(TreeViewNodeAbsoluteIndex)];
                                    if SafeStrToInt(Text,False,Number) then ValueType:=valNumber;
                                    end;
                                 end
                              else if Token=tokString then with Value__ do begin
                                      ValueType:=valText;
                                      Text:=Reader.Lexeme;
                                      end
                              else if Token=tokNumber then with Value__ do begin
                                      ValueType:=valNumber;
                                      Number:=Reader.Number;
                                      end
                                   else Result:=False;
          end; // Term

        begin // Expression
          if   ReadValue__ then begin
               Length:=System.Length(Value__.Text); StartPosition:=1;
               while (StartPosition<Length) and (Value__.Text[StartPosition]<=SPACE) do Inc(StartPosition); // locate first non-blank character, if any

               if (Length=0) or (Value__.Text[StartPosition]<>LEFT_BRACE) then begin
                  Value__.ValueType:=valText; Result:=True; // plain text
                  end
                else begin // "{" [ term ] "}"
                   StartReading(Value__.Text);
                   Result:=Term;
                   end
               end
          else if Reader.Token=tokLeftBrace then
                  Result:=Term
               else if (Reader.Token=tokIdentifier) or (Reader.Token=tokString) then with Value__ do begin
                       ValueType:=valText;
                       Text:=Reader.Lexeme;
                       Result:=True;
                       NextToken;
                       end
                    else if Reader.Token=tokNumber then with Value__ do begin
                            ValueType:=valNumber;
                            Number:=Reader.Number;
                            Result:=True;
                            NextToken;
                            end
                         else Result:=False;
        end; // Expression

        function Assignment:Boolean;
        var ColorTheme : TColorTheme;
            Variable:TVariable;
            Value:TValue;

          function  IsAFileNameSetting(const Variable__:TVariable):Boolean;
          begin
            with Variable__ do
              Result:=(VariableType=varSettings) and
                      OptionsForm.IsAFileNameSetting(Node);
          end;

          function  MakeWorkBitMapTile(const Variable__:TVariable; const Value__:TValue; WallTopQuadrantTransformation__:Boolean):Boolean;
          {
          const WALL_CAP_QUADRANT_TRANSFORMATION:array[TQuadrantType] of TQuadrantType =
                (qTopLeft,qTopRight,qBottomLeft,qBottomRight); // an identity-mapping for testing; nothing is transformed
          }
          // re-arrangement of the tile-quadrants when a wall cap is created
          // based on the isolated wall tile; this re-arrangement assumes the
          // wall cap is shown midways between the cells
          const WALL_CAP_QUADRANT_TRANSFORMATION:array[TQuadrantType] of TQuadrantType =
                (qBottomRight,qBottomLeft,qTopRight,qTopLeft);

          var i,H,W:Integer; R0,R1:TRect; q1,q2:TQuadrantType; oWallType:TWallType;
              Variable1:TVariable;
          begin // MakeWorkBitMapTile
            Result:=(Variable__.VariableType=varPicture) and
                    ((Value__.ValueType=valRect) or (Value__.ValueType=valTile));

            if   Result then
                 if (Variable__.PictureType=ptWall) and
                    (Value__.Text<>'') and
                    (Value__.Text=KEY_COMMON_SKIN_WALL) and
                    (Value__.ValueType=valRect) then begin
                    if OpenWorkBitMap(ptWall,False,ColWidth,RowHeight) and
                       LoadPictureFromFile(ptWall) then with WorkBitMap do begin
                       if               (ColWidth<>MaxInt) and (RowHeight<>MaxInt) then
                                        if   MakeWallFromCommonSkinSource(ColWidth,RowHeight,Pict.OrgBitMap,WorkBitMap.BitMap,WorkBitMap.WallType) then begin
                                             WorkBitMap.TileCount:=WALL_TILE_COUNT;
                                             for i:=0 to WALL_TILE_COUNT-1 do Include(WorkBitMap.TileNoSet,i);
                                             WorkBitMap.LastRect:=Rect(0,0,ColWidth,RowHeight);
                                             end
                                        else raise Exception.Create(TEXT_TASK_FAILED+': '+KEY_COMMON_SKIN_WALL+NL+NL+CheckSkinTypeText)
                       else with Pict.OrgBitMap do
                              if        Width=Height then
                                        if   MakeWallFromCommonSkinSource(Width div 4,Height div 4,Pict.OrgBitMap,WorkBitMap.BitMap,WorkBitMap.WallType) then begin
                                             WorkBitMap.TileCount:=WALL_TILE_COUNT;
                                             for i:=0 to WALL_TILE_COUNT-1 do Include(WorkBitMap.TileNoSet,i);
                                             WorkBitMap.LastRect:=Rect(0,0,Width div 4,Height div 4);
                                             end
                                        else raise Exception.Create(TEXT_TASK_FAILED+': '+KEY_COMMON_SKIN_WALL)
                              else if   Width*2=Height*3 then
                                        if   MakeWallFromCommonSkinSource(Width div 3,Height div 2,Pict.OrgBitMap,WorkBitMap.BitMap,WorkBitMap.WallType) then begin
                                             WorkBitMap.TileCount:=WALL_TILE_COUNT;
                                             for i:=0 to WALL_TILE_COUNT-1 do Include(WorkBitMap.TileNoSet,i);
                                             WorkBitMap.LastRect:=Rect(0,0,Width div 3,Height div 2);
                                             end
                                        else raise Exception.Create(TEXT_TASK_FAILED+': '+KEY_COMMON_SKIN_WALL)
                                   else raise Exception.Create(TEXT_TASK_FAILED+': '+KEY_COMMON_SKIN_WALL)

                       end;
                    end
                 else
                    if Result and
                       //OpenWorkBitMap(Variable__.PictureType,True) then
                       OpenWorkBitMap(Variable__.PictureType,
                                      (Variable__.PictureType<>ptPlayer) and (Variable__.PictureType<>ptPlayerOnGoal),
                                      ColWidth,RowHeight) then
                       if LoadPictureFromFile(Variable__.PictureType) then with WorkBitMap do begin
                          H:=Pict.OrgBitMap.Height; W:=Pict.OrgBitMap.Width;

                          if   (RectWidth(Value__.Rect)=0) or (RectWidth(Value__.Rect)=0) then
                               R0:=Rect(0,0,W,H)
                          else if   Value__.ValueType=valTile then with Value__.Rect do
                                    if   (ColWidth<>MaxInt) and (RowHeight<>MaxInt) then begin
                                         W:=W div Max(1,ColCount);
                                         H:=H div Max(1,RowCount);
                                         R0:=Rect(Pred(Left)*W+RealTileRect.Left,
                                         Pred(Top )*H+RealTileRect.Top,
                                         Right     *W+RealTileRect.Right,
                                         Bottom    *H+RealTileRect.Bottom);
                                         end
                                    else R0:=Rect(0,0,0,0)
                               else R0:=Value__.Rect;

                          W:=RectWidth(R0); H:=RectHeight(R0);
                          R1:=CellToRect(Variable__.TileNo,0,W,H);
                          if ((BitMap.Width>=R1.Right) and (BitMap.Height>=R1.Bottom)) or
                             BitMapResize(BitMap,R1.Right,R1.Bottom) then with BitMap do begin
                             Canvas.CopyMode:=cmSrcCopy;
                             if   Variable__.Quadrants=[] then
                                  Canvas.CopyRect(R1,Pict.OrgBitMap.Canvas,R0)
                             else for q1:=Low(q1) to High(q1) do
                                      if q1 in Variable__.Quadrants then begin
                                         if   (Variable__.PictureType=ptWall) and
                                              (Variable__.TileNo=BT_WALL_CAP) and
                                              WallTopQuadrantTransformation__
                                              then
                                              q2:=WALL_CAP_QUADRANT_TRANSFORMATION[q1]
                                         else q2:=q1;
                                         Canvas.CopyRect(RectQuadrant(R1,q2),Pict.OrgBitMap.Canvas,RectQuadrant(R0,q1));
                                         end;

                             if (Variable.PictureType=ptWall) and
                                (Variable__.TileNo=BT_WALL_NO_NEIGHBOR_WALLS) then
                                with Variable1 do begin // make a wall cap based on the isolated-wall tile
                                  Variable1:=Variable__;
                                  TileNo:=BT_WALL_CAP; Quadrants:=[];
                                  for q1:=Low(q1) to High(q1) do Include(Quadrants,q1);
                                  oWallType:=WorkBitMap.WallType;
                                  Result:=MakeWorkBitMapTile(Variable1,Value__,True);
                                  WorkBitMap.WallType:=oWallType;
                                  end;

                             TileInfo[Variable.PictureType].SizeRect:=Rect(0,0,W,H);
                             Inc(WorkBitMap.TileCount);
                             if Variable__.TileNo<=PLAYER_AND_BOX_FRAME_COUNT-1 then
                                Include(WorkBitMap.TileNoSet,Variable__.TileNo);
                             LastRect:=R1;

                             if WorkBitMap.TileCount>2 then
                                if      Variable__.TileNo=BT_WALL_CAP then
                                        WorkBitMap.WallType:=wtSeamlessWallWithCap
                                else if WorkBitMap.WallType=wtTiledWall then
                                        WorkBitMap.WallType:=wtSeamlessWallNoCap;

                             end;
                          end;
          end;

          function NextVariable:Boolean; // using compiler-terminology, 'TreeViewParentNode__' acts as a sort of environment
          var Value:TValue; VariableValue:PValue;

            function IsAPictureType(PictureType__:TPictureType):Boolean;
            var Index,Offset:Integer;
            begin // kludge: hard-wired number of items for each picture-type
              if   TreeViewParentNode__<>nil then begin
                   Index :=TreeViewParentNodeAbsoluteIndex__;
                   Offset:=PictureSettingsOffset[PictureType__];
                   if   Index>=Offset then
                        if   PictureType__<=ptReverseModeBackground then
                             Result:=Index<PictureSettingsOffset[Succ(PictureType__)]
                        else Result:=Index<Offset+Ord(stGraphicsBoardFiguresPlayerColor)-Ord(stGraphicsBoardFiguresPlayer)
                   else Result:=False;
                   end
              else Result:=False;
            end; // IsAPictureType

            function ParseWallTile(const Text__:String; var TileNo__:Integer; var Quadrants__:TQuadrantSet):Boolean;
            var   i,StopPosition:Integer; Direction:TDirection;

              function ParseQuadrants(const Text__:String; var Quadrants__:TQuadrantSet):Boolean;
              var   i,StopPosition:Integer; Direction1,Direction2:TDirection;
              begin // returns 'True' if parsing succeeds, even if no quadrants are found
                Result:=True; Quadrants__:=[];
                i:=AnsiPos(HYPHEN,Text__);
                if i<>0 then begin
                   StopPosition:=Length(Text__)-2;
                   while i<=StopPosition do
                     if   (Text__[i]=HYPHEN) and
                          CharToDirection(Text__[i+1],Direction1) and
                          CharToDirection(Text__[i+2],Direction2) then begin
                          case Direction1 of
                            Up   : if        Direction2=Left  then Include(Quadrants__,qTopLeft)
                                   else if   Direction2=Right then Include(Quadrants__,qTopRight)
                                        else Result:=False;
                            Left : if        Direction2=Up    then Include(Quadrants__,qTopLeft)
                                   else if   Direction2=Down  then Include(Quadrants__,qBottomLeft)
                                        else Result:=False;
                            Down : if        Direction2=Left  then Include(Quadrants__,qBottomLeft)
                                   else if   Direction2=Right then Include(Quadrants__,qBottomRight)
                                        else Result:=False;
                            Right: if        Direction2=Up    then Include(Quadrants__,qTopRight)
                                   else if   Direction2=Down  then Include(Quadrants__,qBottomRight)
                                        else Result:=False;
                            else   Result:=SokUtil_.Error(TEXT_RANGE_ERROR,'Skins_.Parse/../ParseQuadrants');
                          end; //case
                          Inc(i,3);
                          end
                     else Inc(i);
                   end;
                //Quadrants__:=[];
              end; // ParseQuadrants

            begin // ParseWallTile
              Result:=True; TileNo__:=0; Quadrants__:=[];
              i:=AnsiPos(HYPHEN,Text__);
              if i<>0 then begin
                 StopPosition:=Pred(Length(Text__));
                 while i<=StopPosition do
                   if   Text__[i]=HYPHEN then
                        if      CharToDirection(Text__[Succ(i)],Direction) and
                                ((i=StopPosition) or (Text__[i+2]=HYPHEN)) then begin
                                TileNo__:=TileNo__+DirectionToWallType(Direction) mod WALL_TILE_COUNT; // not fool-proof, but at least 'mod' ensures that it won't crash
                                Inc(i,2);
                                end
                        else if (Text__[Succ(i)]=KEY_WALL_CAP_CHAR_LOWERCASE) or
                                (Text__[Succ(i)]=KEY_WALL_CAP_CHAR_UPPERCASE) then begin
                                TileNo__:=BT_WALL_CAP;
                                i:=MaxInt;
                                end
                             else Inc(i)
                   else Inc(i);
                 end;
              Result:=Result and ParseQuadrants(Text__,Quadrants__);
            end; // ParseWallTile

            function  ParsePlayerAndBoxTile(const Text__:String; var TileNo__:Integer):Boolean;
            var   i,j:Integer; Direction:TDirection;
            begin  // returns 'True'; any errors will send the image to tile[0]
              TileNo__:=0; i:=Length(Text__); j:=1;
              while (i>=1) and IsADigitChar(Text__[i]) and (TileNo__<=PLAYER_AND_BOX_FRAME_COUNT) do begin
                Inc(TileNo__,j*(Ord(Text__[i])-Ord('0')));
                Dec(i); j:=j*10;
                end;
              if (TileNo__=0) and (i>1) and
                 CharToDirection(Text__[i],Direction) and
                 (AnsiPos(HYPHEN,Text__)=Pred(i)) then TileNo__:=Succ(Ord(Direction));
              if TileNo__<=PLAYER_AND_BOX_FRAME_COUNT then begin
                 Result:=True; Dec(TileNo__); // make 'TileNo__' 0-based
                 end
              else begin
                 Result:=False; TileNo__:=0;
                 end;
            end; // ParsePlayerAndBoxTile

            function SpacesBeforeTokenStartPosition:String;
            begin
              Result:='';
              while (Reader.StartPosition>1) and (Reader.Text[Pred(Reader.StartPosition)]<=SPACE) do begin
                         Dec(Reader.StartPosition);
                         Variable.Name:=Variable.Name+SPACE;
                         end;
            end;

          begin // 'NextVariable'
            Result:=False;
            if Reader.Token<>tokEqual then begin
               Variable.VariableType:=varNull;
               if Reader.Token<>tokLeftBrace then
                  Variable.Name:=Text(EQUAL)
               else begin
                  // evaluated name components are only supported for
                  // leading '{...}' components, not for interior components
                  // like in 'foo {bar} baz'
                  // to create names with interior evaluated components, use the
                  // 'add' function, e.g., '{add foo {bar} baz}';
                  Variable.Name:=''; Result:=True;
                  while (Reader.Token=tokLeftBrace) and Result do begin
                    if Variable.Name<>'' then // 'True': this isn't the first component of the name
                       while (Reader.StartPosition>1) and (Reader.Text[Pred(Reader.StartPosition)]<=SPACE) do begin // find spaces before the '{' character
                         Dec(Reader.StartPosition);
                         Variable.Name:=Variable.Name+SPACE; // keep the exact same number of spaces between the name components as there is in the original text
                         end;
                    Result:=Expression(Variable,Value,False);
                    if Result then
                       if        Value.ValueType=valText   then Variable.Name:=Variable.Name+Value.Text
                       else if   Value.ValueType=valNumber then Variable.Name:=Variable.Name+IntToStr(Value.Number)
                            else Result:=False;
                    end;

                  if Reader.Token=tokEqual then
                     Dec(Reader.Position) // undo the last '=' character so calling 'NextCharacter()' return '='; the caller uses that to detect that this is an assignment statement
                  else begin
                     while (Reader.StartPosition>1) and (Reader.Text[Pred(Reader.StartPosition)]<=SPACE) do begin
                       Dec(Reader.StartPosition);
                       Variable.Name:=Variable.Name+SPACE; // keep the exact same number of spaces between the name components as there is in the original text
                       end;
                     Variable.Name:=Variable.Name+Text(EQUAL); // 'Text()' calls 'Identifier()' which trims its result; that's why it doesn't duplicate spaces between the name components that 'Reader.StartPosition' has been decreased in one of the preceding code-lines
                     end;
                  Result:=False; // reset 'Result' before testing if the left-side name is a valid one
                  end;

               Variable.PictureType:=ptScreenBackground; Variable.Quadrants:=[];
               if Variable.Name<>'' then with Variable do begin
                  Node:=TreeNodeLookup(Name,TreeViewParentNode__);
                  if           Node<>nil then begin
                               VariableType:=varSettings; Result:=True;
                               AbsoluteIndex:=Node.AbsoluteIndex;
                               end
                  else if      IsAPictureType(ptWall) and
                               StrBeginsWith(Variable.Name,PICTURE_TYPE_NAME[ptWall]) then begin
                               VariableType:=varPicture; PictureType:=ptWall;
                               Result:=ParseWallTile(Name,TileNo,Quadrants);
                               end
                  else if      IsAPictureType(ptPlayer) and
                               StrBeginsWith(Variable.Name,PICTURE_TYPE_NAME[ptPlayer]) then begin
                               VariableType:=varPicture; PictureType:=ptPlayer;
                               Result:=ParsePlayerAndBoxTile(Name,TileNo);
                               end
                  else if      IsAPictureType(ptPlayerOnGoal) and
                               StrBeginsWith(Variable.Name,PICTURE_TYPE_NAME[ptPlayer]) then begin
                               VariableType:=varPicture; PictureType:=ptPlayerOnGoal;
                               Result:=ParsePlayerAndBoxTile(Name,TileNo);
                               end
                  else if      IsAPictureType(ptPlayerAnimation) and
                               StrBeginsWith(Variable.Name,PICTURE_TYPE_NAME[ptPlayer]) then begin
                               VariableType:=varPicture; PictureType:=ptPlayerAnimation;
                               Result:=ParsePlayerAndBoxTile(Name,TileNo);
                               end
                  else if      IsAPictureType(ptPlayerOnGoalAnimation) and
                               StrBeginsWith(Variable.Name,PICTURE_TYPE_NAME[ptPlayer]) then begin
                               VariableType:=varPicture; PictureType:=ptPlayerOnGoalAnimation;
                               Result:=ParsePlayerAndBoxTile(Name,TileNo);
                               end
                  else if      IsAPictureType(ptBoxAnimation) and
                               StrBeginsWith(Variable.Name,PICTURE_TYPE_NAME[ptBox]) then begin
                               VariableType:=varPicture; PictureType:=ptBoxAnimation;
                               Result:=ParsePlayerAndBoxTile(Name,TileNo);
                               end
                  else if      IsAPictureType(ptBoxOnGoalAnimation) and
                               StrBeginsWith(Variable.Name,PICTURE_TYPE_NAME[ptBox]) then begin
                               VariableType:=varPicture; PictureType:=ptBoxOnGoalAnimation;
                               Result:=ParsePlayerAndBoxTile(Name,TileNo);
                               end
                  else if      AnsiCompareText(Variable.Name,KEY_COL_WIDTH)=0 then begin
                               VariableType:=varColWidth; Result:=True;
                               end
                  else if      AnsiCompareText(Variable.Name,KEY_ROW_HEIGHT)=0 then begin
                               VariableType:=varRowHeight; Result:=True;
                               end
                  else if      AnsiCompareText(Variable.Name,KEY_COLUMNS)=0 then begin
                               VariableType:=varColCount; Result:=True;
                               end
                  else if      AnsiCompareText(Variable.Name,KEY_ROWS)=0 then begin
                               VariableType:=varRowCount; Result:=True;
                               end
                  else if      AnsiCompareText(Variable.Name,KEY_COLOR_THEME)=0 then begin
                               VariableType:=varColorTheme; Result:=True;
                               end
                  else if      LookupVariable(Variable.Name,VariableValue) then begin
                               VariableType:=varVariable; Result:=True;
                               end;
                  end;
               end;
          end; // NextVariable

        begin // Assignment
          Result:=(TreeViewParentNode__=nil) or // 'Node'=nil: no valid section for assignments
                  (Reader.Token=tokEqual);
          if not Result then // 'not Result': not an empty statement; look for an assigmnent
             if NextVariable and
                (NextCharacter=EQUAL) then begin
                //Reader.StartPosition:=Reader.Position; // set start-position to the current position, which is after "="
                //Value.Text:=Text(''); // get the text after the '=' character
                Value.Text:=Copy(Reader.Text,Reader.Position,MaxInt); // get the text after the '=' character
                Value.ValueType:=valText;

                Result:=Expression(Variable,Value,True);

                   if Result then
                      case Variable.VariableType of
                        varNull    : Result:=False;
                        varSettings:
                          case Value.ValueType of
                            valNull: Result:=False;
                            valText: begin
                                       if IsAFileNameSetting(Variable) then begin

                                          if IsASettingsFile__ and StrEqual(Value.Text,DEFAULT_VALUE) then begin
                                             LastFileName__:=Value.Text;
                                             if OptionsForm.IsAPictureSetting(Variable.AbsoluteIndex,Variable.PictureType) then
                                                TileInfo[Variable.PictureType].HasName:=True;
                                             end
                                          else begin
                                             if   (Value.Text<>'') and
                                                  (AnsiPos(COLON{FILE_NAME_PATH_DELIMITER},Value.Text)=0) then // don't test for 'FILE_NAME_PATH_DELIMITER', the filename may be relative like '..\..\filename'
                                                  Value.Text:=ExpandFileName(StrWithTrailingPathDelimiter(ExtractFilePath(OpenForm.Texts[tTextFile].FileName))+Value.Text);
                                             if   (Value.Text<>'') and FileExists(Value.Text) then begin
                                                  LastFileName__:=Value.Text;
                                                  if OptionsForm.IsAPictureSetting(Variable.AbsoluteIndex,Variable.PictureType) then begin
                                                     TileInfo[Variable.PictureType].HasName:=True;
                                                     end;
                                                  end
                                             else begin LastFileName__:='';
                                                        if Value.Text<>'' then
                                                           raise Exception.Create(Format(FileNotFoundText__,[Value.Text]));
                                                        if   (Value.Text='') and IsASettingsFile__ then
                                                             Value.Text:=SPACE // update the settings, even though the filename is empty (the SPACE will be trimmed again by 'OptionsForm.UpdateSettings')
                                                        else Value.Text:='';
                                                  end;
                                             end;
                                          end;


                                       if Value.Text<>'' then begin
                                          //if AnsiCompareText(Value.Text,OptionsForm.SettingsString[TSettings(Variable.AbsoluteIndex)])<>0 then begin
                                               //OptionsForm.SettingsString[TSettings(AbsoluteIndex)]:=Value
                                               Result:=OptionsForm.UpdateSettings(-Variable.AbsoluteIndex,Value.Text,Self);
                                               if Result then begin
                                                  if Variable.AbsoluteIndex=Ord(stGraphicsBoardFiguresWallType) then
                                                     HasWallType:=True;

                                                  if not IsASettingsFile__ then begin
                                                     if IsAFileNameSetting(Variable) and
                                                        (Variable.AbsoluteIndex+5<OptionsForm.TreeView1.Items.Count) and
                                                        StrEqual(OptionsForm.TreeView1.Items[Succ(Variable.AbsoluteIndex)].Text,ImageSectionText) then begin
                                                        // clear 'Section' when 'FileName' is updated
                                                        OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+2),IntToStr(0),Self);
                                                        OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+3),IntToStr(0),Self);
                                                        OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+4),IntToStr(0),Self);
                                                        OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+5),IntToStr(0),Self);

                                                        // if columns or rows are negative values, or columns = 0, then this picture is supposed to be a combined file
                                                        if ((ColCount<=0)
                                                            or
                                                            (RowCount<0)
                                                            or
                                                            VariableColCount
                                                            or
                                                            VariableRowCount
                                                           )
                                                           and
                                                           (Value.Text=LastFileName__)
                                                           and
                                                           (Variable.PictureType>=ptPlayer)
                                                           and
                                                           (Variable.PictureType<=ptBoxOnGoalAnimation) then begin
                                                           if LoadPictureFromFile(Variable.PictureType) then with Pict do
                                                              if (OrgBitMap.Width<>0) and (OrgBitMap.Height<>0) then begin
                                                                 if (ColWidth >0) and (ColCount<=0) then
                                                                    ColCount:=OrgBitMap.Width  div ColWidth;
                                                                 if (RowHeight>0) and (RowCount<=0) then
                                                                    RowCount:=OrgBitMap.Height div RowHeight;

                                                                 if (VariableColCount or VariableRowCount)
                                                                    and
                                                                    GuessColumnsAndRows(
                                                                      OrgBitMap,
                                                                      OrgBitMap.Width,OrgBitMap.Height,
                                                                      ColCount,RowCount,
                                                                      VariableColCount,VariableRowCount) then begin
                                                                    BitMapWidth:=OrgBitMap.Width;
                                                                    BitMapHeight:=OrgBitMap.Height;
                                                                    ColWidth :=BitMapWidth  div Max(1,ColCount);
                                                                    RowHeight:=BitMapHeight div Max(1,RowCount);
                                                                    end
                                                                 else begin
                                                                    {maybe this is obsolete and better handled by}
                                                                    {'GuessColumnsAndRows'}
                                                                    { but better safe than sorry}
                                                                    if RowHeight=MaxInt then // 'True': calculate row information now
                                                                       RowCount:=-Max(1,Abs(RowCount));
                                                                    if ColCount=0 then
                                                                       ColCount:=-OrgBitMap.Width div (OrgBitMap.Height div Max(1,Abs(RowCount)));

                                                                    if ColCount<0 then begin
                                                                       ColCount:=Abs(ColCount);
                                                                       ColWidth:=OrgBitMap.Width div ColCount;
                                                                       BitMapWidth:=OrgBitMap.Width;
                                                                       end;
                                                                    if RowCount<0 then begin
                                                                       RowCount:=Abs(RowCount);
                                                                       RowHeight:=OrgBitMap.Height div RowCount;
                                                                       BitMapHeight:=OrgBitMap.Height;
                                                                       end;
                                                                    end;
                                                                 end;
                                                           end
                                                        else if VariableColCount or VariableRowCount then

                                                        end;
                                                     if Variable.AbsoluteIndex=Ord(stGraphicsBoardBackgroundColor) then begin
                                                        // when the script updates the color it probably also wants to show the background
                                                        OptionsForm.UpdateSettings(-Ord(stGraphicsBoardBackgroundVisible),BooleanText[True],Self);
                                                        OptionsForm.UpdateSettings(-Ord(stGraphicsBoardBackgroundImageFileName),'',Self);
                                                        end;

                                                     if Variable.AbsoluteIndex=Ord(stGraphicsBoardFiguresFloorImageFileName) then
                                                        OptionsForm.UpdateSettings(-Ord(stGraphicsBoardFiguresFloorVisible),BooleanText[True],Self);

                                                     if (SettingsEditor[TSettings(Variable.AbsoluteIndex)]=seColor) and
                                                        OptionsForm.IsAPictureSetting(Variable.AbsoluteIndex,Variable.PictureType) then
                                                        TileInfo[Variable.PictureType].HasColor:=True;
                                                     end;
                                                  end;
                                          //end;
                                          end;
                                     end;
                            valRect: begin
                                       if   StrEqual(Variable.Node.Text,ImageSectionText) and
                                            (Variable.AbsoluteIndex+4<OptionsForm.TreeView1.Items.Count) then with Value.Rect do begin
                                            OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+1),IntToStr(Left  ),Self);
                                            OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+2),IntToStr(Top   ),Self);
                                            OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+3),IntToStr(Right-Left),Self); // note: 'width',  not 'right'
                                            OptionsForm.UpdateSettings(-(Variable.AbsoluteIndex+4),IntToStr(Bottom-Top),Self); // note: 'height', not 'bottom'
                                            end
                                       else Error(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                                     end;
                            else     Error(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                          end; // case
                        varPicture:
                          case Value.ValueType of
                            valNull: Result:=False;
                            valText: Result:=Error(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                            valRect,
                            valTile: begin
                                       if LastFileName__<>'' then
                                          MakeWorkBitMapTile(Variable,Value,False);
                                     end;
                            else     Result:=Error(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                          end; // case
                        varColWidth,
                        varRowHeight,
                        varColCount,
                        varRowCount:
                          case Value.ValueType of
                            valNull: Result:=False;
                            valNumber,
                            valText: begin
                                       if Value.ValueType=valText then begin
                                          StartReading(Value.Text);
                                          Result:=Reader.Token=tokNumber;
                                          if Result then Value.Number:=Reader.Number;
                                          end;

                                       if   Result and (Value.Number>=0) then
                                            case Variable.VariableType of
                                              varColWidth : begin
                                                              ColWidth:=Value.Number;
                                                              if BitMapWidth=0 then BitMapWidth:=High(BitMapWidth); // assume the user wants to define RECT's without any range-checking
                                                            end;
                                              varRowHeight: begin
                                                              RowHeight:=Value.Number;
                                                              if BitMapHeight=0 then BitMapHeight:=High(BitMapHeight); // assume the user wants to define RECT's without any range-checking
                                                            end;
                                              varColCount : begin
                                                              ColWidth :=0; BitMapWidth :=0; ColCount:=-Max(1,Value.Number); // a negated value triggers calculations next time an image is loaded
                                                              //Result:=Error(Format('"%s =" : '+TEXT_NOT_IMPLEMENTED,[KEY_COLUMNS]));
                                                            end;
                                              varRowCount : begin
                                                              RowHeight:=0; BitMapHeight:=0; RowCount:=-Max(1,Value.Number); // a negated value triggers calculations next time an image is loaded
                                                              //Result:=Error(Format('"%s =" : '+TEXT_NOT_IMPLEMENTED,[KEY_ROWS]));
                                                            end;
                                            end // case
                                       else Result:=Error(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokNumber],TextLines[CurrentLineNo]]));
                                     end;
                            else     Result:=Error(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                          end; // case
                        varColorTheme: begin
                          Result := ( Value.ValueType = valText ) and ( Value.Text <> '' ) and Assigned( OptionsForm );
                          if Result then begin
                             Result := False;
                             for ColorTheme := Low( ColorTheme ) to High( ColorTheme ) do
                                 if StrEqual( Value.Text, ColorThemeText[ ColorTheme ] ) then begin
                                    OptionsForm.SetColorTheme( ColorTheme );
                                    Result := True;
                                    end;
                             end;
                          if not Result then
                             //Error(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                             raise Exception.Create(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                          end;
                        varVariable: with Variables[UpCase(Variable.Name[1])] do begin
                                       ValueType:=Value.ValueType;
                                       case ValueType of
                                         valNull     :;
                                         valText     : Text  :=Value.Text;
                                         valRect     : Rect  :=Value.Rect;
                                         valTile     : Rect  :=Value.Rect;
                                         valNumber   : Number:=Value.Number;
                                       end; // case
                                       LastKey:=''; // clear cached key, if any
                                       end;
                        else         Result:=Error(Format(UnknownItemText__,[TextLines[CurrentLineNo]]));
                   end;
                end
             else Result:=True; // nothing happened; continue to next statement
        end; // Assignment

        function IfStatement:Boolean;
        var oPosition:Integer; Condition:String; Variable:TVariable; Value:TValue;

          function SkipIfStatement(RecursionLevel__:Integer):Boolean;
          begin
            if   RecursionLevel__<MAX_RECURSION_LEVEL then begin
                 Result:=True;
                 while Result and
                       (CurrentLineNo<Pred(TextLines.Count)) and
                       (Reader.Token<>tokEndIf) do begin
                       Inc(CurrentLineNo);
                       StartReading(TextLines[CurrentLineNo]);
                       if Reader.Token=tokIf then
                          Result:=SkipIfStatement(Succ(RecursionLevel__));
                       end;
                 Result:=Result and Match(tokEndif);
                 end
            else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokIf],TextLines[CurrentLineNo]]));
          end; // SkipIfStatement

        begin // IfStatement
          Result:=Match(tokIf);
          if Result then begin
             Variable.VariableType:=varNull;
             Value.ValueType:=valText; Value.Text:=Text(EQUAL); // prepare text to be parsed by 'Expression()'
             oPosition:=Reader.Position;

             if Expression(Variable,Value,True) then begin
                if   Value.ValueType=valNumber then
                     Condition:=IntToStr(Value.Number)
                else Condition:=Value.Text;
                end
             else Condition:='';

             StartReading(Copy(TextLines[CurrentLineNo],oPosition,MaxInt));
             if   Skip(tokEqual) then begin
                  Skip(tokEqual); {allow C-style equal-token "=="}
                  Value.Text:=Text('');
                  Result:=Expression(Variable,Value,True);
                  if Result then begin
                     if Value.ValueType=valNumber then Value.Text:=IntToStr(Value.Number);
                     if      StrEqual(Condition,Value.Text) then
                             if   Condition='' then
                                  Condition:=TOKEN_TEXTS[tokIf]
                             else //
                     else    Condition:='';
                     end
                  else
                     raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokIf],TextLines[CurrentLineNo]]));
                  end
             else if StrEqual(Condition,BooleanText[False])
                     or
                     (Condition='0') // treating '0' as 'False' is problematic, but changing it could break existing scripts
                     then
                     Condition:='';

             while Result and
                (CurrentLineNo<Pred(TextLines.Count)) and
                (Reader.Token<>tokEndIf) do begin
                Inc(CurrentLineNo);
                StartReading(TextLines[CurrentLineNo]);

                if        Reader.Token=tokIf then begin
                          if   Condition<>'' then begin
                               Statement(Succ(RecursionLevel__));
                               end
                          else Result:=SkipIfStatement(Succ(RecursionLevel__));
                          end
                else if   Reader.Token=tokElse then begin
                          if   Condition='' then
                               Condition:=TOKEN_TEXTS[tokElse] // anything non-blank will do
                          else Condition:='';
                          end
                else if   Reader.Token=tokEndif then begin
                          end
                else if   Condition<>'' then
                          Statement(Succ(RecursionLevel__));
                end;

             Result:=Result and Match(tokEndIf);
             end;
        end; // IfStatement

        function SectionStatement(var TreeViewParentNode__:TTreeNode; var TreeViewParentNodeAbsoluteIndex__:Integer):Boolean;
        var SubSectionName:String;

          function SubSection(var SubSectionName__:String):Boolean;
          begin
            SubSectionName__:=Text('-]');
            Result:=SubSectionName__<>'';
          end;

        begin // SectionStatement
          Result:=Match(tokLeftBracket);
          TreeViewParentNode__:=nil; TreeViewParentNodeAbsoluteIndex__:=0;
          while Result and
                (Reader.Token<>tokNull) and
                (Reader.Token<>tokRightBracket) and
                SubSection(SubSectionName) do begin
                TreeViewParentNode__:=TreeNodeLookUp(SubSectionName,TreeViewParentNode__);
                if        TreeViewParentNode__<>nil then begin
                          TreeViewParentNodeAbsoluteIndex__:=TreeViewParentNode__.AbsoluteIndex;
                          NextToken;
                          if Reader.Token=tokHyphen then NextToken;
                          end
                else if   (TreeViewParentNodeAbsoluteIndex__=Ord(stGraphicsBoardFiguresWall)) and
                          StrEqual(SubSectionName,'Outer wall razor, pixels') then begin
                          // Version 1.461 changed the option name 'Outer wall
                          // razor, pixels' to 'Outer wall trimming, pixels'.
                          // Embedded text settings in any old skins on the
                          // user's computer aren't updated during installation
                          // of new program versions, so the skin loader
                          // provides backward compatibility for this option.
                          TreeViewParentNode__:=OptionsForm.TreeView1.Items[Ord(stGraphicsBoardFiguresWallOuterWallTrimming)];
                          TreeViewParentNodeAbsoluteIndex__:=TreeViewParentNode__.AbsoluteIndex;
                          NextToken;
                          if Reader.Token=tokHyphen then NextToken;
                          end
                     else Reader.Token:=tokNull; // silently accept non-existent sections; they're probably extra sections for keyword-mapping etc.
                end;
          Result:=True; // 'True': continue to next statement
        end; // SectionStatement

        function PerformStatement:Boolean;
        var NewLineNo,ThisLineNo,FirstIndex,LastIndex,StepCount,oPosition:Integer;
            DoIt:Boolean;
            Condition,UntilExpression:String;
            Variable:TVariable; Value:TValue; VariableValue:PValue;
        begin
          Result:=Match(tokPerform);
          if Result then begin
             Variable.VariableType:=varNull;

             if Reader.Token=tokIdentifier then begin
                Value.ValueType:=valText;
                Value.Text:= {Reader.Lexeme;} Text(SPACE); // use 'Text(SPACE) to get all consecutive non-blank characters; 'Reader.Lexeme' won't do because section names with digits are split by the lexical analyzer 'NextToken'
                NextToken;
                end
             else begin
                Value.Text:=Text('');
                Result:=Expression(Variable,Value,True);
                end;

             if Result then
                if Value.ValueType=valText then begin
                   ThisLineNo:=CurrentLineNo;
                   if      Value.Text='' then
                           NewLineNo:=0
                   else if (LastPerformLineNo>0) and
                           StrEqual(Value.Text,LastPerformName) then
                           NewLineNo:=LastPerformLineNo
                        else begin
                           LastPerformName:=Value.Text;
                           Value.Text:=StrWithBrackets(Value.Text);
                           NewLineNo:=Succ(StrIndexOfCI(Value.Text,TextLines));
                           // note that this finds the first occurrence of the
                           // sub-routine name;
                           // for speed, the primitive skin scripting-language
                           // doesn't handle 'if'-statement scoping of labels correctly;
                           // therefore, never use the same sub-routine name more than once;
                           LastPerformLineNo:=NewLineNo;
                           end;

                   if NewLineNo>0 then // 'True': the section exists
                      if (Reader.Token=tokIdentifier) and StrEqual(Reader.Lexeme,KEY_VARYING) then begin
                         VariableValue:=nil; FirstIndex:=0; LastIndex:=0; StepCount:=0; UntilExpression:='';
                         NextToken;
                         Result:=Expression(Variable,Value,False) and (Value.ValueType=valText) and LookupVariable(Value.Text,VariableValue);
                         if (Reader.Token=tokIdentifier) and StrEqual(Reader.Lexeme,KEY_FROM) then begin
                            NextToken;
                            Result:=Result and
                                    Expression(Variable,Value,False) and
                                    ((Value.ValueType=valNumber)
                                     or
                                     ((Value.ValueType=valText)
                                      and
                                      SafeStrToInt(Value.Text,False,Value.Number)
                                     ));
                            FirstIndex:=Value.Number;
                            LastIndex:=FirstIndex;
                            end;
                         if (Reader.Token=tokIdentifier) and StrEqual(Reader.Lexeme,KEY_TO) then begin
                            NextToken;
                            Result:=Result and
                                    Expression(Variable,Value,False) and
                                    ((Value.ValueType=valNumber)
                                     or
                                     ((Value.ValueType=valText)
                                      and
                                      SafeStrToInt(Value.Text,False,Value.Number)
                                     ));
                            LastIndex:=Value.Number;
                            end;
                         if (Reader.Token=tokIdentifier) and StrEqual(Reader.Lexeme,KEY_BY) then begin
                            NextToken;
                            Result:=Result and
                                    Expression(Variable,Value,False) and
                                    ((Value.ValueType=valNumber)
                                     or
                                     ((Value.ValueType=valText)
                                      and
                                      SafeStrToInt(Value.Text,False,Value.Number)
                                     ));
                            StepCount:=Value.Number;
                            end
                         else if      FirstIndex<LastIndex then StepCount:= 1
                              else if FirstIndex>LastIndex then StepCount:=-1;
                         if (Reader.Token=tokIdentifier) and StrEqual(Reader.Lexeme,KEY_UNTIL) then
                            UntilExpression:=Trim(Copy(Reader.Text,Reader.Position,MaxInt));

                         Result:=Result
                                 and
                                 ((FirstIndex=LastIndex)
                                  or
                                  ((FirstIndex<LastIndex) and (StepCount>0))
                                  or
                                  ((FirstIndex>LastIndex) and (StepCount<0))
                                 )
                                 and
                                 ((StepCount<>0) or (UntilExpression<>''));

                         if   Result then begin
                              VariableValue^.ValueType:=valNumber;
                              VariableValue^.Number:=FirstIndex;
                              DoIt:=True;

                              repeat
                                if VariableValue^.ValueType=valNumber then begin
                                   FirstIndex:=VariableValue^.Number;
                                   if      StepCount>0 then DoIt:=FirstIndex<=LastIndex
                                   else if StepCount<0 then DoIt:=FirstIndex>=LastIndex;
                                   if DoIt then begin
                                      if UntilExpression<>'' then begin
                                         StartReading(UntilExpression);
                                         Value.ValueType:=valText; Value.Text:=Text(EQUAL); // prepare text to be parsed by 'Expression()'
                                         oPosition:=Reader.Position;
                                         DoIt:=Expression(Variable,Value,True);
                                         if DoIt then begin
                                            if   Value.ValueType=valNumber then
                                                 Condition:=IntToStr(Value.Number)
                                            else Condition:=Value.Text;
                                            StartReading(Copy(UntilExpression,oPosition,MaxInt));
                                            if   Skip(tokEqual) then begin // only '=' comparison is implemented
                                                 Skip(tokEqual); {allow C-style equal-token "=="}
                                                 Value.Text:=Text('');
                                                 DoIt:=Expression(Variable,Value,True);
                                                 if DoIt then begin
                                                    if Value.ValueType=valNumber then Value.Text:=IntToStr(Value.Number);
                                                    DoIt:=not StrEqual(Condition,Value.Text);
                                                    end;
                                                 end
                                            else DoIt:=Condition=''; // a non-blank value is interpreted as 'True'
                                            end;
                                         end;
                                       if DoIt then begin
                                          Result:=Statements(TreeViewParentNode__,TreeViewParentNodeAbsoluteIndex__,Succ(RecursionLevel__),NewLineNo,LastFileName__);
                                          if VariableValue^.ValueType=valNumber then
                                             Inc(VariableValue^.Number,StepCount);
                                          end;
                                      end;
                                   end
                                else
                                   raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[NumberExpectedText,TextLines[CurrentLineNo]]));
                              until not DoIt;
                              end
                         else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokPerform],TextLines[CurrentLineNo]]));
                         end
                      else
                         // parse the section
                         Result:=Statements(TreeViewParentNode__,TreeViewParentNodeAbsoluteIndex__,Succ(RecursionLevel__),NewLineNo,LastFileName__);

                   CurrentLineNo:=ThisLineNo; Reader.Token:=tokNull;
                   end
                else
                   raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokPerform],TextLines[CurrentLineNo]]));
             end
          else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokPerform],TextLines[CurrentLineNo]]));
        end; // PerformStatement

        function GetImageTextStatement:Boolean;
        var Count:Integer; s,s1:String; R:TRect; B:TBitMap;
            Variable:TVariable; Value:TValue;
        begin
          Result:=Match(tokGetImageText);
          if Result then with Reader do begin
             // parse rectangle
             Variable.VariableType:=varNull;
             Count:=0; FillChar(R,SizeOf(R),0);
             while Result and (Count<4) and (Token<>tokNull) do begin
               Value.Text:=Copy(Text,StartPosition,MaxInt); Value.ValueType:=valNull;
               Result:=Expression(Variable,Value,True);
               if Result then with Value do
                  if      (ValueType=valRect) and (Count=0) then begin
                          Count:=4; R:=Rect;
                          end
                  else if ValueType=valNumber then begin
                          Inc(Count);
                          case Count of
                             1: R.Left  :=Number;
                             2: R.Top   :=Number;
                             3: R.Right :=Number;
                             4: R.Bottom:=Number;
                          end; // case
                          NextToken;
                          end
                       else begin
                          //raise Exception.Create(Format(TokenExpectedText__,[TOKEN_TEXTS[tokNumber],TextLines[CurrentLineNo]]))
                          NextToken;
                          end;
               end;
             Result:=Result and (Count=4);

             // parse optional header line to add before the embedded image text;
             // typically, a header line is added to ensure that a marker like "[<file name>]"
             // is present before the embedded image text lines, so the script later can
             // issue a "perform <file name>" command to parse the embedded text lines;
             s:='';
             while Result and (Token<>tokNull) do begin
               if Token=tokString then begin
                  s:=s+Lexeme; NextToken;
                  end
               else if Token=tokLeftBrace then begin
                       Value.Text:=Copy(Text,StartPosition,MaxInt); Value.ValueType:=valNull;
                       Result:=Expression(Variable,Value,True);
                       if   Result then with Value do begin
                            if ValueType=valText then s:=s+Text;
                            end
                       else NextToken;
                       end
                    else NextToken;
               end;

             if Result then begin
                if (OpenForm.CurrentImageFileName<>'') and
                   (OpenForm.Image1.Picture.BitMap<>nil) and
                   (OpenForm.Texts[tTextFile].FileName='') then
                   B:=OpenForm.Image1.Picture.BitMap
                else begin
                   //'GetImageText' isn't implemented for dynamically loaded images, only for image-based skins
                   raise Exception.Create(TOKEN_TEXTS[tokGetImageText]+': '+TEXT_NOT_IMPLEMENTED+NL+NL+TextLines[CurrentLineNo]);
                   end;
                if B<>nil then begin
                   //TextLines.BeginUpdate;
                   try     Count:=TextLines.Count;
                           if GetBitMapText(B,R,False,TextLines,s1) then begin
                              if (s<>'') and (s[1]=LEFT_BRACKET) and (s[System.Length(s)]=RIGHT_BRACKET) then begin
                                 i:=Count; // check whether the header line "[ ... ]" already is present in the embedded text lines
                                 while i<TextLines.Count do begin
                                   s1:=TextLines[i];
                                   if (s1='') or (s[1]<>LEFT_BRACKET) or (not StrEqual(s,s1)) then
                                      Inc(i)
                                   else begin
                                     i:=TextLines.Count; s:='';
                                     end;
                                   end;
                                 end;
                              if s<>'' then // 'True': the header line does not exist in the embedded image text lines
                                 TextLines.Insert(Count,s);
                              end;
                   finally //TextLines.EndUpdate;
                   end;
                   end;
                end
             else begin
                //raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokGetImageText],TextLines[CurrentLineNo]]));
                end;
             Result:=True; // silently drop any errors in the statement
             end;
        end; // GetImageTextStatement

      begin // Statement
        case Reader.Token of
          tokEnd           : Result:=True;
          tokIf            : Result:=IfStatement;
          tokLeftBracket   : Result:=SectionStatement(TreeViewParentNode__,TreeViewParentNodeAbsoluteIndex__);
          tokGetImageText  : Result:=GetImageTextStatement;
          tokPerform       : Result:=PerformStatement;
          else               Result:=Assignment;
        end; // case
      end;

    begin // Statements
      Result:=RecursionLevel__<MAX_RECURSION_LEVEL;
      if   Result then begin
           InitializeIniFileReader(tSkinScript);
           Reader.Token:=tokNull;
           LastPerformName:=''; LastPerformLineNo:=-1;
           CurrentLineNo:=StartLineNo__;
           while Result and
              (CurrentLineNo<TextLines.Count) and
              (Reader.Token<>tokEnd) do begin
              StartReading(TextLines[CurrentLineNo]);
              Result:=Statement(Succ(RecursionLevel__));
              Inc(CurrentLineNo);
              end;
           end
      else raise Exception.Create(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[TOKEN_TEXTS[tokPerform],TextLines[CurrentLineNo]]));

      if Result and (RecursionLevel__=0) then CloseWorkBitMap;
    end; // Statements

    function RealTile(const Text__:String; var RealTileRect__:TRect):Boolean;
    var i:Integer; oSignedNumbers:Boolean;
    begin
      oSignedNumbers:=Reader.SignedNumbers; Reader.SignedNumbers:=True;
      StartReading(Text__); i:=0;
      while SkipUntil(tokNumber) do with RealTileRect__ do begin
        Inc(i);
        case i of
          1: Left  :=Reader.Number;
          2: Top   :=Reader.Number;
          3: Right :=Reader.Number;
          4: Bottom:=Reader.Number;
        end; // case
        NextToken;
        end;
      Result:=i=4; Reader.SignedNumbers:=oSignedNumbers;
    end; // RealTile

    procedure Finalize;
    var i:Integer; PictureType:TPictureType; Rect:TRect;
    begin
      if not IsASettingsFile__ then begin
         FillChar(Rect,SizeOf(Rect),0);
         for PictureType:=ptPlayer to High(PictureType) do with TileInfo[PictureType] do
             if (SizeRect.Right>Rect.Right) and (SizeRect.Bottom>Rect.Bottom) then
                Rect:=SizeRect;
         for PictureType:=ptPlayer to High(PictureType) do with TileInfo[PictureType] do
             if HasColor and (not HasName) and
                (Rect.Right<>0) and (Rect.Bottom<>0) and
                SafeStrToInt(RGB_BGR(OptionsForm.SettingsString[TSettings(PictureSettingsColorIndex[PictureType])]),True,i) and
                CloseWorkBitMap and
                OpenWorkBitMap(PictureType,False,0,0) and
                BitMapResize(WorkBitMap.BitMap,Rect.Right,Rect.Bottom) then
                with WorkBitMap.BitMap do with Canvas do begin
                  Brush.Style:=bsSolid; Brush.Color:=TColor(i); FillRect(Rect);
                  if CloseWorkBitMap then
                     if PictureType=ptFloor then
                        OptionsForm.UpdateSettings(-Ord(stGraphicsBoardFiguresFloorVisible),BooleanText[True],Self);
                  end;
         end;
    end; // Finalize

    function Parse(StartLineNo__:Integer):Boolean;
    // precondition: 'TextFile__' and 'TextLines' have been initialized before
    // calling this function;

    // kludge: this is a lately introduced wrapper-function helping to support
    // dynamically adding/deleting lines to the script-file;
    // properly implemented, it would have 'enveloped' the parser's kernel
    // functions listed above, but as it is, the function doesn't take any
    // arguments except the starting line number, and the caller must have
    // initialized 'TextFile__' and 'TextLines' beforehand;

    var OldLineCount:Integer; OldModified:Boolean; s:String;
    begin // Parse.Parse.Parse
      with OpenForm.Texts[TextType__].Memo do
        if TextLines=Lines then begin
           if WordWrap then WordWrap:=False; // otherwise added new lines may be broken unintentionally despite the fact that the Delphi documentation says that 'WordWrap' is cosmetical only
           OldLineCount:=Lines.Count; OldModified:=Modified;
           Lines.BeginUpdate;
           try     try     s:=''; Result:=Statements(nil,0,0,StartLineNo__,s);
                   finally while Lines.Count>OldLineCount do Lines.Delete(Pred(Lines.Count));
                   end;
           finally Lines.EndUpdate;
                   Modified:=OldModified;
           end
           end
        else
           // the caller didn't understand the conditions for calling this wrapper-function
           raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['TSkins.Parse.Parse.Parse']));
    end; // Parse.Parse.Parse

  begin // Parse.Parse
    Result:=True; InitializeReader; ClearErrors;
    ColCount:=1; RowCount:=0; ColWidth:=MaxInt; RowHeight:=MaxInt; TileBase:=1;
    VariableColCount:=False; VariableRowCount:=False;
    BitMapWidth:=0; BitMapHeight:=0;
    RealTileRect:=Rect(0,0,0,0); Separator:=EQUAL; EndOfLineCommentSeparator:='';
    FindToplevelTreeNodes(TreeViewRootNode);
    FileNameSignature:=Self.FileNameSignature(FileName__);
    InitializeIniFileReader(TextType__);
    HasWallType:=False; SkinPixelCodes.HasPixelCodes:=False; s:='';
    TextLines:=OpenForm.Texts[TextType__].Memo.Lines;
    if OptionsForm.TreeView1.Items.Count<Succ(Ord(High(TSettings))-Ord(Low(TSettings))) then
       Result:=SokUtil_.Error(TEXT_TASK_FAILED,Caption);
    if Result and
       (TextType__=tSkinScript) and
       (ReadString(TextLines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_SKIN_TYPE,'')='') then
       Result:=SokUtil_.Error(Format(FileNotALegalSkinScriptText__,[ScriptFileName]),Caption);
    if Result then begin
       s:=ReadString(TextLines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_COLUMNS,'');
       if s<>'' then begin
          i:=0;
          if   ReadUnsignedInteger(s,i,ColCount) and (ColCount>0) then
               VariableColCount:=(i<Length(s)) and (s[STRING_BASE+i]=PERIOD)
          else Result:=SokUtil_.Error(Format(TokenExpectedText__,[KEY_COLUMNS,s]),Caption);
          end;
       end;
    if Result then begin
       s:=ReadString(TextLines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_ROWS,'');
       if s<>'' then begin
          i:=0;
          if   ReadUnsignedInteger(s,i,RowCount) and (RowCount>0) then
               VariableRowCount:=(i<Length(s)) and (s[STRING_BASE+i]=PERIOD)
          else Result:=SokUtil_.Error(Format(TokenExpectedText__,[KEY_ROWS,s]),Caption);
          end;
       end;
    if Result then begin
       s:=ReadString(TextLines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_REAL_TILE,'');
       if (s<>'') and (not RealTile(s,RealTileRect)) then
          Result:=SokUtil_.Error(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_REAL_TILE,s]),Caption);
       end;
    if Result then begin
       s:=StrWithoutDoubleQuotes(ReadString(TextLines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_SEPARATOR,Separator));
       if   Length(s)=1 then Separator:=s[1]
       else Result:=SokUtil_.Error(Format(SyntaxErrorText__+NL+NL+PleaseChangeItAndTryAgainText,[KEY_SEPARATOR,s]),Caption);
       end;
    if Result then
       EndOfLineCommentSeparator:=StrWithoutDoubleQuotes(ReadString(TextLines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_END_OF_LINE_COMMENT_SEPARATOR,EndOfLineCommentSeparator));
    if Result then begin
       s:=ReadString(TextLines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_TILE_BASE,'');
       if s<>'' then begin
          i:=0;
          if   ReadUnsignedInteger(s,i,TileBase) and (TileBase>=0) and (TileBase<=1) then
          else Result:=SokUtil_.Error(Format(TokenExpectedText__,[KEY_TILE_BASE,s]),Caption);
          end;
       end;
    if Result and
       (OpenForm.CurrentImageFileName<>'') and
       (OpenForm.Image1.Picture.BitMap<>nil) and
       (OpenForm.Texts[tTextFile].FileName='') then // FileName='': the input file is an image
       with OpenForm.Image1.Picture.BitMap do begin
         BitMapWidth:=Width; BitMapHeight:=Height;
         if GuessColumnsAndRows(OpenForm.Image1.Picture.BitMap,BitMapWidth,BitMapHeight,ColCount,RowCount,VariableColCount,VariableRowCount) then begin
            ColWidth :=Width div Max(1,ColCount);
            if RowCount=0 then
               RowCount:=Max(1,BitMapHeight div Max(1,ColWidth)); // dynamic row-count only works for tiles that are square
            RowHeight:=Max(1,Height div RowCount);

            with RealTileRect do begin
              if Right >0 then Right :=Max(0,ColWidth-Right);
              if Bottom>0 then Bottom:=Max(0,RowHeight-Bottom);
              end;
            end
         else Result:=SokUtil_.Error(UnableToCalculateColumsAndRowsText+NL+NL+PleaseChangeItAndTryAgainText,Caption);
         end;

    if Result then begin
       Result:=Parse(0); // parse the primary script file

       if Result and
          (TextType__=tSkinScript) then begin
          StartLineNo:=0;
          if StrAnsiPosCI(VALUE_FILE_TYPE_SETTINGS_FILE,
                          ReadString(OpenForm.Texts[tSkinScript].Memo.Lines,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_FILE_TYPE,''))<>0 then
             // the secondary script-file should be a Sokoban YASC settings file
             Result:=False // not done yet
          else begin
             InitializeIniFileReader(tTextFile);
             if OpenForm.Texts[tTextFile].FileName<>'' then begin
                StartLineNo:=IndexOfKey(OpenForm.Texts[tTextFile].Memo.Lines,EQUAL,TEXT_APPLICATION_TITLE_LONG,'',i); {empty 'Key': finds the section but returns the negated line number of the next line}
                if StartLineNo<>0 then begin
                   // the secondary script-file contains a line with '[Sokoban YASC]', hence, parse the file, searching for additional information specific to this skin}
                   StartLineNo:=Abs(StartLineNo);
                   Result:=False; // not done yet
                   end;
                end;
             end;

          if not Result then begin // parse the secondary script-file
             TextType__:=tTextFile;
             TextLines:=OpenForm.Texts[tTextFile].Memo.Lines; // 'TextLines' is what the parser sees as input
             Result:=Parse(StartLineNo); // parse the secondary script-file
             end;
          end;

       if Result then Finalize;
       end;
  end; // Parse.Parse

begin // Parse
  try    //StartTimeMS:=GetTickCount;
         oCursor:=Screen.Cursor;
         WorkBitMap.BitMap:=nil; WorkBitMap.FileName:='';
         WorkBitMap.PictureType:=ptScreenBackground;
         FillChar(TileInfo,SizeOf(TileInfo),0);
         FillChar(Variables,SizeOf(Variables),0);
         Pict:=Pict_.TPict.Create;
         try     Screen.Cursor:=crHourGlass;
                 Result:=Parse;
         finally Screen.Cursor:=oCursor; Pict.Free; WorkBitMap.BitMap.Free;
                 for Ch:=Low(Variables) to High(Variables) do Variables[Ch].Text:='';
         end;
         //SokUtil_.Msg(IntToStr(CalculateElapsedTimeMS(StartTimeMS,GetTickCount)),'Parse Time',MB_OK);
  except on E:Exception do begin
            if E.Message<>'' then
               if   AnsiPos(ThisApplicationHandlesTheseImageFormatsOnlyText,E.Message)<>0 then
                    Msg  (E.Message,'',MB_OK+MB_ICONINFORMATION)
               else SokUtil_.Error(E.Message,Caption)
            else begin
               // no message text: silent error, or the error has already been shown
               end;
            Result:=False;
            end;
  end;
end; // Parse

function TSkins.ReadColumnsAndRows(Lines__:TStrings;
                                   var ColCount__,RowCount__:Integer;
                                   var VariableColCount__,VariableRowCount__:Boolean;
                                   var ErrorText__:String):Boolean;
var i:Integer; s:String;
begin
  Result:=False;
  ColCount__:=0; RowCount__:=0;
  VariableColCount__:=True; VariableRowCount__:=True;
  ErrorText__:='';
  InitializeIniFileReader;
  s:=ReadString(Lines__,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_COLUMNS,'');
  if s<>'' then begin
     i:=0;
     if   ReadUnsignedInteger(s,i,ColCount__) and (ColCount__>0) then begin
          Result:=True;
          VariableColCount__:=(i<Length(s)) and (s[STRING_BASE+i]=PERIOD);
          end
     else ErrorText__:=Format(TokenExpectedText__,[KEY_COLUMNS,s]);
     end;
  s:=ReadString(Lines__,EQUAL,INIFILE_SECTION_SOKOBAN_SKIN_SCRIPT,KEY_ROWS,'');
  if s<>'' then begin
     i:=0;
     if   ReadUnsignedInteger(s,i,RowCount__) and (RowCount__>0) then
          VariableRowCount__:=(i<Length(s)) and (s[STRING_BASE+i]=PERIOD)
     else begin Result:=False; ErrorText__:=Format(TokenExpectedText__,[KEY_ROWS,s]);
          end;
     end;
end;

function  TSkins.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
var i:Integer;
begin
  IniFile.EraseSection(SETTINGS_INIFILE_SECTION_SKINS);
  IniFile.EraseSection(SETTINGS_INIFILE_SECTION_SKIN_SCRIPTS);

  Result:=SaveComboBoxToIniFile(IniFile,SETTINGS_INIFILE_SECTION_SKINS,SKINS_CAPACITY,OpenForm.SkinsComboBox);
  for i:=0 to Pred(OpenForm.SkinsComboBox.Items.Count) do // save skintype-association for each skin
      IniFile.WriteString(SETTINGS_INIFILE_SECTION_SKINS,Skins[i,SKIN_NAME_INDEX],Skins[i,SKIN_SCRIPT_INDEX]);

  for i:=0 to Pred(OpenForm.SkinScriptsComboBox.Items.Count) do
      IniFile.WriteString(SETTINGS_INIFILE_SECTION_SKIN_SCRIPTS,IntToStr(Succ(i)),Scripts[i,SKIN_SCRIPT_INDEX]);
end;

procedure TSkins.SetLastSkinNo(LastSkinNo__:Integer);
begin
  fLastSkinNo:=Max(-1,LastSkinNo__);
end;

procedure TSkins.SetScriptFileName(const FileName__:String);
var s:String;
begin // property 'ScriptFileName'
  if   OpenForm<>nil then with OpenForm.Texts[tSkinScript] do
       if   ScriptFileName=FileName__ then //
       else if   LoadTextFromFile(FileName__,tSkinScript,s) then begin
                 end
            else Error(s,'')
  else Error(TEXT_TASK_FAILED,'');
end;

procedure TSkins.SetScripts(Index__,NameOrScript__:Integer; const Value__:String);
var Node:SokUtil_.TNode;
begin // property 'Scripts {Name,FileName}'
  with OpenForm.SkinScriptsComboBox do begin
    if   (Index__>=0) and (Index__<Items.Count) then
         if        NameOrScript__=SKIN_NAME_INDEX then Items[Index__]:=Value__
         else if   NameOrScript__=SKIN_SCRIPT_INDEX then begin
                   if    (Items.Objects[Index__]=nil) and
                         CreateObject(otNode,Node) then Items.Objects[Index__]:=Node;
                   if    Items.Objects[Index__]<>nil then
                         SokUtil_.TNode(Items.Objects[Index__]).Text:=Value__
                   else  raise Exception.Create(TEXT_MEMORY_FULL)
                   end
              else raise Exception.Create(TEXT_RANGE_ERROR)
    else raise Exception.Create(TEXT_RANGE_ERROR);
    end;
end;

procedure TSkins.SetSettingsScriptFileName(FileName__:String);
begin
  fSettingsScriptFileName:=FileName__;
end;

procedure TSkins.SetSkinFileName(const FileName__:String);
var s:String;
begin // property 'SkinFileName'
  if   OpenForm<>nil then with OpenForm.Texts[tTextFile] do
       if   SkinFileName=FileName__ then //
       else if   LoadTextFromFile(FileName__,tTextFile,s) then begin
                 end
            else Error(s,'')
  else Error(TEXT_TASK_FAILED,'');
end;

procedure TSkins.SetSkins(Index__,NameOrScript__:Integer; const Value__:String);
var Node:SokUtil_.TNode;
begin // property 'Skins {FileName,ScriptFileName}'
  with OpenForm.SkinsComboBox do begin
    if   (Index__>=0) and (Index__<Items.Count) then
         if        NameOrScript__=SKIN_NAME_INDEX then Items[Index__]:=Value__
         else if   NameOrScript__=SKIN_SCRIPT_INDEX then begin
                   if    (Items.Objects[Index__]=nil) and
                         CreateObject(otNode,Node) then Items.Objects[Index__]:=Node;
                   if    Items.Objects[Index__]<>nil then
                         SokUtil_.TNode(Items.Objects[Index__]).Text:=Value__
                   else  raise Exception.Create(TEXT_MEMORY_FULL)
                   end
              else raise Exception.Create(TEXT_RANGE_ERROR)
    else raise Exception.Create(TEXT_RANGE_ERROR);
    end;
end;

function  TSkins.UpdateMenu:Boolean;
var i:Integer; MenuItem:TMenuItem;
begin
  Result:=True;
  with OptionsForm.MenuItemRecentSkins do
    try
      while Count>2 do Items[0].Free; // clear the list, leaving static items only
      for i:=Pred(OpenForm.SkinsComboBox.Items.Count) downto 0 do begin
          MenuItem:=TMenuItem.Create(OptionsForm);
          Insert(0,MenuItem);
          MenuItem.Caption:=ExtractFileNameWithoutPathAndExtension(Skins[i,SKIN_NAME_INDEX]);
          MenuItem.OnClick:=OptionsForm.MenuItemLoadSkinFromHistoryClick;
          MenuItem.Tag:=i; // 'Tag' = reference back to the skins
          end;
    except on E:Exception do Result:=Error(E.Message,'');
    end;
end;

function  TSkins.MakeWallFromCommonSkinSource(ColWidth__,RowHeight__:Integer;
                                              var   SourceBitMap__  :TBitMap; // kludge: resizes the source bitmap if tiles have an odd size
                                              var   DestBitMap__    :TBitMap;
                                              var   WallType__      :TWallType):Boolean;
var W,H,Col,Row,ColCount,RowCount:Integer; R0:TRect; B1:TBitMap;

  function MakeWallTiles_3_2:Boolean;

    function BottomRect(Col,Row:Integer):TRect;
    begin
      Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
      Inc(Result.Top,(Result.Bottom-Result.Top) div 2);
    end;

    function LeftRect(Col,Row:Integer):TRect;
    begin
      Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
      Dec(Result.Right,(Result.Right-Result.Left) div 2);
    end;

    function RightRect(Col,Row:Integer):TRect;
    begin
      Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
      Inc(Result.Left,(Result.Right-Result.Left) div 2);
    end;

    function TopRect(Col,Row:Integer):TRect;
    begin
      Result:=CellToRect(Col,Row,ColWidth__,RowHeight__);
      Dec(Result.Bottom,(Result.Bottom-Result.Top) div 2);
    end;

  begin
    with DestBitMap__.Canvas do begin
      //Brush.Color:=RGBToColor(MaskBitMapColor); Brush.Style:=FillRect(Rect(0,0,BitMap.Width,BitMap.Height));
      CopyMode:=cmSrcCopy;

      // 00: no wall neighbours, i.e., a single wall square
      CopyRect(CellToRect( 0,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(1,1,ColWidth__,RowHeight__));

      // 01: wall above
      CopyRect(   TopRect( 1,0),SourceBitMap__.Canvas,   TopRect( 0,1));
      CopyRect(BottomRect( 1,0),SourceBitMap__.Canvas,BottomRect( 1,1));

      // 02: wall to the right
      CopyRect(  LeftRect( 2,0),SourceBitMap__.Canvas, LeftRect(  1,1));
      CopyRect( RightRect( 2,0),SourceBitMap__.Canvas, RightRect( 1,0));

      // 03: wall above and to the right
      CopyRect(CellToRect( 3,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));
      CopyRect(  LeftRect( 3,0),  DestBitMap__.Canvas,  LeftRect( 1,0));
      CopyRect(BottomRect( 3,0),  DestBitMap__.Canvas,BottomRect( 2,0));

      // 04: wall below
      CopyRect(   TopRect( 4,0),SourceBitMap__.Canvas,   TopRect( 1,1));
      CopyRect(BottomRect( 4,0),SourceBitMap__.Canvas,BottomRect( 0,1));

      // 05: wall above and below
      CopyRect(CellToRect( 5,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,1,ColWidth__,RowHeight__));

      // 06: wall below and to the right
      CopyRect(CellToRect( 6,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));
      CopyRect(   TopRect( 6,0),  DestBitMap__.Canvas,   TopRect( 2,0));
      CopyRect(  LeftRect( 6,0),  DestBitMap__.Canvas,  LeftRect( 4,0));

      // 07: wall above, below, and to the right
      CopyRect(  LeftRect( 7,0),SourceBitMap__.Canvas,  LeftRect( 0,1));
      CopyRect( RightRect( 7,0),SourceBitMap__.Canvas, RightRect( 0,0));

      // 08: wall to the left
      CopyRect(  LeftRect( 8,0),SourceBitMap__.Canvas,  LeftRect( 1,0));
      CopyRect( RightRect( 8,0),SourceBitMap__.Canvas, RightRect( 1,1));

      // 09: wall above and to the left
      CopyRect(CellToRect( 9,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));
      CopyRect( RightRect( 9,0),  DestBitMap__.Canvas, RightRect( 1,0));
      CopyRect(BottomRect( 9,0),  DestBitMap__.Canvas,BottomRect( 8,0));

      // 10: wall to the left and to the right
      CopyRect(CellToRect(10,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(1,0,ColWidth__,RowHeight__));

      // 11: wall above, to the left, and to the right
      CopyRect(CellToRect(11,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));
      CopyRect(BottomRect(11,0),SourceBitMap__.Canvas,BottomRect( 1,0));

      // 12: wall below and to the left
      CopyRect(CellToRect(12,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));
      CopyRect(   TopRect(12,0),  DestBitMap__.Canvas,   TopRect( 8,0));
      CopyRect( RightRect(12,0),  DestBitMap__.Canvas, RightRect( 4,0));

      // 13: wall above, below, and to the left
      CopyRect(CellToRect(13,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));
      CopyRect( RightRect(13,0),SourceBitMap__.Canvas, RightRect( 0,1));

      // 14: wall below, to the left, and to the right
      CopyRect(CellToRect(14,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));
      CopyRect(   TopRect(14,0),SourceBitMap__.Canvas,   TopRect( 1,0));

      // 15: walls on all 4 sides
      CopyRect(CellToRect(15,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));

      // 16: wall cap

      {$IFDEF WALL_CAP_QUADRANTS_ABCD}
        CopyRect(CellToRect(16,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(2,0,ColWidth__,RowHeight__));

        { // test
        Brush.Style:=bsSolid;
        Brush.Color:=clLime;
        R0:=CellToRect(16,0,ColWidth__,RowHeight__);
        FillRect(R0);
        Pen.Style:=psSolid; Pen.Color:=clNavy;
        MoveTo(R0.Left,R0.Top); LineTo(R0.Right,R0.Bottom);
        }
      {$ENDIF}

      {$IFDEF WALL_CAP_QUADRANTS_DCBA}
        // the tile consists of 4 quadrants:
        // AB
        // CD
        // in order to cover the gap, the tile is shown halfway between the
        // cells hence, the quadrants are re-arranged like this:
        // DC
        // BA

        CopyRect(CellToRect(32,0,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(5,1,ColWidth__ div 2,RowHeight__ div 2));
        CopyRect(CellToRect(33,0,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(4,1,ColWidth__ div 2,RowHeight__ div 2));
        CopyRect(CellToRect(32,1,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(5,0,ColWidth__ div 2,RowHeight__ div 2));
        CopyRect(CellToRect(33,1,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(4,0,ColWidth__ div 2,RowHeight__ div 2));
      {$ENDIF}

      //BitMapDump(DestBitMap__);
      end;
    Result:=True;
  end; // MakeWallTiles_3_2

  function MakeWallTiles_4_4:Boolean;
  begin
    with DestBitMap__.Canvas do begin
      //Brush.Color:=RGBToColor(MaskBitMapColor); Brush.Style:=FillRect(Rect(0,0,BitMap.Width,BitMap.Height));
      CopyMode:=cmSrcCopy;

      // 00: no wall neighbours, i.e., a single wall square
      CopyRect(CellToRect( 0,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(3,3,ColWidth__,RowHeight__));

      // 01: wall above
      CopyRect(CellToRect( 1,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(3,2,ColWidth__,RowHeight__));

      // 02: wall to the right
      CopyRect(CellToRect( 2,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,3,ColWidth__,RowHeight__));

      // 03: wall above and to the right
      CopyRect(CellToRect( 3,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,2,ColWidth__,RowHeight__));

      // 04: wall below
      CopyRect(CellToRect( 4,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(3,0,ColWidth__,RowHeight__));

      // 05: wall above and below
      CopyRect(CellToRect( 5,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(3,1,ColWidth__,RowHeight__));

      // 06: wall below and to the right
      CopyRect(CellToRect( 6,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,0,ColWidth__,RowHeight__));

      // 07: wall above, below, and to the right
      CopyRect(CellToRect( 7,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(0,1,ColWidth__,RowHeight__));

      // 08: wall to the left
      CopyRect(CellToRect( 8,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(2,3,ColWidth__,RowHeight__));

      // 09: wall above and to the left
      CopyRect(CellToRect( 9,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(2,2,ColWidth__,RowHeight__));

      // 10: wall to the left and to the right
      CopyRect(CellToRect(10,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(1,3,ColWidth__,RowHeight__));

      // 11: wall above, to the left, and to the right
      CopyRect(CellToRect(11,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(1,2,ColWidth__,RowHeight__));

      // 12: wall below and to the left
      CopyRect(CellToRect(12,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(2,0,ColWidth__,RowHeight__));

      // 13: wall above, below, and to the left
      CopyRect(CellToRect(13,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(2,1,ColWidth__,RowHeight__));

      // 14: wall below, to the left, and to the right
      CopyRect(CellToRect(14,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(1,0,ColWidth__,RowHeight__));

      // 15: walls on all 4 sides
      CopyRect(CellToRect(15,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(1,1,ColWidth__,RowHeight__));

      // 16: wall cap

      {$IFDEF WALL_CAP_QUADRANTS_ABCD}
        CopyRect(CellToRect(16,0,ColWidth__,RowHeight__),SourceBitMap__.Canvas,CellToRect(3,3,ColWidth__,RowHeight__));
      {$ENDIF}

      {$IFDEF WALL_CAP_QUADRANTS_DCBA}
        // the tile consists of 4 quadrants:
        // AB
        // CD
        // in order to cover the hole, the tile is shown halfway between the
        // cells, hence, the quadrants are re-arranged like this:
        // DC
        // BA

        CopyRect(CellToRect(32,0,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(5,5,ColWidth__ div 2,RowHeight__ div 2));
        CopyRect(CellToRect(33,0,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(4,5,ColWidth__ div 2,RowHeight__ div 2));
        CopyRect(CellToRect(32,1,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(5,4,ColWidth__ div 2,RowHeight__ div 2));
        CopyRect(CellToRect(33,1,ColWidth__ div 2,RowHeight__ div 2),SourceBitMap__.Canvas,CellToRect(4,4,ColWidth__ div 2,RowHeight__ div 2));
      {$ENDIF}

      //BitMapDump(DestBitMap__);
      end;
    Result:=True;
  end; // MakeWallTiles_4_4

begin // MakeWallFromCommonSkinSource
  Result:=False;
  if (ColWidth__>=2) and (RowHeight__>=2) and
     (SourceBitMap__<>nil) and (DestBitMap__<>nil) then with SourceBitMap__ do begin

     ColCount:=Width  div ColWidth__;
     RowCount:=Height div RowHeight__;

     W:=ColWidth__; H:=RowHeight__;
     if Odd(W) then Inc(W);
     if Odd(H) then Inc(H);
     if ((W<>ColWidth__) or (H<>RowHeight__)) and
        BitMapResize(DestBitMap__,ColCount*W,RowCount*H) then
        with DestBitMap__ do with Canvas do begin
          CopyMode:=cmSrcCopy;
          if (ColCount=0) or (RowCount=0) then
             StretchDraw(Classes.Rect(0,0,Width,Height),SourceBitMap__)
          else begin
             R0:=CellToRect(0,0,W,H);
             if BitMapCreate(B1,W,H) then
                try     // resize the tiles individually, otherwise they contaminate each other
                        B1.Canvas.CopyMode:=cmSrcCopy;
                        for Col:=0 to Pred(ColCount) do
                            for Row:=0 to Pred(RowCount) do begin
                                B1.Canvas.CopyRect(R0,SourceBitMap__.Canvas,CellToRect(Col,Row,ColWidth__,RowHeight__));
                                DestBitMap__.Canvas.StretchDraw(CellToRect(Col,Row,W,H),B1);
                                end;
                finally B1.Free;
                end;
             end;

          BitMapSwap(SourceBitMap__,DestBitMap__);
          ColWidth__:=W; RowHeight__:=H;
          end;

     if (SourceBitMap__.Height>=4) and (SourceBitMap__.Width>=6) and
        BitMapResize(DestBitMap__,WALL_TILE_COUNT*ColWidth__,RowHeight__) then
        with SourceBitMap__ do
          if      3 * Height * ColWidth__ = 2 * Width * RowHeight__ then begin
                  Result:=MakeWallTiles_3_2;
                  WallType__:=wtSeamlessWallWithCap;
                  end
          else if Height*RowHeight__=Width*ColWidth__     then begin
                  Result:=MakeWallTiles_4_4;
                  WallType__:=wtSeamlessWallNoCap;
                  end;
     end;
end;

function  TSkins.MakeWallFromMinimumTileSet(var BitMap__:TBitMap; var TileCount__:Integer; var TileNoSet__:TTileNoSet):Boolean;
const
    MAX_TILE_COUNT=17;
    MINIMUM_WALL_TILE_NO_SET_1:TTileNoSet=[0,5,10,15];
    MINIMUM_WALL_TILE_NO_SET_2:TTileNoSet=[0,2, 8,10]; // walls are connected left and right only, i.e., not up and down
var i,ColWidth,RowHeight:Integer;

  function BottomRect(Tile:Integer):TRect;
  begin
    Result:=CellToRect(Tile,0,ColWidth,RowHeight);
    with Result do Inc(Top,(Bottom-Top) div 2);
  end;

  function LeftRect(Tile:Integer):TRect;
  begin
    Result:=CellToRect(Tile,0,ColWidth,RowHeight);
    with Result do Dec(Right,(Right-Left) div 2);
  end;

  function RightRect(Tile:Integer):TRect;
  begin
    Result:=CellToRect(Tile,0,ColWidth,RowHeight);
    with Result do Inc(Left,(Right-Left) div 2);
  end;

  function TopRect(Tile:Integer):TRect;
  begin
    Result:=CellToRect(Tile,0,ColWidth,RowHeight);
    with Result do Dec(Bottom,(Bottom-Top) div 2);
  end;

begin // MakeWallFromMinimumTileSet; precondition: 'BitMap__' has the proper size for the 17 wall-tiles
  Result:=((TileNoSet__*MINIMUM_WALL_TILE_NO_SET_1=MINIMUM_WALL_TILE_NO_SET_1)
           or
           (TileNoSet__*MINIMUM_WALL_TILE_NO_SET_2=MINIMUM_WALL_TILE_NO_SET_2)
          )
          and
          (BitMap__<>nil)
          and
          (BitMap__.Width>=MAX_TILE_COUNT)
          and
          (BitMap__.Height>=1);
  if Result and
     (TileCount__<MAX_TILE_COUNT) then with BitMap__ do with Canvas do begin
     CopyMode:=cmSrcCopy;
     ColWidth:=Width div MAX_TILE_COUNT; RowHeight:=Height;

     if not ( 5 in TileNoSet__) then // 05: wall above and below
        CopyRect(CellToRect( 5,0,ColWidth,RowHeight),Canvas,CellToRect( 0,0,ColWidth,RowHeight)); // walls are connected left and right only

     if not (15 in TileNoSet__) then // 15: walls on all 4 sides
        CopyRect(CellToRect(15,0,ColWidth,RowHeight),Canvas,CellToRect(10,0,ColWidth,RowHeight)); // walls are connected left and right only

     // 00: no wall neighbours, i.e., a single wall square

     if not ( 1 in TileNoSet__) then begin // 01: wall above
        CopyRect(   TopRect( 1),Canvas,   TopRect( 5));
        CopyRect(BottomRect( 1),Canvas,BottomRect( 0));
        end;

     if not ( 2 in TileNoSet__) then begin // 02: wall to the right
        CopyRect(  LeftRect( 2),Canvas,  LeftRect( 0));
        CopyRect( RightRect( 2),Canvas, RightRect(10));
        end;

     if not ( 3 in TileNoSet__) then begin // 03: wall above and to the right
        CopyRect(   TopRect( 3),Canvas,   TopRect(15));
        CopyRect(BottomRect( 3),Canvas,BottomRect( 2));
        CopyRect(  LeftRect( 3),Canvas,  LeftRect( 1));
        end;

     if not ( 4 in TileNoSet__) then begin // 04: wall below
        CopyRect(   TopRect( 4),Canvas,   TopRect( 0));
        CopyRect(BottomRect( 4),Canvas,BottomRect( 5));
        end;

     // 05: wall above and below

     if not ( 6 in TileNoSet__) then begin // 06: wall below and to the right
        CopyRect(BottomRect( 6),Canvas,BottomRect(15));
        CopyRect(   TopRect( 6),Canvas,   TopRect( 2));
        CopyRect(  LeftRect( 6),Canvas,  LeftRect( 4));
        end;

     if not ( 7 in TileNoSet__) then begin // 07: wall above, below, and to the right
        CopyRect(   TopRect( 7),Canvas,   TopRect(15));
        CopyRect(BottomRect( 7),Canvas,BottomRect(15));
        CopyRect(  LeftRect( 7),Canvas,  LeftRect( 5));
        end;

     if not ( 8 in TileNoSet__) then begin // 08: wall to the left
        CopyRect(  LeftRect( 8),Canvas,  LeftRect(10));
        CopyRect( RightRect( 8),Canvas, RightRect( 0));
        end;

     if not ( 9 in TileNoSet__) then begin // 09: wall above and to the left
        CopyRect(   TopRect( 9),Canvas,   TopRect(15));
        CopyRect( RightRect( 9),Canvas, RightRect( 1));
        CopyRect(BottomRect( 9),Canvas,BottomRect( 8));
        end;

      // 10: wall to the left and to the right

      if not (11 in TileNoSet__) then begin // 11: wall above, to the left, and to the right
         CopyRect(   TopRect(11),Canvas,   TopRect(15));
         CopyRect(BottomRect(11),Canvas,BottomRect(10));
         end;

      if not (12 in TileNoSet__) then begin // 12: wall below and to the left
         CopyRect(BottomRect(12),Canvas,BottomRect(15));
         CopyRect( RightRect(12),Canvas, RightRect( 4));
         CopyRect(   TopRect(12),Canvas,   TopRect( 8));
         end;

      if not (13 in TileNoSet__) then begin // 13: wall above, below, and to the left
         CopyRect(  LeftRect(13),Canvas,  LeftRect(15));
         CopyRect( RightRect(13),Canvas, RightRect( 5));
         end;

      if not (14 in TileNoSet__) then begin // 14: wall below, to the left, and to the right
         CopyRect(   TopRect(14),Canvas,   TopRect(10));
         CopyRect(BottomRect(14),Canvas,BottomRect(15));
         end;

      // 15: walls on all 4 sides

      // 16: wall cap
      //-------------

      for i:=0 to 15 do // '15': the last item 16 is the wall cap, and it isn't modified by this function
          if not (i in TileNoSet__) then begin
             Inc(TileCount__); Include(TileNoSet__,i);
             end;
     end;
end;

function  TSkins.CalculateOuterWallTrimming(const BitMap__:TBitMap; Rect__:TRect; MaskPct__:Integer; var OuterWallTrimming__:TRect):Boolean;
begin // not implemented
  Result:=False; FillChar(OuterWallTrimming__,SizeOf(OuterWallTrimming__),0);
end;

end.

