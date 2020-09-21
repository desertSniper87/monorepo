unit IniFile_;
// standard inifiles are limited to 65535 bytes;
// this simplified version does not have this limitation

interface

uses Classes;

type

  TIniFile = class
  private
    FFileName:String;
    SL:TStringList;
    Modified:Boolean;
    function    Close:Boolean;
    function    Open(const FileName__:String):Boolean;
    function    Flush:Boolean;
    function    IndexOfKey(const Section,Key:String):Integer;
    procedure   SetFileName(const FileName__:String);
  protected
    function    GetItems(Index__:Integer):String;
  public
    procedure   Clear;
    constructor Create(const FileName__:String);
    destructor  Destroy; override;

    property    FileName:String read FFileName write SetFileName;
    property    Items[Index__:Integer]:String read GetItems;

    procedure   DeleteKey(const Section, Key: String);
    procedure   EraseSection(const Section: String);
    function    LoadFromFile(const FileName__:String):Boolean;
    function    LineCount:Integer;
    function    ReadBool(const Section, Key: String; DefaultValue: Boolean): Boolean;
    function    ReadBoolText(const Section, Key: String; DefaultValue: Boolean): Boolean;
    function    ReadInteger(const Section, Key: String; DefaultValue: Integer): Integer;
    procedure   ReadSection (const Section: String; Strings: TStrings);
    procedure   ReadSections(Strings: TStrings);
    function    ReadString(const Section, Key, DefaultValue: String): String;
    function    RenameSection(const OldName,NewName:String):Boolean;
    function    SectionExists (const Section: String): Boolean;
    procedure   WriteBool(const Section, Key: String; Value: Boolean);
    procedure   WriteBoolText(const Section, Key: String; Value: Boolean);
    procedure   WriteInteger(const Section, Key: String; Value: Integer);
    procedure   WriteString(const Section, Key, Value: String);
  end;

{exported functions}

function  IndexOfKey(const Strings: TStrings; KeyValueSeparator__:Char; const Section,Key:String; var ValuePosition__:Integer):Integer;
function  InitializeIniFileReader:Boolean;
function  ReadSection(const Strings:TStrings; const Section__:String; var SectionAsString__:String):Boolean;
function  ReadString(const Strings:TStrings;  KeyValueSeparator__:Char; const Section, Key, DefaultValue: String): String;

implementation

uses SysUtils,Windows,Forms,Misc_,SokUtil_,Text_;

var
  // note: there is only one reader in this application, therefore, the following variables are global
  LastSection:String;
  IndexOfLastSection:Integer;

function  IndexOfKey(const Strings: TStrings; KeyValueSeparator__:Char; const Section,Key:String; var ValuePosition__:Integer):Integer;
var i,KeyLength,StrLength:Integer; s:String;
begin // precondition: 'InitializeIniFileReader' has been called in order to clear the cached section in case the last one isn't valid anymore
  Result:=0;
  if Strings<>nil then begin
     if      (Section=LastSection) then Result:=IndexOfLastSection
     else if Section<>'' then begin
             //Result:=Succ(Strings.IndexOf(StrWithBrackets(Section)));
             Result:=-1; s:=StrWithBrackets(Section);
             for i:=0 to Pred(Strings.Count) do
                 if (Strings[i]<>'') and
                    (Strings[i][1]=LEFT_BRACKET) and
                    (AnsiCompareText(Strings[i],s)=0) then begin
                    Result:=i; break;
                    end;
             Inc(Result);
             end;
     LastSection:=Section; IndexOfLastSection:=Result;

     if (Result>0) or (Section='') then begin // Result return values: "0": section does not exist; "-": insertion point; "+": position
        KeyLength:=Length(Key);
        if KeyLength<>0 then
           while Result<Strings.Count do begin
             s:=Strings.Strings[Result]; StrLength:=Length(s);
             if        s='' then Inc(Result)
             else if   s[1]=LEFT_BRACKET then begin
                       if (Result>0) and (Strings.Strings[Pred(Result)]='') then Dec(Result);
                       break;
                       end
             else if   StrLength<=KeyLength then Inc(Result)
             else if   AnsiStrLIComp(PChar(Key),PChar(s),KeyLength)=0 then begin // 'True': the string begins with the key (case insensitively)
                       i:=Succ(KeyLength);
                       if   KeyValueSeparator__>SPACE then
                            while (s[i]<=SPACE) and (i<StrLength) do Inc(i); // skip any trailing spaces after the key
                       if   s[i]=KeyValueSeparator__ then begin // 'True': found the key;
                            ValuePosition__:=Succ(i); // now help the caller by pointing to the first non-blank character in the value (if any)
                            while (ValuePosition__<=StrLength) and
                                  (s[ValuePosition__]<=SPACE) do
                                  Inc(ValuePosition__);
                            exit; // quick-and-dirty exit
                            end
                       else Inc(Result);
                       end
             else      Inc(Result);
             end;

        Result:=-Result;
        end;
     end;
end;

function InitializeIniFileReader:Boolean; // note: there is only one reader in this application, therefore, the variables are global
begin
  Result:=True; LastSection:=''; IndexOfLastSection:=0;
end;

function  ReadSection(const Strings:TStrings; const Section__:String; var SectionAsString__:String):Boolean;
var i,j:Integer; s:String; Section:TStringList;
begin
 i:=Abs(IndexOfKey(Strings,EQUAL,Section__,'',j)); SectionAsString__:='';
 Result:=i<>0;
 if Result then with Strings do begin
    j:=i;
    while (j<Count) and (j>0) do begin
      s:=Strings[j];
      if (s<>'') and (s[1]=LEFT_BRACKET) then j:=-j
      else Inc(j);
      end;

    try  Section:=TStringList.Create;
         try     for i:=i to Pred(Abs(j)) do Section.Add(Strings[i]);
                 SectionAsString__:=Section.Text;
         finally Section.Free;
         end;
    except
      on E:Exception do Result:=Error(E.Message,Application.Title);
    end;
    end;
end;

function  ReadString(const Strings:TStrings; KeyValueSeparator__:Char; const Section, Key, DefaultValue: String): String;
var i,ValuePosition:Integer;
begin
  i:=IndexOfKey(Strings,KeyValueSeparator__,Section,Key,ValuePosition);
  if   i>0 then
       Result:=Copy(Strings.Strings[i],ValuePosition,MaxInt)
  else Result:=DefaultValue;
  if (Result<>'') and
     ((Result[1]=SPACE) or (Result[Length(Result)]=SPACE)) then
     Result:=Trim(Result);
end;

constructor TIniFile.Create(const FileName__:String);
begin
  Modified:=False; SL:=nil;
  try    SL:=TStringList.Create;
         if   Open(FileName__) then //
         else FileName:=FileName__;
  except on E:Exception do Destroy;
  end;
end;

destructor TIniFile.Destroy;
begin
  Close; SL.Free; SL:=nil;
end;

procedure TIniFile.SetFileName(const FileName__:String);
begin
  FFileName:=FileName__;
end;

function TIniFile.Open(const FileName__:String):Boolean;
begin
  Result:=(SL<>nil) and (AnsiCompareText(FileName,FileName__)=0);
  if (not Result) and Close and (SL<>nil) then begin
     FileName:=FileName__;
     if FileExists(FileName) then
        try    SL.LoadFromFile(FileName); Result:=True;
               if SL.Count>0 then SL.Strings[ 0 ] := StrRemoveBOM( SL.Strings[ 0 ] );
               with SL do while (Count>0) and (Trim(Strings[Pred(Count)])='') do Delete(Pred(Count)); // delete any trailing blank lines
               IniFile_.ReadString(SL,EQUAL,'','','');; // to clear the cache
        except on E:Exception do begin
                  Close;
                  Result:=Error(Format(OpenFileFailedShortText__,[FileName])+NL+NL+
                                TEXT_FAILURE_DESCRIPTION+NL+E.Message,
                                Application.Title+' - IniFile.Open');
                  end;
        end
     else Result:=True; {creates a new empty inifile}
     end;
end;

function  TIniFile.Close:Boolean;
begin
  Result:=Flush; if Result then Clear;
end;

function  TIniFile.Flush:Boolean;
begin
  if Modified then
     try    if FileName='' then raise Exception.Create(FileNameMissingText);
            SL.SaveToFile(FileName);
            Modified:=False;
     except on E:Exception do Error(Format(SaveFileFailedText__,[FileName])+NL+NL+
                                    TEXT_FAILURE_DESCRIPTION+NL+E.Message,
                                    Application.Title+' - IniFile.Flush');

     end;
  Result:=Modified=False;
end;

procedure TIniFile.Clear;
begin
  if SL<>nil then SL.Clear;
  Modified:=False; FFileName:=''; InitializeIniFileReader;
end;

function  TIniFile.IndexOfKey(const Section,Key:String):Integer;
var i:Integer;
begin
  Result:=IniFile_.IndexOfKey(SL,EQUAL,Section,Key,i);
end;

procedure TIniFile.DeleteKey(const Section, Key: String);
var i:Integer;
begin
  if SL<>nil then begin
     i:=IndexOfKey(Section,Key);
     if i>0 then SL.Delete(i);
     end;
end;

procedure TIniFile.EraseSection(const Section: String);
var i:Integer;
begin
  if SL<>nil then with SL do begin
     i:=IndexOf(StrWithBrackets(Section));
     if i>=0 then begin
        LastSection:=''; IndexOfLastSection:=0;

        Modified:=True;
        repeat Delete(i);
        until  (i>=Count) or
               StrBeginsWithChar(Strings[i],LEFT_BRACKET);

        Dec(i);
        while (i>=0) and (Trim(Strings[i])='') do begin
          if (i=0) or (Trim(Strings[Pred(i)])='') then begin
             Delete(i); Dec(i);
             end
          else i:=-1;
          end;
        end;
     end;
end;

function  TIniFile.GetItems(Index__:Integer):String;
begin
  if   (Index__>=0) and (Index__<SL.Count) then
       Result:=SL.Strings[Index__]
  else Result:='';
end;

function  TIniFile.LineCount:Integer;
begin
  if SL<>nil then Result:=SL.Count
  else Result:=0;
end;

function  TIniFile.LoadFromFile(const FileName__:String):Boolean;
begin
  if   FileExists(FileName__) then
       Result:=Open(FileName__)
  else Result:=Error(Format(TEXT_FILE_NOT_FOUND_FORMAT,[FileName__]),Application.Title+' - IniFile.LoadFromFile');
end;

function  TIniFile.ReadBool(const Section, Key: String; DefaultValue: Boolean): Boolean;
begin
  Result:=ReadInteger(Section,Key,Ord(DefaultValue))<>0;
end;

function  TIniFile.ReadBoolText(const Section, Key: String; DefaultValue: Boolean): Boolean;
var ValueAsText:String;
begin
  ValueAsText:=ReadString(Section,Key,BooleanText[DefaultValue]);
  Result:=StrEqual(ValueAsText,BooleanText[True]) or (ValueAsText='1'); // '1': interpret the values 1 and 0 as 'true' and 'false'
end;

function  TIniFile.ReadInteger(const Section, Key: String; DefaultValue: Integer): Integer;
var s:String;
begin
  s:=Trim(ReadString(Section,Key,''));
  if   (s<>'') and SafeStrToInt(s,False,Result) then //
  else Result:=DefaultValue;
end;

procedure TIniFile.ReadSection{-keys} (const Section: String; Strings: TStrings);
var i,j:Integer; // returns section keys, not values
begin
  if Strings<>nil then begin
     Strings.Clear;
     if SL<>nil then begin
        i:=Succ(SL.IndexOf(StrWithBrackets(Section)));
        if i>0 then
           while (i<SL.Count) and
                 (not StrBeginsWithChar(SL.Strings[i],LEFT_BRACKET)) do begin
                 j:=AnsiPos(EQUAL,SL.Strings[i]);
                 if j>0 then Strings.Add(Copy(SL.Strings[i],1,Pred(j)));
                 Inc(i);
                 end;
        end;
     end;
end;

procedure TIniFile.ReadSections(Strings: TStrings);
var i:Integer; // returns section names
begin
  if Strings<>nil then begin
     Strings.Clear;
     if SL<>nil then
        for i:=0 to Pred(SL.Count) do
            if StrBeginsWithChar(SL.Strings[i],LEFT_BRACKET) then
               Strings.Add(StrWithoutBrackets(SL.Strings[i]));
     end;
end;

function  TIniFile.ReadString(const Section, Key, DefaultValue: String): String;
begin
  Result:=IniFile_.ReadString(SL,EQUAL,Section,Key,DefaultValue);
end;

function  TIniFile.RenameSection(const OldName,NewName:String):Boolean;
var i:Integer;
begin
  if (SL<>nil) and (OldName<>NewName) then begin
     i:=SL.IndexOf(StrWithBrackets(OldName));
     Result:=(i>=0) and (not SectionExists(NewName));
     if Result then begin
        SL.Strings[i]:=StrWithBrackets(NewName);
        Modified:=True;
        end;
     end
  else Result:=False;
end;

function  TIniFile.SectionExists (const Section: String): Boolean;
begin
  Result:=(SL<>nil) and (SL.IndexOf(StrWithBrackets(Section))>=0);
end;

procedure TIniFile.WriteBool(const Section, Key: String; Value: Boolean);
begin
  WriteInteger(Section,Key,Ord(Value));
end;

procedure TIniFile.WriteBoolText(const Section, Key: String; Value: Boolean);
begin
  WriteString(Section,Key,BooleanText[Value]);
end;

procedure TIniFile.WriteInteger(const Section, Key: String; Value: Integer);
begin
  WriteString(Section,Key,IntToStr(Value));
end;

procedure TIniFile.WriteString(const Section, Key, Value: String);
var i:Integer; s:String;
begin
  if SL<>nil then with SL do begin
     i:=IndexOfKey(Section,Key); s:=Key+EQUAL+Value;
     if i>0 then
        if SL.Strings[i]<>s then begin
           Modified:=True;
           SL.Strings[i]:=s;
           end
        else
     else begin
       Modified:=True;
       if i=0 then begin // new section
          if Count<>0 then Add('');
          Add(StrWithBrackets(Section)); Add(s);
          InitializeIniFileReader; // in order to search for the section the next time
          end
       else begin
          i:=Abs(i);
          if i<Count then Insert(i,s)
          else Add(s);
          end;
       end;
     end;
end;

end.
