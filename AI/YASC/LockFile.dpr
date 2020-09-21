
{$APPTYPE CONSOLE}
program LockFile;
uses SysUtils;

const
  TEXT_PRESS_ENTER = 'Press [Enter]';

function WriteToFile(const FileName,Text: String):Integer;
var
  FileHandle : Integer;
begin
  Result:=-1;
  FileHandle := FileOpen(FileName, fmOpenReadWrite or fmShareDenyWrite);
  if FileHandle > 0 then begin
    //[valid file handle}

    Writeln(FileName);

    Writeln;
    Write(TEXT_PRESS_ENTER);
    Readln;

    if Length(Text)<>0 then begin
       Result:=FileWrite(FileHandle,Pointer(Addr(Text[1]))^,Length(Text));
       if   Result=Length(Text)  then
            Writeln('File write OK')
       else Writeln('File write error');
       end
    else Result:=0;

    FileClose(FileHandle);
    end
  else begin
    // [Open error: FileHandle = negative DOS error code]
    Writeln('Open error ',FileHandle,' on file ',FileName);
    end;
end;

begin
  Writeln('Lock File');
  Writeln;

  if (ParamCount=1) and FileExists(ParamStr(1)) then
     WriteToFile(ParamStr(1),'')
  else begin
     Writeln('Usage:');
     Writeln('  LockFile <file name>');
     Writeln;
     Write(TEXT_PRESS_ENTER);
     Readln;
     end;
end.
