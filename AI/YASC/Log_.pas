unit Log_;

interface


type
  TLogFile = class
  private
    fEnabled:Boolean;
    fFileName:String;
    fState:Integer;
    TextFile:System.TextFile;
  protected
    function    Open(const FileName__:String; New__:Boolean):Boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    function    Append(const FileName__:String):Boolean;
    function    Close:Boolean;
    function    New(const FileName__:String):Boolean;
    function    Writeln(const Text__:String):Boolean;

    property    Enabled:Boolean read fEnabled write fEnabled;
    property    FileName:String read fFileName;
    property    State:Integer read fState;
  end;

implementation

uses SysUtils,SokUtil_;

constructor TLogFile.Create;
begin
  Enabled:=True; fState:=fmClosed; fFileName:='';
end;

destructor TLogFile.Destroy;
begin
  Close;
  Enabled:=False; fFileName:='';
end;

function TLogFile.Append(const FileName__:String):Boolean;
begin
  Result:=Open(FileName__,False);
end;

function TLogFile.Close:Boolean;
begin
  Enabled:=False;
  if State=fmOutput then
     try    CloseFile(TextFile); fState:=fmClosed;
     except on E:Exception do Error(E.Message,TEXT_CLOSE_FILE_ERROR+': '+FileName);
     end;
  Result:=State=fmClosed;
end;

function TLogFile.New(const FileName__:String):Boolean;
begin
  Result:=Open(FileName__,True);
end;

function TLogFile.Open(const FileName__:String; New__:Boolean):Boolean;
begin
  Result:=False;
  if StrEqual(FileName,FileName__) and (State=fmOutput) and (not New__) then begin
     Result:=True; Enabled:=True;
     end
  else begin
     if not FileExists(FileName__) then New__:=True;
     if Close then begin
        Enabled:=True; fFileName:=FileName__;
        try    Assign(TextFile,FileName);
               if New__ then Rewrite(TextFile)
               else System.Append(TextFile);
               fState:=fmOutput; Result:=True;
        except on E:Exception do begin
                  fState:=fmClosed;
                  Result:=Error(E.Message,TEXT_OPEN_FILE_ERROR+': '+FileName);
                  end;
        end;
        end;
     end;
end;

function  TLogFile.Writeln(const Text__:String):Boolean;
begin
  if not Enabled then Result:=True
  else if   State=fmOutput then begin
            try System.Writeln(TextFile,Text__); Flush(TextFile); Result:=True;
            except on E:Exception do begin
                      Result:=Error(E.Message,FileName);
                      Close;
                      end;
            end;
            end
       else Result:=False;

end;

end.
