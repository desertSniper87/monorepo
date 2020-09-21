unit PNG_;

interface

uses Windows,Graphics;

type
  TLoadPNGImageFromFile    = function( FileName__    : PChar;
                                       var Picture__ : TPicture
                                     ) : Boolean; stdcall;

  TPNGImageLoader          = class( TObject )
  private
    DLLHandle              : THandle;
    LoadFromFileFunction   : TLoadPNGImageFromFile;
  public
    constructor              Create;
    destructor               Destroy; override;

    function                 LoadFromFile(
                               const FileName__ : String;
                               var   Picture__  : TPicture ) : Boolean;
  end;

function  IsPNGFile( const FileName__ : String ) : Boolean;

implementation

uses SysUtils,Forms,
     SokUtil_,SokFile_,Misc_,Plugin_;

const
  PNG_IMAGE_LOADER_DLL_FILENAME = 'PNG.dll';

function  IsPNGFile( const FileName__ : String ) : Boolean;
const PNG_FILE_HEADER : array[ 0 .. 9 ] of Char = (
      Char($89), Char($50), Char($4E), Char($47),
      Char($0D), Char($0A), Char($1A), Char($0A),
      Char($00), Char($00) ); // '$00', '$00': string terminators
begin
  Result := ( System.Pos( FILE_NAME_WIDE_CHARACTER_TO_ANSI_CHARACTER_CONVERSION_ERROR_CHARACTER, FileName__ ) = 0 )
            and
            FileStartsWith( FileName__, PNG_FILE_HEADER );
end;

constructor TPNGImageLoader.Create;
begin
  DLLHandle := 0; LoadFromFileFunction:=nil;
end;

destructor TPNGImageLoader.Destroy;
begin
  if DLLHandle <> 0 then
     try    FreeLibrary( DLLHandle );
     except on E : Exception do begin end;
     end;
  DLLHandle := 0;
end;

function TPNGImageLoader.LoadFromFile(
           const FileName__ : String; var Picture__ : TPicture ) : Boolean;

var Path, DLLFileName : String;
begin
  Result := DLLHandle <> 0;
  if not Result then begin
     Path        := ExtractFilePath( Application.ExeName );
     DLLFileName := Path + PNG_IMAGE_LOADER_DLL_FILENAME;
     Result      := FileExists( DLLFileName );
     if not Result then begin
        DLLFileName := Path+
                       StrWithTrailingPathDelimiter(DEFAULT_PLUGINS_DIRECTORY)+
                       PNG_IMAGE_LOADER_DLL_FILENAME;
        Result      := FileExists( DLLFileName );
        end;
     if Result then
        try    DLLHandle := LoadLibrary( PChar( DLLFileName ) );
        except on E : Exception do DLLHandle := 0;
        end;
     Result := DLLHandle <> 0;
     if Result then
        LoadFromFileFunction := GetProcAddress( DLLHandle, PChar( 'LoadPNGImageFromFile' ) );
     end;
   Result:=Result and
           Assigned( LoadFromFileFunction ) and
           LoadFromFileFunction( PChar( FileName__ ), Picture__ );
end;

end.