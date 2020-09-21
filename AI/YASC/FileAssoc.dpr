program FileAssoc;

uses
  Windows,ShlObj,Registry,IniFiles;
{$R FileAssocManifest.RES}

const
  ASSOCIATED_APPLICATION_NAME
                          = 'Sokoban.exe';
  BACKSLASH               = '\';
  CR                      = #13;
  FILE_PATH_SEPARATOR_CHAR=  BACKSLASH;
  LF                      = #10;
  FILE_ASSOC_SECTION      = 'File Associations';
  OLD_SOK_ASSOC_KEY       = 'Old SOK Association';
  OLD_XSB_ASSOC_KEY       = 'Old XSB Association';
  NL                      = CR+LF;
  SOK_ASSOC_KEY           = '.sok';
  XSB_ASSOC_KEY           = '.xsb';
  YASC_ASSOC_KEY_VALUE    = 'Sokoban YASC (bd).Level';

type
  TFileAssoc              = class(TObject)
  private
    AssociatedApplicationName
                          : String;
    OldSOKFileAssociation : String;
    OldXSBFileAssociation : String;
    SOKFileAssociation    : String;
    XSBFileAssociation    : String;
    RegIniFile            : TRegIniFile;
  public
    constructor       Create;
    destructor        Destroy; override;

    function          AssociateFileTypes:Boolean;
    function          Close:Boolean;
    function          HasFileAssociations:Boolean;
    function          LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function          Open:Boolean;
    function          Refresh:Boolean;
    function          SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    function          UnAssociateFileTypes:Boolean;
  end;

constructor TFileAssoc.Create;
var i:Integer;
begin
  AssociatedApplicationName:=ParamStr(0);
  i:=Length(AssociatedApplicationName);
  while (i<>0) and (AssociatedApplicationName[i]<>FILE_PATH_SEPARATOR_CHAR) do Dec(i);
  AssociatedApplicationName:=Copy(AssociatedApplicationName,1,i)+ASSOCIATED_APPLICATION_NAME;
  RegIniFile:=nil;
  OldSOKFileAssociation:=SOK_ASSOC_KEY; // self-references means 'undefined' here
  OldXSBFileAssociation:=XSB_ASSOC_KEY;
  SOKFileAssociation   :='';
  XSBFileAssociation   :='';
  if ParamCount>=2 then OldSOKFileAssociation:=ParamStr(2); // retrieve the old associations from the input parameters
  if ParamCount>=3 then OldXSBFileAssociation:=ParamStr(3);
end;

destructor TFileAssoc.Destroy;
begin
  Close;
end;

function  TFileAssoc.AssociateFileTypes:Boolean;
begin
  Result:=Refresh;
  if Result then with RegIniFile do begin
     if (SOKFileAssociation<>YASC_ASSOC_KEY_VALUE) and
        (SOKFileAssociation<>'') then OldSOKFileAssociation:=SOKFileAssociation;
     if (XSBFileAssociation<>YASC_ASSOC_KEY_VALUE) and
        (XSBFileAssociation<>'') then OldXSBFileAssociation:=XSBFileAssociation;

     WriteString(SOK_ASSOC_KEY,'',YASC_ASSOC_KEY_VALUE);
     WriteString(XSB_ASSOC_KEY,'',YASC_ASSOC_KEY_VALUE);

     WriteString(YASC_ASSOC_KEY_VALUE,'','Sokoban Level');

     Result:=OpenKey(YASC_ASSOC_KEY_VALUE,True);
     if Result then WriteString('DefaultIcon','',AssociatedApplicationName+',0');

     Result:=Result and
             OpenKey('shell',True) and
             OpenKey('open',True);
     if Result then WriteString('command','',AssociatedApplicationName+' "%1"');

     CloseKey;

     SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
     end;
end;

function  TFileAssoc.Close:Boolean;
begin
//  try  // try-except disabled to avoid linking the large 'SysUtils' to the program
         RegIniFile.Free; RegIniFile:=nil;
//  except on E:Exception do Error(E.Message,Application.Title);
//  end;
  Result:=RegIniFile=nil;
end;

function  TFileAssoc.HasFileAssociations:Boolean;
begin
  Result:=Refresh and
          (SOKFileAssociation=YASC_ASSOC_KEY_VALUE) and
          (XSBFileAssociation=YASC_ASSOC_KEY_VALUE);
end;

function  TFileAssoc.LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
begin
  Result:=True;
  OldSOKFileAssociation:=IniFile.ReadString(FILE_ASSOC_SECTION,OLD_SOK_ASSOC_KEY,OldSOKFileAssociation);
  if OldSOKFileAssociation='' then OldSOKFileAssociation:=SOK_ASSOC_KEY;
  OldXSBFileAssociation:=IniFile.ReadString(FILE_ASSOC_SECTION,OLD_XSB_ASSOC_KEY,OldXSBFileAssociation);
  if OldXSBFileAssociation='' then OldXSBFileAssociation:=XSB_ASSOC_KEY;
end;

function  TFileAssoc.Open:Boolean;
begin
  if RegIniFile=nil then begin
//     try  // try-except disabled to avoid linking the large 'SysUtils' to the program
            RegIniFile:=TRegIniFile.Create('');
//     except on E:Exception do begin
//            RegIniFile.Free; RegIniFile:=nil;
//            Error(E.Message,Application.Title);
//            end;
//     end;
     end;

  Result:=RegIniFile<>nil;
  if Result then begin
     RegIniFile.RootKey:=HKEY_CLASSES_ROOT;
     RegIniFile.CloseKey;
     end;
end;

function  TFileAssoc.Refresh:Boolean;
begin
  Result:=Open;
  if Result then with RegIniFile do begin
     SOKFileAssociation:=ReadString(SOK_ASSOC_KEY,'','');
     XSBFileAssociation:=ReadString(XSB_ASSOC_KEY,'','');
     end;
end;

function  TFileAssoc.SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
begin
  Result:=True;
  IniFile.WriteString(FILE_ASSOC_SECTION,OLD_SOK_ASSOC_KEY,OldSOKFileAssociation);
  IniFile.WriteString(FILE_ASSOC_SECTION,OLD_XSB_ASSOC_KEY,OldXSBFileAssociation);
end;

function  TFileAssoc.UnAssociateFileTypes:Boolean;
begin
  Result:=Refresh;
  if Result then with RegIniFile do begin
     if SOKFileAssociation=YASC_ASSOC_KEY_VALUE then begin
        if   OldSOKFileAssociation<>SOK_ASSOC_KEY then
             WriteString(SOK_ASSOC_KEY,'',OldSOKFileAssociation)
        else //WriteString(SOK_ASSOC_KEY,'','');
             DeleteKey(SOK_ASSOC_KEY,'');
        end;

     if XSBFileAssociation=YASC_ASSOC_KEY_VALUE then begin
        if   OldXSBFileAssociation<>XSB_ASSOC_KEY then
             WriteString(XSB_ASSOC_KEY,'',OldXSBFileAssociation)
        else //WriteString(XSB_ASSOC_KEY,'','');
             DeleteKey(XSB_ASSOC_KEY,'');
        end;

     EraseSection(YASC_ASSOC_KEY_VALUE);
     CloseKey;

     SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
     end;
end;

var FileAssociations:TFileAssoc;

begin
  FileAssociations:=nil;
  if (ParamCount>=1) and (Length(ParamStr(1))>=2) then
     case UpCase(ParamStr(1)[2]) of
       'I': begin
              FileAssociations:=TFileAssoc.Create;
              FileAssociations.AssociateFileTypes;
            end;
       'U': begin
              FileAssociations:=TFileAssoc.Create;
              FileAssociations.UnAssociateFileTypes;
            end;
     end;
  if   Assigned(FileAssociations) then FileAssociations.Free
  else MessageBox(0,
                  'Usage:'+NL+
                  '"/install": Associate ".sok" and ".xsb" files with Sokoban YASC.'+NL+
                  '"/uninstall": Remove file associations with Sokoban YASC.',
                  'Sokoban YASC File Associations',
                  MB_OK+MB_ICONINFORMATION);
end.

