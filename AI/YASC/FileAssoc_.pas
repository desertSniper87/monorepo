unit FileAssoc_; {File Associations}

// note that the module is outdated;
// the code only works on the pre-Vista Windows versions Win9x, WinNT, and WinXP

interface

uses IniFile_,Registry;

// the following constants must match the ones defined in the installation
// scripts "Sokoban.iss" and "Sokoban - no skins.iss";
//
const
  FILE_ASSOC_SECTION      = 'File Associations';
  FILE_ASSOC_PROGRAM_NAME = 'Sokoban YASC File Associations.exe';
  OLD_SOK_ASSOC_KEY       = 'Old SOK Association';
  OLD_XSB_ASSOC_KEY       = 'Old XSB Association';
  SOK_ASSOC_KEY           = '.sok';
  XSB_ASSOC_KEY           = '.xsb';
  YASC_ASSOC_KEY_VALUE    = 'Sokoban YASC (bd).Level';

type
  TFileAssoc              = class(TObject)
  private
    OldSOKFileAssociation : String;
    OldXSBFileAssociation : String;
    SOKFileAssociation    : String;
    XSBFileAssociation    : String;
    RegIniFile            : TRegIniFile;
    function InstallOrUninstallUsingExternalProgram(Install__:Boolean):Boolean;
  public
    constructor       Create;
    destructor        Destroy; override;

    function          AssociateFileTypes(UseExternalProgram__:Boolean):Boolean;
    function          Close:Boolean;
    function          HasFileAssociations:Boolean;
    function          LoadSettingsFromIniFile(const IniFile:TIniFile):Boolean;
    function          Open:Boolean;
    function          Refresh:Boolean;
    function          SaveSettingsToIniFile(const IniFile:TIniFile):Boolean;
    function          UnAssociateFileTypes(UseExternalProgram__:Boolean):Boolean;
  end;

implementation

uses SysUtils,ShellApi,ShlObj,Windows,Forms,SokUtil_,Misc_,Text_;

constructor TFileAssoc.Create;
begin
  RegIniFile:=nil;
  OldSOKFileAssociation:=SOK_ASSOC_KEY; // self-references means 'undefined' here
  OldXSBFileAssociation:=XSB_ASSOC_KEY;
  SOKFileAssociation   :='';
  XSBFileAssociation   :='';
end;

destructor TFileAssoc.Destroy;
begin
  Close;
end;

function  TFileAssoc.AssociateFileTypes(UseExternalProgram__:Boolean):Boolean;
begin
  Result:=Refresh;
  if Result then with RegIniFile do begin
     if (SOKFileAssociation<>YASC_ASSOC_KEY_VALUE) and
        (SOKFileAssociation<>'') then OldSOKFileAssociation:=SOKFileAssociation;
     if (XSBFileAssociation<>YASC_ASSOC_KEY_VALUE) and
        (XSBFileAssociation<>'') then OldXSBFileAssociation:=XSBFileAssociation;

     if UseExternalProgram__ then
        InstallOrUninstallUsingExternalProgram(True)
     else begin
        WriteString(SOK_ASSOC_KEY,'',YASC_ASSOC_KEY_VALUE);
        WriteString(XSB_ASSOC_KEY,'',YASC_ASSOC_KEY_VALUE);

        WriteString(YASC_ASSOC_KEY_VALUE,'','Sokoban Level');

        Result:=OpenKey(YASC_ASSOC_KEY_VALUE,True);
        if Result then WriteString('DefaultIcon','',Application.ExeName+',0');

        Result:=Result and
                OpenKey('shell',True) and
                OpenKey('open',True);
        if Result then WriteString('command','',Application.ExeName+' "%1"');

        CloseKey;

        SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
        end;
     end;
end;

function  TFileAssoc.Close:Boolean;
begin
  try    RegIniFile.Free; RegIniFile:=nil;
  except on E:Exception do Error(E.Message,Application.Title);
  end;
  Result:=RegIniFile=nil;
end;

function  TFileAssoc.HasFileAssociations:Boolean;
begin
  Result:=Refresh and
          (SOKFileAssociation=YASC_ASSOC_KEY_VALUE) and
          (XSBFileAssociation=YASC_ASSOC_KEY_VALUE);
end;

function TFileAssoc.InstallOrUninstallUsingExternalProgram(Install__:Boolean):Boolean;
const UNINSTALL_INSTALL_PARAMETER:array[Boolean] of String=('/uninstall','/install');
var   ApplicationPath,ExternalProgramName:String;
begin
  Result:=Refresh;
  if   Result then begin
       ApplicationPath:=StrWithoutTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
       ExternalProgramName:=StrWithTrailingPathDelimiter(ApplicationPath)+FILE_ASSOC_PROGRAM_NAME;
       if FileExists(ExternalProgramName) then begin
          if ShellApi.ShellExecute(0,'open',
                                   PChar(StrWithDoubleQuotes(ExternalProgramName)),
                                   PChar(UNINSTALL_INSTALL_PARAMETER[Install__]+SPACE+StrWithDoubleQuotes(OldSOKFileAssociation)+SPACE+StrWithDoubleQuotes(OldXSBFileAssociation)),
                                   PChar(ApplicationPath),
                                   SW_SHOWMINNOACTIVE)
             <=32 then begin {task failed} end;
          SleepEx(100,False);
          end
       else Msg(Format(FileAssociationProgramNotFoundText__,[StrWithDoubleQuotes(ExternalProgramName)]),Application.Title,MB_OK+MB_ICONINFORMATION);
       end;
  if   Install__ then Result:=HasFileAssociations
  else Result:=not HasFileAssociations;
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
  if RegIniFile=nil then
     try    RegIniFile:=TRegIniFile.Create('');
     except on E:Exception do begin
            RegIniFile.Free; RegIniFile:=nil;
            Error(E.Message,Application.Title);
            end;
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

function  TFileAssoc.UnAssociateFileTypes(UseExternalProgram__:Boolean):Boolean;
begin
  Result:=Refresh;
  if Result then with RegIniFile do begin
     if UseExternalProgram__ then
        InstallOrUninstallUsingExternalProgram(False)
     else begin
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
end;

end.
