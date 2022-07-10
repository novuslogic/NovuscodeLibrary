{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusWinVersionUtils;

interface

uses sysutils, Windows, Forms, NovusUtilities;

type
  TTranslation = record
    Language, CharSet: Word;
  end;

  TNovusWinVersionUtils = class(TNovusUtilities)
    class function GetFixedFileInfo(FileInfo: Pointer): PVSFixedFileInfo;
    class function GetFullVersionNumber(aFileName: String = ''): string;
    class function GetReleaseNumber(aFileName: String = ''): String;
    class function GetMagMinVersionNumber(aFileName: String = ''): String;
    class function CreateFileInfo(aFileName: String): Pointer;
    class procedure FreeFileInfo(FileInfo: Pointer);
    class function GetProductName(aFileName: String = ''): String;
    class function GetLegalCopyright(aFileName: String = ''): String;
    class function GetTranslationCount(FileInfo: Pointer): UINT;
    class function GetTranslation(FileInfo: Pointer; i: UINT): TTranslation;
    class function GetFileInfoString(FileInfo: Pointer;
      Translation: TTranslation; StringName: string): string;
    class function GetBuildNumber(aFilename: string=''): String;
  end;

  PTranslations = ^TTranslations;
  TTranslations = array [0 .. 65535] of TTranslation;

implementation

class function TNovusWinVersionUtils.GetFixedFileInfo(FileInfo: Pointer)
  : PVSFixedFileInfo;
var
  Len: UINT;
begin
  if not VerQueryValue(FileInfo, '\', Pointer(Result), Len) then
    raise Exception.Create('Fixed file info not available');
end;

class function TNovusWinVersionUtils.GetBuildNumber(aFilename: string = ''): String;
var
  FileInfo: Pointer;
begin
  Result := '';

  FileInfo := CreateFileInfo(aFilename);

  if FileInfo = NIL then Exit;

  Result := Format('%d', [LoWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

  FreeFileInfo(FileInfo);
end;

class function TNovusWinVersionUtils.GetFullVersionNumber(aFileName: String = ''): string;
var
  FileInfo: Pointer;
begin
  Result := '';

  FileInfo := CreateFileInfo(aFilename);

  if FileInfo = NIL then Exit;

  Result := Format('%d.%d.%d.%d',
        [HiWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
        LoWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
        HiWord(GetFixedFileInfo(FileInfo).dwFileVersionLS),
        LoWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

      FreeFileInfo(FileInfo);
end;

class function TNovusWinVersionUtils.GetReleaseNumber(aFilename: string=''): string;
var
  FileInfo: Pointer;
begin
  Result := '';

  FileInfo := CreateFileInfo(aFilename);

  if FileInfo = NIL then Exit;

  Result := Format('%d', [HiWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

  FreeFileInfo(FileInfo);
end;

class function TNovusWinVersionUtils.GetMagMinVersionNumber(aFilename: string=''): string;
var
  FileInfo: Pointer;
begin
  FileInfo := CreateFileInfo(aFilename);

  Result := '';

  if FileInfo = NIL then Exit;

  Result := Format('%d.%d.%d',
    [HiWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
    LoWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
    HiWord(GetFixedFileInfo(FileInfo).dwFileVersionLS),
    LoWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

  FreeFileInfo(FileInfo);
end;

class function TNovusWinVersionUtils.CreateFileInfo(aFileName: String): Pointer;
var
  lpVerInfo: Pointer;
  rVerValue: PVSFixedFileInfo;
  dwInfoSize: cardinal;
  dwValueSize: cardinal;
  dwDummy: cardinal;
  lpstrPath: pchar;
begin
  if Trim(aFilename) = '' then
    aFilename := Application.ExeName;

  lpstrPath := pchar(aFileName);

  //dwInfoSize := GetFileVersionInfoSize(lpstrPath, dwDummy);
  dwInfoSize := GetFileVersionInfoSizeW(lpstrPath, dwDummy);

  if dwInfoSize = 0 then
  begin
    Result := 0;
    Exit;
  end;

  GetMem(lpVerInfo, dwInfoSize);
  GetFileVersionInfo(lpstrPath, 0, dwInfoSize, lpVerInfo);
  VerQueryValue(lpVerInfo, '\', Pointer(rVerValue), dwValueSize);

  Result := lpVerInfo;
end;

class procedure TNovusWinVersionUtils.FreeFileInfo(FileInfo: Pointer);
begin
  if FileInfo <> nil then
    FreeMem(FileInfo);
end;

class function TNovusWinVersionUtils.GetLegalCopyright(aFileName: String = ''): string;
var
  FileInfo: Pointer;
  Translation: TTranslation;
begin
  FileInfo := CreateFileInfo(aFileName);

  Result := '';

  if FileInfo = NIL then Exit;

  if GetTranslationCount(FileInfo) > 0 then
  begin
    Translation := GetTranslation(FileInfo, 0);
    Result := GetFileInfoString(FileInfo, Translation, 'LegalCopyright');
  end;

  FreeFileInfo(FileInfo);
end;

class function TNovusWinVersionUtils.GetProductName(aFileName: String = ''): string;
var
  FileInfo: Pointer;
  Translation: TTranslation;
begin
  FileInfo := CreateFileInfo(aFileName);

  Result := '';

  if FileInfo = NIL then Exit;

  if GetTranslationCount(FileInfo) > 0 then
  begin
    Translation := GetTranslation(FileInfo, 0);
    Result := GetFileInfoString(FileInfo, Translation, 'ProductName');
  end;

  FreeFileInfo(FileInfo);
end;

class function TNovusWinVersionUtils.GetTranslationCount(FileInfo: Pointer): UINT;
var
  P: PTranslations;
  Len: UINT;
begin
  if not VerQueryValue(FileInfo, '\VarFileInfo\Translation', Pointer(P), Len)
  then
    raise Exception.Create('File info translations not available');
  Result := Len div 4;
end;

class function TNovusWinVersionUtils.GetTranslation(FileInfo: Pointer; i: UINT)
  : TTranslation;
var
  P: PTranslations;
  Len: UINT;
begin
  if not VerQueryValue(FileInfo, '\VarFileInfo\Translation', Pointer(P), Len)
  then
    raise Exception.Create('File info translations not available');
  if i * SizeOf(TTranslation) >= Len then
    raise Exception.Create('Specified translation not available');
  Result := P[i];
end;

class function TNovusWinVersionUtils.GetFileInfoString(FileInfo: Pointer;
  Translation: TTranslation; StringName: string): string;
var
  P: pchar;
  Len: UINT;
begin
  Result := '';

  if not VerQueryValue(FileInfo,
    pchar('\StringFileInfo\' + IntToHex(Translation.Language,
    4) + IntToHex(Translation.CharSet, 4) + '\' + StringName), Pointer(P), Len)
  then
    raise Exception.Create('Specified file info string not available');
  SetString(Result, P, Len);

  Result := Trim(Result);
end;

end.
