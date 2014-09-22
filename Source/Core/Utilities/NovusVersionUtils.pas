unit NovusVersionUtils;

interface

uses Windows, sysutils, Forms, NovusUtilities;

type
  { Translation record }
  TTranslation = record
    { Language identifier }
    Language,
      { Character-set identifier }
    CharSet: Word;
  end;

  PTranslations = ^TTranslations;
  TTranslations = array[0..65535] of TTranslation;

  TNovusVersionUtils = class(TNovusUtilities)
    class function GetFixedFileInfo(FileInfo: Pointer): PVSFixedFileInfo;
    class function GetFullVersionNumber: string;
    class function GetReleaseNumber: String;
    class function GetMagMinVersionNumber: String;
    class function CreateFileInfo(aFileName: String): Pointer;
    class procedure FreeFileInfo(FileInfo: Pointer);
    class function GetProductName: String;
    class function GetLegalCopyright: String;
    class function GetTranslationCount(FileInfo: Pointer): UINT;
    class function GetTranslation(FileInfo: Pointer; i: UINT): TTranslation;
    class function GetFileInfoString(FileInfo: Pointer; Translation: TTranslation; StringName: string): string;
    class function GetBuildNumber: String;
  end;

implementation


{ Return pointer to fixed file version info }
class function TNovusVersionUtils.GetFixedFileInfo(FileInfo: Pointer): PVSFixedFileInfo;
var
  Len: UINT;
begin
  if not VerQueryValue(FileInfo, '\', Pointer(Result), Len) then
    raise Exception.Create('Fixed file info not available');
end;

class function TNovusVersionUtils.GetBuildNumber: String;
var
  FileInfo: Pointer;
begin
  FileInfo := CreateFileInfo(Application.ExeName);

  Result := Format('%d', [LoWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

  FreeFileInfo(FileInfo);
end;

class function TNovusVersionUtils.GetFullVersionNumber;
var
  FileInfo: Pointer;
begin
  FileInfo := CreateFileInfo(Application.ExeName);

  Result := Format('%d.%d.%d.%d', [
      HiWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
      LoWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
      HiWord(GetFixedFileInfo(FileInfo).dwFileVersionLS),
      LoWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

  FreeFileInfo(FileInfo);
end;

class function TNovusVersionUtils.GetReleaseNumber;
var
  FileInfo: Pointer;
begin
  FileInfo := CreateFileInfo(Application.ExeName);

  Result := Format('%d', [
      HiWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

  FreeFileInfo(FileInfo);
end;

class function TNovusVersionUtils.GetMagMinVersionNumber;
var
  FileInfo: Pointer;
begin
  FileInfo := CreateFileInfo(Application.ExeName);

  Result := Format('%d.%d.%d', [
      HiWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
      LoWord(GetFixedFileInfo(FileInfo).dwFileVersionMS),
      HiWord(GetFixedFileInfo(FileInfo).dwFileVersionLS),
      LoWord(GetFixedFileInfo(FileInfo).dwFileVersionLS)]);

  FreeFileInfo(FileInfo);
end;

{ Return pointer to file version info block }
class function TNovusVersionUtils.CreateFileInfo(aFileName: String): Pointer;
var
  lpVerInfo: pointer;
  rVerValue: PVSFixedFileInfo;
  dwInfoSize: cardinal;
  dwValueSize: cardinal;
  dwDummy: cardinal;
  lpstrPath: pchar;
begin
  lpstrPath := pchar(aFilename);

  dwInfoSize := GetFileVersionInfoSize(lpstrPath, dwDummy);

  if dwInfoSize = 0 then
  begin
    Result := 0;
    Exit;
  end;

  GetMem(lpVerInfo, dwInfoSize);
  GetFileVersionInfo(lpstrPath, 0, dwInfoSize, lpVerInfo);
  VerQueryValue(lpVerInfo, '\', pointer(rVerValue), dwValueSize);

  Result := lpVerInfo;

  //FreeMem(lpVerInfo, dwInfoSize);




(*
  { Get file version info block size }
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  { If size is valid }
  if Size > 0 then
  begin
    { Allocate memory for file version info block }
    GetMem(Result, Size);
    { Get file version info block }
    if not GetFileVersionInfo(PChar(FileName), Handle, Size, Result) then
    begin
      { Free allocated memory if failed }
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
    *)
end;

{ Free file version info block memory }
class procedure TNovusVersionUtils.FreeFileInfo(FileInfo: Pointer);
begin
  { Free allocated memory }
  if FileInfo <> nil then
    FreeMem(FileInfo);
end;

class function TNovusVersionUtils.GetLegalCopyright;
var
  FileInfo: Pointer;
  Translation: TTranslation;
begin
  FileInfo := CreateFileInfo(Application.ExeName);

  result := '';

  if GetTranslationCount(FileInfo) > 0 then
    begin
      Translation := GetTranslation(FileInfo, 0);
      Result := GetFileInfoString(FileInfo, Translation, 'LegalCopyright');
    end;

  FreeFileInfo(FileInfo);
end;

class function TNovusVersionUtils.GetProductName;
var
  FileInfo: Pointer;
  Translation: TTranslation;
begin
  FileInfo := CreateFileInfo(Application.ExeName);

  result := '';

  if GetTranslationCount(FileInfo) > 0 then
    begin
      Translation := GetTranslation(FileInfo, 0);
      Result := GetFileInfoString(FileInfo, Translation, 'ProductName');
    end;

  FreeFileInfo(FileInfo);
end;

{ Return number of available file version info translations }
class function TNovusVersionUtils.GetTranslationCount(FileInfo: Pointer): UINT;
var
  P: PTranslations;
  Len: UINT;
begin
  if not VerQueryValue(FileInfo, '\VarFileInfo\Translation', Pointer(P),
Len)
    then
    raise Exception.Create('File info translations not available');
  Result := Len div 4;
end;

{ Return i-th translation in the file version info translation list }
class function TNovusVersionUtils.GetTranslation(FileInfo: Pointer; i: UINT): TTranslation;
var
  P: PTranslations;
  Len: UINT;
begin
  if not VerQueryValue(FileInfo, '\VarFileInfo\Translation', Pointer(P),
Len)
    then
    raise Exception.Create('File info translations not available');
  if i * SizeOf(TTranslation) >= Len then
    raise Exception.Create('Specified translation not available');
  Result := P[i];
end;

{ Return the value of the specified file version info string using the
specified translation }
class function TNovusVersionUtils.GetFileInfoString(FileInfo: Pointer; Translation: TTranslation;
StringName: string):
  string;
var
  P: PChar;
  Len: UINT;
begin
  if not VerQueryValue(FileInfo, PChar('\StringFileInfo\' +
    IntToHex(Translation.Language, 4) +
    IntToHex(Translation.CharSet, 4) +
    '\' + StringName), Pointer(P), Len) then
    raise Exception.Create('Specified file info string not available');
  SetString(Result, P, Len);
end;

end.
