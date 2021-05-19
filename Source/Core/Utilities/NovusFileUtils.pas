{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusFileUtils;

interface

uses StrUtils, NovusUtilities, Windows, SysUtils, SHFolder, ShellApi, ShlObj,
  Classes, NovusWindows, ComObj, WinInet, ShLwApi, System.IOUtils;

Type

  TNovusFileUtils = class(tNovusUtilities)
  public
    /// <summary>
    /// Check if file is being used or locked.
    /// </summary>
    class function IsFileInUse(fName: string): boolean;
    /// <summary>
    /// Converts File path to a canonicalized URL.
    /// </summary>
    class function FilePathToURL(const aFilePath: string): string;
    class function IsFileReadonly(fName: string): boolean;
    class function MoveDir(aFromDirectory, aToDirectory: String): boolean;
    class function CopyDir(aFromDirectory, aToDirectory: String): boolean;
    class function ExtractName(aFullFileName: String): String;
    /// <summary>
    /// Extracts the extension part of a full file name without "."
    /// </summary>
    class function ExtractFileExtA(aFileExt: String): String;

    class function AbsoluteFilePath(aFilename: String): String;
    /// <summary>
    /// Uses IncludeTrailingPathDelimiter, if filename is blank returns blank.
    /// </summary>
    class function TrailingBackSlash(const aFilename: string): string;
    class function GetSpecialFolder(const CSIDL: integer): string;
    class function IsOnlyFolder(aFolder: string): boolean;

    /// <summary>
    ///  Is Valid folder
    /// </summary>
    class function IsValidFolder(aFolder: String): boolean;
    /// <summary>
    /// Returns the encoding of text file or not text file
    /// </summary>
    /// <remarks>
    /// <para>
    /// Result will return -1 if not text file
    /// </para>
    /// <para>
    /// 0 = TEncoding.UTF8 <br />1 = TEncoding.UTF7 <br />2 =
    /// TEncoding.Unicode <br />3 = TEncoding.Default <br />4 =
    /// TEncoding.BigEndianUnicode <br />5 = TEncoding.ASCII
    /// </para>
    /// </remarks>
{$IFDEF DELPHI2009_UP}
    class function IsTextFile(aFilename: String;
      var aEncoding: tEncoding): integer;
{$ENDIF}
  end;

function PathCombine(lpszDest: PChar; const lpszDir, lpszFile: PChar): PChar;
  stdcall; external 'shlwapi.dll' name 'PathCombineA';

implementation

class function TNovusFileUtils.IsFileReadonly(fName: string): boolean;
var
  HFileRes: HFILE;
  Res: string[6];

  function CheckAttributes(FileNam: string; CheckAttr: string): boolean;
  var
    fa: integer;
  begin
    fa := GetFileAttributes(PChar(FileNam));
    Res := '';

    if (fa and FILE_ATTRIBUTE_NORMAL) <> 0 then
    begin
      Result := False;
      Exit;
    end;

    if (fa and FILE_ATTRIBUTE_ARCHIVE) <> 0 then
      Res := Res + 'A';
    if (fa and FILE_ATTRIBUTE_COMPRESSED) <> 0 then
      Res := Res + 'C';
    if (fa and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
      Res := Res + 'D';
    if (fa and FILE_ATTRIBUTE_HIDDEN) <> 0 then
      Res := Res + 'H';
    if (fa and FILE_ATTRIBUTE_READONLY) <> 0 then
      Res := Res + 'R';
    if (fa and FILE_ATTRIBUTE_SYSTEM) <> 0 then
      Res := Res + 'S';

    Result := AnsiContainsText(Res, CheckAttr);
  end; (* CheckAttributes *)

  procedure SetAttr(fName: string);
  var
    Attr: integer;
  begin
    Attr := 0;
    if AnsiContainsText(Res, 'A') then
      Attr := Attr + FILE_ATTRIBUTE_ARCHIVE;
    if AnsiContainsText(Res, 'C') then
      Attr := Attr + FILE_ATTRIBUTE_COMPRESSED;
    if AnsiContainsText(Res, 'D') then
      Attr := Attr + FILE_ATTRIBUTE_DIRECTORY;
    if AnsiContainsText(Res, 'H') then
      Attr := Attr + FILE_ATTRIBUTE_HIDDEN;
    if AnsiContainsText(Res, 'S') then
      Attr := Attr + FILE_ATTRIBUTE_SYSTEM;

    SetFileAttributes(PChar(fName), Attr);
  end; (* SetAttr *)

begin // IsFileInUse
  Result := False;

  if not FileExists(fName) then
    Exit;

  Result := CheckAttributes(fName, 'R');
end;

class function TNovusFileUtils.IsFileInUse(fName: string): boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(fName) then
  begin
    Exit;
  end;

  HFileRes := CreateFile(PChar(fName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  Result := (HFileRes = INVALID_HANDLE_VALUE);

  if not(Result) then
  begin
    CloseHandle(HFileRes);
  end;
end;

class function TNovusFileUtils.MoveDir(aFromDirectory,
  aToDirectory: String): boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    wFunc := FO_MOVE;
    fFlags := FOF_FILESONLY;
    pFrom := PChar(aFromDirectory + #0);
    pTo := PChar(aToDirectory)
  end;
  Result := (0 = ShFileOperation(fos));
end;

class function TNovusFileUtils.CopyDir(aFromDirectory,
  aToDirectory: String): boolean;
var
  s: TSearchRec;
//  fos: TSHFileOpStruct;
  lsInDir , lsOutDir: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(aFromDirectory) + '*',faDirectory, s) = 0 then
  begin
    repeat
      if (s.Name <> '.') and (s.Name <> '..') and ((s.Attr and faDirectory) = faDirectory) then
      begin
        lsInDir := IncludeTrailingPathDelimiter(aFromDirectory) + s.Name;
        lsOutDir := IncludeTrailingPathDelimiter(aToDirectory) + s.Name;
        // Create new subdirectory in outDir
        mkdir(lsOutDir);
        // Recurse into subdirectory in inDir
        TNovusFileUtils.CopyDir(lsInDir,lsOutDir);
      end;
    until FindNext(s) <> 0;
  end;
  FindClose(s);



  (*
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    wFunc := FO_COPY;
    fFlags := FOF_FILESONLY;
    pFrom := PChar(aFromDirectory + #0);
    pTo := PChar(aToDirectory)
  end;
  Result := (0 = ShFileOperation(fos));
  *)



end;

class function TNovusFileUtils.AbsoluteFilePath(aFilename: String): String;
var
  lpFileName: PChar;
  lpBuffer: array [0 .. MAX_PATH] of char;
  cResult: Cardinal;
begin
  cResult := 0;

  lpFileName := PChar(aFilename);
  cResult := GetFullPathName(lpFileName, MAX_PATH, lpBuffer, lpFileName);
  Result := ExtractFilePath(lpBuffer);
end;

class function TNovusFileUtils.ExtractFileExtA(aFileExt: String): String;
begin
  Result := '';
  if Trim(aFileExt) = '' then
    Exit;
  Result := Copy(ExtractFileExt(aFileExt), 2, Length(ExtractFileExt(aFileExt)));
end;

class function TNovusFileUtils.TrailingBackSlash(const aFilename
  : string): string;
begin
  Result := '';

  if Trim(aFilename) <> '' then
    Result := IncludeTrailingPathDelimiter(aFilename);
end;

class function TNovusFileUtils.GetSpecialFolder(const CSIDL: integer): string;
var
  RecPath: PWideChar;
begin
  RecPath := StrAlloc(MAX_PATH);
  try
    FillChar(RecPath^, MAX_PATH, 0);
    if SHGetSpecialFolderPath(0, RecPath, CSIDL, False) then
      Result := RecPath
    else
      Result := '';
  finally
    StrDispose(RecPath);
  end;
end;

class function TNovusFileUtils.IsValidFolder(aFolder: String): boolean;
var
  S: String;
  I: integer;
begin
  Result := False;
  if not TNovusFileUtils.IsOnlyFolder(aFolder) then Exit;

  Result := DirectoryExists(aFolder);

  (*
  if Result = False then
  begin
    S := aFolder;
    repeat
      I := LastDelimiter('\/', S);
      MoveFile(nil, PChar(S));
      if (GetLastError = ERROR_ALREADY_EXISTS) or
        ((GetFileAttributes(PChar(Copy(S, I + 1, MaxInt)))
        = INVALID_FILE_ATTRIBUTES) and (GetLastError = ERROR_INVALID_NAME)) then
        Exit;
      if I > 0 then
        S := Copy(S, 1, I - 1);
    until I = 0;
    Result := True;
  end;
  *)
end;

{$IFDEF DELPHI2009_UP}

class function TNovusFileUtils.IsTextFile(aFilename: String;
  var aEncoding: tEncoding): integer;
var
  F: TFileStream;
  Buffer: TBytes;
  FEncoding: tEncoding;
  FSize: integer;
begin
  Result := -1;
  if not FileExists(aFilename) then
    Exit;

  try
    FEncoding := NIL;
    F := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyWrite);

    if F.Position < F.Size then
    begin
      FSize := F.Size;
      if FSize >= 1024 then
        FSize := 1024;

      SetLength(Buffer, FSize);
      F.ReadBuffer(Pointer(Buffer)^, Length(Buffer));

      FEncoding := NIL;

      Result := tEncoding.GetBufferEncoding(Buffer, FEncoding);
    end;

  Finally
    aEncoding := FEncoding;
    F.Free;
  End;
end;
{$ENDIF}

class function TNovusFileUtils.FilePathToURL(const aFilePath: string): string;
var
  BufferLen: DWORD;
begin
  Try
    BufferLen := INTERNET_MAX_URL_LENGTH;
    SetLength(Result, BufferLen);
    UrlCreateFromPath(PChar(aFilePath), PChar(Result), @BufferLen, 0);
    SetLength(Result, BufferLen);
  Except
    raise Exception.Create(tNovusUtilities.GetExceptMess);
  End;
end;

class function TNovusFileUtils.IsOnlyFolder(aFolder: string): boolean;
var
  lsTmpFolder: string;
  lsTmpFilename: string;
begin
  Result := False;

  aFolder := Trim(aFolder);

  if aFolder = '' then
    Exit;

  lsTmpFolder := ExtractFilePath(aFolder);
  lsTmpFilename := ExtractFilename(aFolder);

  Result := ((lsTmpFilename = '') and (lsTmpFolder <> ''));
end;

class function TNovusFileUtils.ExtractName(aFullFileName: String): String;
begin
  Result := stringreplace(extractfilename(aFullFileName), ExtractFileExt(aFullFileName), '', [rfIgnoreCase]);
end;

end.
