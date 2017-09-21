{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusFileUtils;

interface

uses StrUtils, NovusUtilities, Windows, SysUtils, SHFolder, ShellApi, ShlObj, Classes;

Type
  TNovusFileUtils = class(tNovusUtilities)
  public
    /// <summary>
    ///   Check if file is being used or locked.
    /// </summary>
    class function IsFileInUse(fName : string) : boolean;
    class function IsFileReadonly(fName : string) : boolean;
    class function MoveDir(aFromDirectory, aToDirectory: String): Boolean;
    class function CopyDir(aFromDirectory, aToDirectory: String): Boolean;
    /// <summary>
    ///   Extracts the extension part of a full file name without "."
    /// </summary>
    class function ExtractFileExtA(aFileExt: String): String;
    class function AbsoluteFilePath(aFilename: String): String;
     /// <summary>
    ///   Uses IncludeTrailingPathDelimiter, if filename is blank returns blank.
    /// </summary>
    class function TrailingBackSlash(const aFilename: string): string;
    class function GetSpecialFolder(const CSIDL: integer) : string;
    class function IsValidFolder(aFolder: String): Boolean;
    /// <summary>
    ///   Returns if true or false if a text file
    /// </summary>
{$IFDEF DELPHI2009_UP}
    class function IsTextFile(aFilename: String;var aEncoding: tEncoding): integer;
{$ENDIF}
  end;

  function PathCombine(lpszDest: PChar; const lpszDir, lpszFile: PChar):PChar; stdcall; external 'shlwapi.dll' name 'PathCombineA';

implementation


class function TNovusFileUtils.IsFileReadonly(fName : string) : boolean;
var
  HFileRes : HFILE;
  Res: string[6];

 function CheckAttributes(FileNam: string; CheckAttr: string): Boolean;
   var
     fa: Integer;
   begin
     fa := GetFileAttributes(PChar(FileNam)) ;
     Res := '';

     if (fa and FILE_ATTRIBUTE_NORMAL) <> 0 then
     begin
       Result := False;
       Exit;
     end;

     if (fa and FILE_ATTRIBUTE_ARCHIVE) <> 0 then  Res := Res + 'A';
     if (fa and FILE_ATTRIBUTE_COMPRESSED) <> 0 then  Res := Res + 'C';
     if (fa and FILE_ATTRIBUTE_DIRECTORY) <> 0 then  Res := Res + 'D';
     if (fa and FILE_ATTRIBUTE_HIDDEN) <> 0 then  Res := Res + 'H';
     if (fa and FILE_ATTRIBUTE_READONLY) <> 0 then  Res := Res + 'R';
     if (fa and FILE_ATTRIBUTE_SYSTEM) <> 0 then  Res := Res + 'S';

     Result := AnsiContainsText(Res, CheckAttr) ;
   end; (*CheckAttributes*)

   procedure SetAttr(fName: string) ;
   var
     Attr: Integer;
   begin
     Attr := 0;
     if AnsiContainsText(Res, 'A') then  Attr := Attr + FILE_ATTRIBUTE_ARCHIVE;
     if AnsiContainsText(Res, 'C') then  Attr := Attr + FILE_ATTRIBUTE_COMPRESSED;
     if AnsiContainsText(Res, 'D') then  Attr := Attr + FILE_ATTRIBUTE_DIRECTORY;
     if AnsiContainsText(Res, 'H') then  Attr := Attr + FILE_ATTRIBUTE_HIDDEN;
     if AnsiContainsText(Res, 'S') then  Attr := Attr + FILE_ATTRIBUTE_SYSTEM;

     SetFileAttributes(PChar(fName), Attr) ;
   end; (*SetAttr*)
 begin //IsFileInUse
   Result := False;

   if not FileExists(fName) then exit;

   Result := CheckAttributes(fName, 'R');
end;

class function TNovusFileUtils.IsFileInUse(fName : string) : boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(fName) then begin
    Exit;
  end;

  HFileRes := CreateFile(PChar(fName)
    ,GENERIC_READ or GENERIC_WRITE
    ,0
    ,nil
    ,OPEN_EXISTING
    ,FILE_ATTRIBUTE_NORMAL
    ,0);

  Result := (HFileRes = INVALID_HANDLE_VALUE);

  if not(Result) then begin
    CloseHandle(HFileRes);
  end;
end;


class function TNovusFileUtils.MoveDir(aFromDirectory, aToDirectory: String): Boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    wFunc  := FO_MOVE;
    fFlags := FOF_FILESONLY;
    pFrom  := PChar(aFromDirectory + #0);
    pTo    := PChar(aToDirectory)
  end;
  Result := (0 = ShFileOperation(fos));
end;


class function TNovusFileUtils.CopyDir(aFromDirectory, aToDirectory: String): Boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    wFunc  := FO_COPY;
    fFlags := FOF_FILESONLY;
    pFrom  := PChar(aFromDirectory + #0);
    pTo    := PChar(aToDirectory)
  end;
  Result := (0 = ShFileOperation(fos));
end;

class function TNovusFileUtils.AbsoluteFilePath(aFilename: String): String;
var
  lpFileName : pchar;
  lpBuffer : array[0..MAX_PATH] of char;
  cResult: Cardinal;
begin
  lpFileName := PCHAR(aFilename);
  cResult := GetFullPathName(lpFileName , MAX_PATH, lpBuffer, lpFileName );
  result := ExtractFilePath(lpBuffer);
end;

class function TNovusFileUtils.ExtractFileExtA(aFileExt: String): String;
begin
  Result := '';
  if Trim(Result) = '' then Exit;
  Result := Copy(ExtractFileExt(aFileExt), 2, Length(aFileExt));
end;

class function TNovusFileUtils.TrailingBackSlash(const aFilename: string): string;
begin
  Result := '';

  if Trim(aFilename) <> '' then
    Result := IncludeTrailingPathDelimiter(aFilename);
end;


class function TNovusFileUtils.GetSpecialFolder(const CSIDL: integer) : string;
var
  RecPath : PWideChar;
begin
  RecPath := StrAlloc(MAX_PATH);
    try
    FillChar(RecPath^, MAX_PATH, 0);
    if SHGetSpecialFolderPath(0, RecPath, CSIDL, false)
      then result := RecPath
      else result := '';
    finally
      StrDispose(RecPath);
    end;
end;

class function TNovusFileUtils.IsValidFolder(aFolder: String): Boolean;
var
  S: String;
  I: Integer;
begin
  Result := False;

  if FileExists(aFolder) then Exit;

  Result := DirectoryExists(aFolder);

  if Result = false then
    begin
      S := aFolder;
      repeat
        I := LastDelimiter('\/', S);
        MoveFile(nil, PChar(S));
        if (GetLastError = ERROR_ALREADY_EXISTS) or
           (
             (GetFileAttributes(PChar(Copy(S, I + 1, MaxInt))) = INVALID_FILE_ATTRIBUTES)
             and
             (GetLastError=ERROR_INVALID_NAME)
           ) then
          Exit;
        if I>0 then
          S := Copy(S,1,I-1);
      until I = 0;
      Result := True;
    end;
end;

{$IFDEF DELPHI2009_UP}
class function TNovusFileUtils.IsTextFile(aFilename: String;var aEncoding: TEncoding): integer;
var
  F: TFileStream;
  Buffer: array [0 .. 1023] of byte;
  FEncoding: TEncoding;
  ch: byte;
Const
  cNUL = 0; // Null char
  cBS = 8; // Back Space
  cCR = 13; // Carriage Return
  cSUB = 26; // Substitute

  function isControlChar(aCh: byte): boolean;
  begin
    Result := (((aCh > cNUL) and (aCh < cBS)) or
      ((aCh > cCR) and (aCh < cSUB)));

  end;

begin
  Result := -1;
  if not FileExists(aFilename) then
    Exit;

  Result := 0;

  Try
    F := TFileStream.Create(aFilename, fmOpenRead	or fmShareDenyWrite);

    if F.Position < F.Size then
    begin
      F.Read(Buffer, 1024);

      for ch in Buffer do
      begin
        if not isControlChar(ch) then
        begin
          if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
            FEncoding := TEncoding.UTF8
          else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
            FEncoding := TEncoding.Unicode
          else if (Buffer[0] = 0) and (Buffer[1] = 0) and (Buffer[2] = $FE) and
            (Buffer[3] = $FF) then
            FEncoding := TEncoding.BigEndianUnicode
          else if (Buffer[0] = $2B) and (Buffer[1] = $2F) and (Buffer[2] = $76)
          then
            FEncoding := TEncoding.UTF7
          else
            FEncoding := TEncoding.Default;

          aEncoding := FEncoding;

          Result := 1;

          break;
        end;
      end;
    end;

  Finally
    F.Free;
  End;
end;
{$ENDIF}

end.
