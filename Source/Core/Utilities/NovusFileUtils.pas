unit NovusFileUtils;

interface

uses StrUtils, NovusUtilities, Windows, SysUtils, Dialogs, SHFolder, ShellApi;

Type
  TNovusFileUtils = class(tNovusUtilities)
  public
    class function IsFileInUse(fName : string) : boolean;
    class function IsFileReadonly(fName : string) : boolean;
    class function MoveDir(aFromDirectory, aToDirectory: String): Boolean;
    class function CopyDir(aFromDirectory, aToDirectory: String): Boolean;
    class function AbsoluteFilePath(aFilename: String): String;
    class function TrailingBackSlash(const aFilename: string): string;
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

class function TNovusFileUtils.TrailingBackSlash(const aFilename: string): string;
begin
  Result := '';

  if Trim(aFilename) <> '' then
    Result := IncludeTrailingPathDelimiter(aFilename);
end;

end.
