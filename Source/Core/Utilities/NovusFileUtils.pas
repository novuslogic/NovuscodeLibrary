unit NovusFileUtils;

interface

uses StrUtils, NovusUtilities, Windows, SysUtils, Dialogs, SHFolder;

Type
  TNovusFileUtils = class(tNovusUtilities)
  public
    class function IsFileInUse(fName : string) : boolean;
    class function GetSpecialFolderPath(afolder : Integer) : string;
  end;


implementation

(*
0 = [Current User]\My Documents  - CSIDL_PERSONAL;
1 = All Users\Application Data - CSIDL_COMMON_APPDATA
2 = [User Specific]\Application Data - CSIDL_LOCAL_APPDATA;
3 = Program Files - CSIDL_PROGRAM_FILES;
4 = All Users\Documents - CSIDL_COMMON_DOCUMENTS;
*)

class function TNovusFileUtils.GetSpecialFolderPath(afolder : Integer) : string;
const
   SHGFP_TYPE_CURRENT = 0;
 var
   path: array [0..MAX_PATH] of char;
 begin
   if SUCCEEDED(SHGetFolderPath(0, afolder,0,SHGFP_TYPE_CURRENT,@path[0])) then
     Result := path
   else
     Result := '';
 end;

class function TNovusFileUtils.IsFileInUse(fName : string) : boolean;
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


end.
