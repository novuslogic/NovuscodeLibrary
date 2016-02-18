unit NovusUtilities;

interface

uses SysUtils, Classes, Windows, Messages, typinfo,
     Graphics;


Const
  DosDelimSet  : set of Char = ['\', ':', #0];
  StMaxFileLen  = 260;

  WM_CLOSEWINDOW = WM_USER + 1;
  WM_UPDATE = WM_USER + 1;


Type
  TNovusUtilities = class(TObject)
  public
     class function TwipsPerPixelX(Canvas : TCanvas) : Extended;
     class function TwipsPerPixelY(Canvas : TCanvas) : Extended;
     class function CopyObject(Src,Dest : TObject; Related : Boolean = FALSE): Boolean;
     class function AppRootDirectory: String;
     class function GetExceptMess: String;
     class procedure FreeObject(q : TObject);
     class function FindStringListValue(const Strings: tstringlist; Name: String): String;
     class function FindFileSize(Afile: String): Integer;
     class function JustFilename(const PathName : String) : String;
     class function JustPathname(const PathName : ShortString) : String;
     class function GetPropertyasClass(aObject: TObject; aPropertyName: string): TObject;
     class function IsProperty(aObject: TObject; aPropertyName: string): boolean;
     class function GetParamValue(const aParamKey : string; var aValue : string ) : boolean;
  end;

implementation

Uses NovusWindows;

class function TNovusUtilities.AppRootDirectory;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

class function TNovusUtilities.GetExceptMess;
begin
  Result := TNovusWindows.WindowsExceptMess;
end;

class procedure TNovusUtilities.FreeObject(Q : TObject);
begin
  if Not Assigned(Q) then Exit;

  SysUtils.FreeandNil(Q);
end;

class function TNovusUtilities.FindStringListValue(const Strings: tstringlist; Name: String): String;
Var
  I: integer;
begin
  Result := '';
  For I := 0 to Strings.Count -1 do
    begin
      If Uppercase(Strings.Names[i]) = Uppercase(Name) then
        begin
          Result := Strings.ValueFromIndex[i];
          Break;
        end;
    end;
end;

class function TNovusUtilities.FindFileSize(Afile: String): Integer;
var
  SR : TSearchRec;
  R  : Integer;
begin
  R := FindFirst(Afile, faAnyFile, SR);
  if R = 0 then Result := SR.Size;

  SysUtils.FindClose(SR);
end;


class function TNovusUtilities.JustFilename(const PathName : String) : String;
var
  I : Longint;
begin
  Result := '';
  if PathName = '' then
    Exit;
  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);
  Result := Copy(PathName, Succ(I), StMaxFileLen);
end;

class function TNovusUtilities.JustPathname(const PathName : ShortString) : String;
Var
  I: Integer;
begin
  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}

  if I = 0 then
    {Had no drive or directory name}
    SetLength(Result, 0)
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = '\') then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := Copy(PathName, 1, I);
end;

class function TNovusUtilities.CopyObject(Src,Dest : TObject; Related : Boolean = FALSE): Boolean;
var Mem  : Pointer;
    Size : Integer;
begin
{ Source or Dest = NIL -> Exit }
if(NOT Assigned(Src))OR
   (NOT Assigned(Dest)) then
  begin
   Result := FALSE;
   Exit;
  end;
{ Copy }
if(NOT Related)OR { If Related = FALSE, copy object regardless of
inheritance }
   (Src.InheritsFrom(Dest.ClassType))OR     { Src descends from Dest }
   (Dest.InheritsFrom(Src.ClassType)) then { Dest descends from Src }
  begin
   if Src.InstanceSize < Dest.InstanceSize then
    Size := Src.InstanceSize else
    Size := Dest.InstanceSize; { Get the needed memory amount }
   Dec(Size,4); { Virtual method table occupies the first 4 bytes - leave it
alone!!! }
   GetMem(Mem,Size);
   if Size < Dest.InstanceSize then
    System.FillChar(PByteArray(Dest)^[4],Size,0);
   System.Move(PByteArray(Src)^[4],PByteArray(Dest)^[4],Size);
   FreeMem(Mem);
   Result := TRUE;
  end else
   Result := FALSE;
end;

class function TNovusUtilities.GetPropertyasClass(aObject: TObject; aPropertyName: string): TObject;
var
  i, Count: integer;
  PropList: PPropList;
begin
  Result := nil;
  if aObject.Classinfo = nil then Exit;
  Count := GetTypeData(aObject.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * Sizeof(Pointer));
    try
      GetPropInfos(aObject.ClassInfo, PropList);
      for i := 0 to Count - 1 do
        if ansicomparetext(PropList^[i].Name, aPropertyName) = 0 then
        begin
          case PropList^[i].PropType^.Kind of
            tkClass: Result := TObject(GetOrdProp(aObject, PropList^[i]));
          end;
          Break;
        end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;
end;

class function TNovusUtilities.IsProperty(aObject: TObject; aPropertyName: string): boolean;
var
  i, Count: integer;
  PropList: PPropList;
begin
  Result := false;
  Count := GetTypeData(aObject.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * Sizeof(Pointer));
    try
      GetPropInfos(aObject.ClassInfo, PropList);
      for i := 0 to Count - 1 do
{$IFDEF win32}
        if ansicomparetext(PropList^[i].Name, aPropertyName) = 0 then
{$ELSE}
        if ansicomparetext(PropList^[i]^.Name, aPropertyName) = 0 then
{$ENDIF}
        begin
          Result := true;
          Break;
        end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;
end;


class function TNovusUtilities.TwipsPerPixelX(Canvas : TCanvas) : Extended;
begin
  result := (1440 / GetDeviceCaps(Canvas.Handle, LOGPIXELSX));
end;

class function TNovusUtilities.TwipsPerPixelY(Canvas : TCanvas) : Extended;
begin
  result := (1440 / GetDeviceCaps(Canvas.Handle, LOGPIXELSY));
end;

 class function TNovusUtilities.GetParamValue(const aParamKey : string; var aValue : string ) : boolean;
var
  lparamloop : integer;
  i: Integer;
  liPos : integer;
begin
  aValue := '';

  if paramcount > 0 then
    for lparamloop := 1 to paramcount do
    begin
      liPos := pos(lowercase(aParamKey), lowercase(paramstr(lparamloop)) );
      if (liPos > 0) then
      begin
        result := True;
        liPos := liPos+length(aParamKey);
        aValue := system.copy( paramstr(lparamloop), liPos, length( paramstr(lparamloop) ) );
        break;
      end;
    end;

end;


end.




