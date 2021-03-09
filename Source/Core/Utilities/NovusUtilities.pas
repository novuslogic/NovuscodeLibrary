{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusUtilities;

interface

uses SysUtils, Classes, Windows, Messages, typinfo, System.RegularExpressions;

Const
  CR = #13#10;
  DosDelimSet: set of Char = ['\', ':', #0];
  StMaxFileLen = 260;

  WM_CLOSEWINDOW = WM_USER + 1;
  WM_UPDATE = WM_USER + 1;

Type
  TNovusUtilities = class(TObject)
  public
    class function GetLastSysErrorMess: string;
    class function CopyObject(Src, Dest: TObject;
      Related: Boolean = FALSE): Boolean;
    class function AppRootDirectory: String;
    class function GetExceptMess: String;
    class procedure FreeObject(q: TObject);
    class function FindStringListValue(const Strings: tstringlist;
      Name: String): String;
    class function FindFileSize(Afile: String): Integer;
    class function GetPropertyasClass(aObject: TObject;
      aPropertyName: string): TObject;
    class function IsProperty(aObject: TObject; aPropertyName: string): Boolean;
    class function GetParamValue(const aParamKey: string;
      var aValue: string): Boolean;

    class function RegExMatchEx(aInput: string; aPattern: string;
      aInversed: Boolean; aMatchValue: Boolean; aIgnoreCase: boolean = true): String;
    class function RegExMatch(aInput: string; aPattern: string;
      aInversed: Boolean = FALSE): String;
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

class procedure TNovusUtilities.FreeObject(q: TObject);
begin
  if Not Assigned(q) then
    Exit;

  SysUtils.FreeandNil(q);
end;

class function TNovusUtilities.FindStringListValue(const Strings: tstringlist;
  Name: String): String;
Var
  I: Integer;
begin
  Result := '';
  For I := 0 to Strings.Count - 1 do
  begin
    If Uppercase(Strings.Names[I]) = Uppercase(Name) then
    begin
      Result := Strings.ValueFromIndex[I];
      Break;
    end;
  end;
end;

class function TNovusUtilities.FindFileSize(Afile: String): Integer;
var
  SR: TSearchRec;
  R: Integer;
begin
  Result := 0;

  R := FindFirst(Afile, faAnyFile, SR);
  if R = 0 then
    Result := SR.Size;

  SysUtils.FindClose(SR);
end;

class function TNovusUtilities.CopyObject(Src, Dest: TObject;
  Related: Boolean = FALSE): Boolean;
var
  Mem: Pointer;
  Size: Integer;
begin
  if (NOT Assigned(Src)) OR (NOT Assigned(Dest)) then
  begin
    Result := FALSE;
    Exit;
  end;

  if (NOT Related) or (Src.InheritsFrom(Dest.ClassType)) OR
    (Dest.InheritsFrom(Src.ClassType)) then
  begin
    if Src.InstanceSize < Dest.InstanceSize then
      Size := Src.InstanceSize
    else
      Size := Dest.InstanceSize;
    Dec(Size, 4);
    GetMem(Mem, Size);
    if Size < Dest.InstanceSize then
      System.FillChar(PByteArray(Dest)^[4], Size, 0);
    System.Move(PByteArray(Src)^[4], PByteArray(Dest)^[4], Size);
    FreeMem(Mem);
    Result := TRUE;
  end
  else
    Result := FALSE;
end;

class function TNovusUtilities.GetPropertyasClass(aObject: TObject;
  aPropertyName: string): TObject;
var
  I, Count: Integer;
  PropList: PPropList;
begin
  Result := nil;
  if aObject.Classinfo = nil then
    Exit;
  Count := GetTypeData(aObject.Classinfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * Sizeof(Pointer));
    try
      GetPropInfos(aObject.Classinfo, PropList);
      for I := 0 to Count - 1 do
        if ansicomparetext(PropList^[I].Name, aPropertyName) = 0 then
        begin
          case PropList^[I].PropType^.Kind of
            tkClass:
              Result := TObject(GetOrdProp(aObject, PropList^[I]));
          end;
          Break;
        end;
    finally
      FreeMem(PropList, Count * Sizeof(Pointer));
    end;
  end;
end;

class function TNovusUtilities.IsProperty(aObject: TObject;
  aPropertyName: string): Boolean;
var
  I, Count: Integer;
  PropList: PPropList;
begin
  Result := FALSE;
  Count := GetTypeData(aObject.Classinfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * Sizeof(Pointer));
    try
      GetPropInfos(aObject.Classinfo, PropList);
      for I := 0 to Count - 1 do
{$IFDEF win32}
        if ansicomparetext(PropList^[I].Name, aPropertyName) = 0 then
{$ELSE}
        if ansicomparetext(PropList^[I]^.Name, aPropertyName) = 0 then
{$ENDIF}
        begin
          Result := TRUE;
          Break;
        end;
    finally
      FreeMem(PropList, Count * Sizeof(Pointer));
    end;
  end;
end;

class function TNovusUtilities.GetParamValue(const aParamKey: string;
  var aValue: string): Boolean;
var
  lparamloop: Integer;
  liPos: Integer;
begin
  aValue := '';

  Result := FALSE;

  if paramcount > 0 then
    for lparamloop := 1 to paramcount do
    begin
      liPos := pos(lowercase(aParamKey), lowercase(ParamStr(lparamloop)));
      if (liPos > 0) then
      begin
        Result := TRUE;
        liPos := liPos + Length(aParamKey);
        aValue := System.Copy(ParamStr(lparamloop), liPos,
          Length(ParamStr(lparamloop)));
        Break;
      end;
    end;

end;

class function TNovusUtilities.GetLastSysErrorMess: string;
begin
  Result := SysErrorMessage(GetLastError);
end;

class function TNovusUtilities.RegExMatch(aInput: string; aPattern: string;
  aInversed: Boolean = False): String;
begin
  Result := RegExMatchEx(aInput,aPattern,aInversed, false);
end;

class function TNovusUtilities.RegExMatchEx(aInput: string; aPattern: string;
      aInversed: Boolean; aMatchValue: Boolean; aIgnoreCase: boolean = true): String;
var
  fMatch: tMatch;
  fGroup: tGroup;
begin
  ZeroMemory(@Result, Sizeof(Result));


  if aIgnoreCase then
    begin
      if not TRegEx.IsMatch(aInput, aPattern, [roIgnoreCase]) then
        begin
          Result := aInput;

          Exit;
        end;
    end
  else
    begin
      if not TRegEx.IsMatch(aInput, aPattern) then
        begin
          Result := aInput;

          Exit;
        end;
    end;

  Try
    if aIgnoreCase then
      fMatch := TRegEx.Match(aInput, aPattern, [roIgnoreCase])
    else
      fMatch := TRegEx.Match(aInput, aPattern);

    if aMatchValue = false then
      begin
        if fMatch.Index <= (fMatch.Groups.Count - 1) then
          Result := fMatch.Groups.Item[fMatch.Index].Value
        else
          Result := fMatch.Value;
      end
    else
      Result := fMatch.Value;

    if aInversed then
      begin
        if aIgnoreCase then
          Result := StringReplace(aInput, Result, '', [rfReplaceAll, rfIgnoreCase])
        else
          Result := StringReplace(aInput, Result, '', [rfReplaceAll]);
      end;

  Finally
    ZeroMemory(@fMatch, Sizeof(fMatch));
  End;

end;

end.
