{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusUtilities;

interface

uses System.SysUtils, Classes, typinfo, System.RegularExpressions, REST.JsonReflect,
     System.JSON, Data.DBXJSONReflect;

Const
  CR = #13#10;
  DosDelimSet: set of Char = ['\', ':', #0];
  StMaxFileLen = 260;

Type
  TNovusUtilities = class(TObject)
  public
    /// <summary>
    /// Return current exception
    /// </summary>
    class function GetExceptMess: String;
    class function GetLastSysErrorMess: string;

    class function CopyObject(Src, Dest: TObject; Related: Boolean = FALSE): Boolean;


    class procedure FreeObject(q: TObject);

    class function CloneObject(aObject: tObject): tObject;

    class function FindStringListValue(const Strings: tstringlist;
      Name: String): String;
    class function FindFileSize(Afile: String): Integer;
    class function GetPropertyasClass(aObject: TObject;
      aPropertyName: string): TObject;
    class function IsProperty(aObject: TObject; aPropertyName: string): Boolean;
    class function GetParamValue(const aParamKey: string;
      var aValue: string): Boolean;

    class procedure ClearStringList(aStringList: TStringList);
    class procedure CloneStringList(aSource, aDestination: TStringList);

    class function RegExMatchEx(aInput: string; aPattern: string;
      aInversed: Boolean; aMatchValue: Boolean; aIgnoreCase: boolean = true): String;
    class function RegExMatch(aInput: string; aPattern: string;
      aInversed: Boolean = FALSE): String;
  end;

implementation

class procedure TNovusUtilities.CloneStringList(aSource, aDestination: TStringList);
Var
  I: integer;
begin
  ClearStringList(aDestination);

  For i :=0 To Pred(aSource.Count) do
     aDestination.AddObject(aSource.Strings[I],CloneObject(aSource.Objects[I]));
end;

class procedure TNovusUtilities.ClearStringList(aStringList: TStringList);
var
  i: Integer;
begin
  for i := 0 to pred(aStringList.Count) do
   begin
     if Assigned(aStringList.Objects[i]) then
       begin
         aStringList.Objects[i].Free;
         aStringList.Objects[i] := Nil;
       end;
   end;

  aStringList.Clear;
end;

class procedure TNovusUtilities.FreeObject(q: TObject);
begin
  if Not Assigned(q) then
    Exit;

  System.SysUtils.FreeandNil(q);
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

  System.SysUtils.FindClose(SR);
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
  //ZeroMemory(@Result, Sizeof(Result));
  Result := '';

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
    //ZeroMemory(@fMatch, Sizeof(fMatch));
  End;

end;

class function TNovusUtilities.CloneObject(aObject: TObject): TObject;
var
  MarshalObj: TJSONMarshal;
  UnMarshalObj: TJSONUnMarshal;
  JSONValue: TJSONValue;
begin
  Result := nil;
  if not Assigned(AObject)  then Exit;

  MarshalObj := TJSONMarshal.Create;
  UnMarshalObj := TJSONUnMarshal.Create;
  try
    JSONValue := MarshalObj.Marshal(aObject);
    try
      if Assigned(JSONValue) then
        Result := UnMarshalObj.Unmarshal(JSONValue);
    finally
      JSONValue.Free;
    end;
  finally
    MarshalObj.Free;
    UnMarshalObj.Free;
  end;
end;


class function TNovusUtilities.GetExceptMess: string;
Var
  ValSize: Integer;
  P: Pointer;
  S: String;
begin
  result := '';

  If ExceptObject = NIL then
    Exit;

  ValSize := 255;

  P := AllocMem(ValSize);

  ExceptionErrorMessage(ExceptObject, ExceptAddr, P, ValSize);

{$IFDEF DELPHI2009_UP}
  S := StrPas(PWideChar(P));
{$ELSE}
  S := StrPas(P);
{$ENDIF}
  FreeMem(P);

  S := Copy(S, (Pos('.', S) + 1), Length(S) - Pos('.', S));
  result := Copy(S, (Pos('.', S) + 1), Length(S) - Pos('.', S));
end;


end.
