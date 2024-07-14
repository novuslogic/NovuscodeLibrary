{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusJSONUtils;

interface

Uses NovusUtilities, Data.DBXJSON, SysUtils, JSON, NovusDateUtils,
  NovusDateStringUtils,
  NovusStringUtils, RegularExpressions;

Type
  tNovusJSONUtils = class(tNovusUtilities)
  private
  protected
  public
    class function GetJSONArrayValue(AJSONArray: TJSONArray; const key: string): TJSONArray;
    class function GetJSONObjectValue(AJSONObject: TJSONObject; const key: string): TJSONObject;
    class function GetJSONStringValue(jsonObj: TJSONObject; const key: string): string;
    class function InJSONArray(const aElement: string;
      const aJSONArray: TJSONArray): TJSONPair;
    class function IsNullorBlank(const aValue: String): Boolean;
    class function InJSONArrayToDate(const aElement: string;
      const aJSONArray: TJSONArray): tDateTime;
    class function InJSONArrayToInt64(const aElement: string;
      const aJSONArray: TJSONArray): Int64;
    class function InJSONArrayToString(const aElement: string;
      const aJSONArray: TJSONArray): String;
    class function InJSONArrayToCurrency(const aElement: string;
      const aJSONArray: TJSONArray): Currency;
    class function InJSONArrayToInteger(const aElement: string;
        const aJSONArray: TJSONArray): Integer;
    class function InJSONArrayToDateTime(const aElement: string;
        const aJSONArray: TJSONArray): TDateTime;
    class function InJSONArrayToBoolean(const aElement: string;
        const aJSONArray: TJSONArray): Boolean;
    class function InJSONArrayToDouble(const aElement: string;
        const aJSONArray: TJSONArray): Double;
  end;

implementation

class function tNovusJSONUtils.InJSONArray(const aElement: string;
  const aJSONArray: TJSONArray): TJSONPair;
Var
  I: Integer;
begin
  Result := NIL;

  if Not Assigned(aJSONArray) then
    Exit;

  for I := 0 to aJSONArray.Size - 1 do
  begin
    if Trim(Uppercase(TJSONPair(aJSONArray.Get(I)).JSONString.Value))
      = Trim(Uppercase(aElement)) then
    begin
      Result := TJSONPair(aJSONArray.Get(I));

      Break;
    end;
  end;
end;

class function tNovusJSONUtils.IsNullorBlank(const aValue: String): Boolean;
begin
  Result := ((Lowercase(Trim(aValue)) = 'null') or (Trim(aValue) = ''));
end;

class function tNovusJSONUtils.InJSONArrayToDate(const aElement: string;
  const aJSONArray: TJSONArray): tDateTime;
begin
  Result := 0;

  if InJSONArray(aElement, aJSONArray) <> NIL then
     Result := TNovusDateUtils.JSONDateToDatetime(InJSONArray(aElement, aJSONArray)
      .JsonValue.ToString);
end;

class function tNovusJSONUtils.InJSONArrayToInt64(const aElement: string;
  const aJSONArray: TJSONArray): Int64;
begin
  Result := 0;
  if InJSONArray(aElement, aJSONArray) <> NIL then
    Result := TNovusStringUtils.Str2Int64(InJSONArray(aElement, aJSONArray)
      .JsonValue.Value);

end;

class function tNovusJSONUtils.InJSONArrayToInteger(const aElement: string;
  const aJSONArray: TJSONArray): Integer;
begin
  Result := 0;
  if InJSONArray(aElement, aJSONArray) <> NIL then
    Result := TNovusStringUtils.Str2Int(InJSONArray(aElement, aJSONArray)
      .JsonValue.Value);

end;

class function tNovusJSONUtils.InJSONArrayToString(const aElement: string;
  const aJSONArray: TJSONArray): String;
begin
  Result := '';
  if InJSONArray(aElement, aJSONArray) <> NIL then
    Result := InJSONArray(aElement, aJSONArray).JsonValue.Value;
end;

class function tNovusJSONUtils.InJSONArrayToCurrency(const aElement: string;
  const aJSONArray: TJSONArray): Currency;
begin
  Result := 0;
  if InJSONArray(aElement, aJSONArray) <> NIL then
    Result := TNovusStringUtils.Str2Curr(InJSONArray(aElement, aJSONArray)
      .JsonValue.Value);
end;

class function tNovusJSONUtils.InJSONArrayToDateTime(const aElement: string;
        const aJSONArray: TJSONArray): TDateTime;
begin
  Result := 0;
  if InJSONArray(aElement, aJSONArray) <> NIL then
    Result := TNovusStringUtils.Str2DateTime(InJSONArray(aElement, aJSONArray)
      .JsonValue.Value);
end;

class function tNovusJSONUtils.InJSONArrayToBoolean(const aElement: string;
        const aJSONArray: TJSONArray): Boolean;
begin
  Result := false;
  if InJSONArray(aElement, aJSONArray) <> NIL then
    Result := TNovusStringUtils.StrToBoolean(InJSONArray(aElement, aJSONArray)
      .JsonValue.Value);
end;

class function tNovusJSONUtils.InJSONArrayToDouble(const aElement: string;
        const aJSONArray: TJSONArray): Double;
begin
   Result := 0;
  if InJSONArray(aElement, aJSONArray) <> NIL then
    Result := TNovusStringUtils.Str2Float(InJSONArray(aElement, aJSONArray)
      .JsonValue.Value);
end;

class function tNovusJSONUtils.GetJSONStringValue(jsonObj: TJSONObject; const key: string): string;
begin
  Result := '';
  if Not Assigned(jsonObj) then Exit;

  try
    Result := jsonObj.GetValue<string>(key);
  except
    Result := '';
  end;
end;

class function tNovusJSONUtils.GetJSONObjectValue(AJSONObject: TJSONObject; const key: string): TJSONObject;
begin
  Result := Nil;
  if Not Assigned(AJSONObject) then Exit;
  try
    Result := AJSONObject.GetValue<TJSONObject>(key);
  except
    Result := Nil;
  end;
end;


class function tNovusJSONUtils.GetJSONArrayValue(AJSONArray: TJSONArray; const key: string): TJSONArray;
begin
  Result := Nil;
  if Not Assigned(AJSONArray) then Exit;
  try
    Result := AJSONArray.GetValue<TJSONArray>(key);
  except
    Result := Nil;
  end;
end;


end.

