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
  Result := TNovusDateUtils.JSONDateToDatetime(InJSONArray(aElement, aJSONArray)
    .JsonValue.ToString);
end;

class function tNovusJSONUtils.InJSONArrayToInt64(const aElement: string;
  const aJSONArray: TJSONArray): Int64;
begin
  Result := TNovusStringUtils.Str2Int64(InJSONArray(aElement, aJSONArray)
    .JsonValue.Value);
end;

class function tNovusJSONUtils.InJSONArrayToString(const aElement: string;
  const aJSONArray: TJSONArray): String;
begin
  Result := InJSONArray(aElement, aJSONArray).JsonValue.Value;
end;

class function tNovusJSONUtils.InJSONArrayToCurrency(const aElement: string;
  const aJSONArray: TJSONArray): Currency;
begin
  Result := TNovusStringUtils.Str2Curr(InJSONArray(aElement, aJSONArray)
    .JsonValue.Value);
end;

end.
