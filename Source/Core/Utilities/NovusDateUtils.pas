unit NovusDateUtils;

interface

Uses NovusStringUtils, NovusUtilities, SysUtils, Classes, Windows, XSBuiltIns,
  System.DateUtils;

Type
  Months = array [1 .. 12] of Word;

Const
  UnixStartDate = 25569.0;
  iMinYIndex = 1;
  iMaxYIndex = 10;
  TWOZERO = '20';
  ONENINE = '19';

  MonthsArray: array [1 .. 12] of String = ('JAN', 'FEB', 'MAR', 'APR', 'MAY',
    'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
  MTHDAYS: Months = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  Last2DigYearArray: array [iMinYIndex .. iMaxYIndex] of String = ('00', '01',
    '02', '03', '04', '05', '06', '07', '08', '09');

  MonthOnly = 'm';
  DayOnly = 'd';
  YearOnly = 'y';

type
  TNovusDateUtils = class(tNovusUtilities)
  private
  protected
  public
    class function IsValidYear(sStr: String): Boolean;
    class function IsValidMonth(iMonth: Integer): Boolean;
    class function isLeapYear(year: Integer): Boolean;
    class function GetIntMonth(sStr: String): Integer;
    class function GetIntYear(sStr: String; iPos: Integer): Integer;
    class function UnixTimeToDateTime(const aUnixDate: Int64): TDateTime;
    class function DateTimeToISO8601(const aDateTime: TDateTime): string;
    class function JSONDateToDatetime(aJSONDate: string): TDateTime;
  end;

implementation

class function TNovusDateUtils.GetIntMonth(sStr: String): Integer;
var
  i, j: Integer;
  sUppStr: String;
begin
  sUppStr := UpperCase(Trim(sStr));
  if TNovusStringUtils.IsNumeric(sUppStr) then
  begin
    TNovusStringUtils.Str2LongS(sUppStr, Result);
    If (Result < 1) and (Result > 12) then
      Result := -1;
    Exit;
  end;

  j := Length(sUppStr);
  if (j >= 1) and (j <= 2) then
  begin
    if (sUppStr = 'JA') then
      sUppStr := 'JAN'
    else if (sUppStr = 'F') or (sUppStr = 'FE') then
      sUppStr := 'FEB'
    else if (sUppStr = 'MR') then
      sUppStr := 'MAR'
    else if (sUppStr = 'AP') then
      sUppStr := 'APR'
    else if (sUppStr = 'MY') then
      sUppStr := 'MAY'
    else if (sUppStr = 'AU') then
      sUppStr := 'AUG'
    else if (sUppStr = 'S') or (sUppStr = 'SE') then
      sUppStr := 'SEP'
    else if (sUppStr = 'O') or (sUppStr = 'OC') then
      sUppStr := 'OCT'
    else if (sUppStr = 'N') or (sUppStr = 'NO') then
      sUppStr := 'NOV'
    else if (sUppStr = 'D') or (sUppStr = 'DE') then
      sUppStr := 'DEC';
  end;

  for i := 1 to 12 do
  begin
    if (Pos(MonthsArray[i], sUppStr) > 0) then
    begin
      Result := i;
      Break;
    end
    else
      Result := -1;
  end;
end;

class function TNovusDateUtils.GetIntYear(sStr: String; iPos: Integer): Integer;
var
  i, iIndex: Integer;
  sYear, sCenYear: String;
begin
  iIndex := 0;

  for i := iPos to Length(Trim(sStr)) do
  begin
    if (sStr[i] in ['0' .. '9']) then
    begin
      iIndex := i;
      Break;
    end;
  end;
  sYear := Copy(sStr, iIndex, (Length(sStr) - iIndex) + 1);
  if IsValidYear(sYear) then
  begin
    sCenYear := TWOZERO + sYear;
    sYear := '';
    sYear := sCenYear;
  end;
  if TNovusStringUtils.IsIntStr(sYear) then
    Result := StrToInt(sYear)
  else
    Result := -1;
end;

class function TNovusDateUtils.IsValidYear(sStr: String): Boolean;
var
  i: Integer;
begin
  for i := iMinYIndex to iMaxYIndex do
  begin
    if Last2DigYearArray[i] = Trim(sStr) then
    begin
      IsValidYear := True;
      Break;
    end
    else
      IsValidYear := False;
  end;
end;

class function TNovusDateUtils.IsValidMonth(iMonth: Integer): Boolean;
var
  i: Integer;
begin
  for i := 1 to 12 do
  begin
    if iMonth in [1 .. 12] then
    begin
      IsValidMonth := True;
      Break;
    end
    else
      IsValidMonth := False;
  end;

end;

class function TNovusDateUtils.isLeapYear(year: Integer): Boolean;
begin
  if year mod 4 = 0 then
    isLeapYear := True
  else
    isLeapYear := False;
end;

class function TNovusDateUtils.UnixTimeToDateTime(const aUnixDate: Int64)
  : TDateTime;
var
  UTCTime, LocalTime: TSystemTime;
begin
  Result := 0;

  if aUnixDate <> 0 then
  begin
    FileTimeToSystemTime(TFileTime(Int64(aUnixDate + 11644473600000) *
      10000), UTCTime);
    SystemTimeToTzSpecificLocalTime(nil, UTCTime, LocalTime);
    Result := SystemTimeToDateTime(LocalTime);
  end
end;

class function TNovusDateUtils.JSONDateToDatetime(aJSONDate: string): TDateTime;
var
  fwYear, fwMonth, fwDay, fwHour, fwMinute, fwSecond, fwMillisecond: Word;
  lsJSONValue: String;
begin
  Try
    lsJSONValue := Trim(TNovusStringUtils.StripChar(aJSONDate, #34 (* " *) ));

    fwYear := TNovusStringUtils.Str2Int(Copy(lsJSONValue, 1, 4));
    fwMonth := TNovusStringUtils.Str2Int(Copy(lsJSONValue, 6, 2));
    fwDay := TNovusStringUtils.Str2Int(Copy(lsJSONValue, 9, 2));
    fwHour := TNovusStringUtils.Str2Int(Copy(lsJSONValue, 12, 2));
    fwMinute := TNovusStringUtils.Str2Int(Copy(lsJSONValue, 15, 2));

    fwSecond := TNovusStringUtils.Str2Int(Copy(TNovusStringUtils.StripChar(lsJSONValue, 'Z'), 18, 2));

    fwMillisecond :=
      Round(TNovusStringUtils.Str2Float(Copy(lsJSONValue, 19, 4)));

    Result := EncodeDateTime(fwYear, fwMonth, fwDay, fwHour, fwMinute, fwSecond,
      fwMillisecond);
  Except
    Result := 0;
  End;
end;

class function TNovusDateUtils.DateTimeToISO8601(const aDateTime
  : TDateTime): string;
Var
  D: TXSDateTime;
  year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  FDateTime: TDateTime;
begin
  Result := '';

  if aDateTime <> 0 then
  begin
    DecodeDate(aDateTime, year, Month, Day);
    DecodeTime(aDateTime, Hour, Min, Sec, MSec);

    FDateTime := EncodeDateTime(year, Month, Day, Hour, Min, Sec, MSec);

    D := TXSDateTime.Create;
    D.AsDateTime := FDateTime;

    Result := D.NativeToXS;

    D.Free;
  end;
end;

end.
