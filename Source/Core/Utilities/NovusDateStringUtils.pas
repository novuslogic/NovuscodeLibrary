unit NovusDateStringUtils;

interface

Uses NovusDateUtils, SysUtils, NovusStringUtils,
  System.RegularExpressions, DateUtils;

type
  TNovusDateStringUtils = class(tNovusDateUtils)
  private
  protected
  public
    class function FormatedMinutesBetween(aStart: tDateTime;
      aEnd: tDateTime): String;
    class function JSONDateStr2UnixTime(aJSONDateString: String): Int64;
    class function UnixTimeToJSONDate(aUnixTime: Int64): String;
  end;

implementation

class function TNovusDateStringUtils.FormatedMinutesBetween(aStart: tDateTime;
  aEnd: tDateTime): String;
var
  M: Int64;
begin
  M := MinutesBetween(aEnd, aStart);
  Result := Format('%2.2d:%2.2d', [M div 60, M mod 60]);
end;

(*
  "\"\\/Date(1335205592410)\\/\""         .NET JavaScriptSerializer
  "\"\\/Date(1335205592410-0500)\\/\""    .NET DataContractJsonSerializer
  "2012-04-23T18:25:43.511Z"              JavaScript built-in JSON object
  "2012-04-21T18:25:43-05:00"             ISO 8601
*)

class function TNovusDateStringUtils.UnixTimeToJSONDate
  (aUnixTime: Int64): String;
begin
  Result := Format('/Date(%d)/', [aUnixTime]);
end;

class function TNovusDateStringUtils.JSONDateStr2UnixTime(aJSONDateString
  : String): Int64;
var
  regexpr: TRegEx;
  match: TMatch;
begin
  Try
    Result := 0;

    aJSONDateString := Trim(aJSONDateString);
    regexpr := TRegEx.Create('\d+', []);
    match := regexpr.match(aJSONDateString);

    if match.Success then
      Result := TNovusStringUtils.StrToUInt64(match.Value);

  Except
    Result := 0;

  End;
end;

end.
