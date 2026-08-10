{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusDateStringUtils;

interface

uses
  SysUtils,
  DateUtils,
  NovusDateUtils,
  NovusStringUtils
  {$IFNDEF FPC}
  , System.RegularExpressions
  {$ENDIF}
  ;

type
  TNovusDateStringUtils = class(TNovusDateUtils)
  private
  protected
  public
    class function FormatedMinutesBetween(aStart: TDateTime;
      aEnd: TDateTime): String;

    class function JSONDateStr2UnixTime(aJSONDateString: String): Int64;

    class function UnixTimeToJSONDate(aUnixTime: Int64): String;
  end;

implementation

class function TNovusDateStringUtils.FormatedMinutesBetween(
  aStart: TDateTime;
  aEnd: TDateTime): String;
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

class function TNovusDateStringUtils.UnixTimeToJSONDate(
  aUnixTime: Int64): String;
begin
  Result := Format('/Date(%d)/', [aUnixTime]);
end;

class function TNovusDateStringUtils.JSONDateStr2UnixTime(
  aJSONDateString: String): Int64;

{$IFDEF FPC}
  function ExtractFirstNumber(const S: String): String;
  var
    I: Integer;
    Started: Boolean;
  begin
    Result := '';
    Started := False;

    for I := 1 to Length(S) do
    begin
      if S[I] in ['0'..'9'] then
      begin
        Result := Result + S[I];
        Started := True;
      end
      else if Started then
        Break;
    end;
  end;
{$ENDIF}

{$IFNDEF FPC}
var
  RegExpr: TRegEx;
  Match: TMatch;
{$ENDIF}

var
  LValue: String;

begin
  try
    Result := 0;

    aJSONDateString := Trim(aJSONDateString);

    {$IFDEF FPC}
    LValue := ExtractFirstNumber(aJSONDateString);
    {$ELSE}
    RegExpr := TRegEx.Create('\d+', []);
    Match := RegExpr.Match(aJSONDateString);

    if Match.Success then
      LValue := Match.Value
    else
      LValue := '';
    {$ENDIF}

    if LValue <> '' then
      Result := TNovusStringUtils.StrToUInt64(LValue);

  except
    Result := 0;
  end;
end;

end.