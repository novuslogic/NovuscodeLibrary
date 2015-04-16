unit NovusDateStringUtils;

interface

Uses NovusDateUtils, SysUtils, Controls, NovusStringUtils, System.RegularExpressions ;

type
  TNovusDateSrtingUtils = class(tNovusDateUtils)
  private
  protected
  public
    class function InterStrToDate(sDateParam : String): TDate;
    class function GetDateOrder: String ;
    class function JSONDateStr2UnixTime(aJSONDateString: String): Int64;
    class function UnixTimeToJSONDate(aUnixTime: Int64): String;
  end;

implementation

class function TNovusDateSrtingUtils.InterStrToDate(sDateParam : String): TDate;
var sMask : String;
    bDayFirstMonth,
    bMonthFirstYear : Boolean;
    iDay,
    iMonth,
    iYear
    : Integer;
    iDel : array[0..2] of Integer;
    I ,
    J: Integer;
    DateOrder : String;
begin
  J := 0;
  sMask :=  UpperCase(InternationalDate(True));

  try

      if (Pos('MMM', sMask) = 0) and (Length(sDateParam)=Length(sMask))then
      begin
        Result := StrToDate(sDateParam);
        Exit;
      end;

      DateOrder:= GetDateOrder ;

      for I := 1 to Length(sDateParam) do
      begin
        if (sDateParam[I] = FormatSettings.DateSeparator) then
        begin
          iDel[J] := I;
          J := J + 1;
        end;
      end;


      if DateOrder='DMY'{bDayFirstMonth} then
      begin
        iDay := StrToInt(Copy(sDateParam,1,iDel[0]-1));

        iMonth := GetIntMonth(Copy(sDateParam,iDel[0]+1,iDel[1]-iDel[0]-1));

        iYear := StrToInt(Copy(sDateParam,iDel[1]+1,100));
      end
      else if DateOrder='MDY'{bMonthFirstYear} then
      begin
        iMonth := GetIntMonth(Copy(sDateParam,1,iDel[0]-1));

        iDay := StrToInt(Copy(sDateParam,iDel[0]+1,iDel[1]-iDel[0]-1));

        iYear := StrToInt(Copy(sDateParam,iDel[1]+1,100));
      end
      else
      begin
        iYear := StrToInt(Copy(sDateParam,1,iDel[0]-1));

        iMonth := GetIntMonth(Copy(sDateParam,iDel[0]+1,iDel[1]-iDel[0]-1));

        iDay := StrToInt(Copy(sDateParam,iDel[1]+1,100));
      end;

      if (iYear < 100) then
        iYear := 2000 + iYear;

      Result :=(EncodeDate(iYear,iMonth,iDay));

  except

      Result:=0;

  end;

end;

class function TNovusDateSrtingUtils.GetDateOrder: String;
var
   ShortDate : String;
begin
  Result:='';

  ShortDate := UpperCase(InternationalDate(False));

  if  Pos('D', ShortDate ) < Pos('M', ShortDate)  then
   begin
     If Pos('Y', ShortDate) < Pos('D',ShortDate) then
        Result:='YDM'
     else If Pos('Y', ShortDate) < Pos('M', ShortDate) then
        Result:='DYM'
     else
        Result:='DMY';
   end
  else
   begin
    If Pos('Y', ShortDate) > Pos('D',ShortDate) then
       Result:='MDY'
    else If Pos('Y', ShortDate) > Pos('M', ShortDate) then
       Result:='MYD'
    else
       Result:='YMD'
   end;

end;

(*
"\"\\/Date(1335205592410)\\/\""         .NET JavaScriptSerializer
"\"\\/Date(1335205592410-0500)\\/\""    .NET DataContractJsonSerializer
"2012-04-23T18:25:43.511Z"              JavaScript built-in JSON object
"2012-04-21T18:25:43-05:00"             ISO 8601
*)

class function TNovusDateSrtingUtils.UnixTimeToJSONDate(aUnixTime: Int64): String;
begin
  Result := Format('/Date(%d)/', [aUnixTime]);
end;

class function TNovusDateSrtingUtils.JSONDateStr2UnixTime(aJSONDateString: String): Int64;
var
  regexpr : TRegEx;
  match   : TMatch;
begin
  Try
    Result := 0;

    aJSONDateString := Trim(aJSONDateString);
    regexpr := TRegEx.Create('\d+',[]);
    match := regexpr.Match(aJSONDateString);

    if match.Success then
      Result := TNovusStringUtils.StrToUInt64(match.Value);

  Except
    Result := 0;

  End;
end;


end.

