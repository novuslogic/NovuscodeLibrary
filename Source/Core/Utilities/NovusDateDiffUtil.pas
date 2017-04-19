unit NovusDateDiffUtil;

interface

uses
  Sysutils;

Type
  EInvalidPeriod = Class(Exception);

Function DateDiff(Period: Word; Date2, Date1: TDatetime): Longint;

implementation

Function DateDiff(Period: Word; Date2, Date1: TDatetime): Longint;
Var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  Year1, Month1, Day1, Hour1, Min1, Sec1, MSec1: Word;
Begin
  DecodeDate(Date1, Year, Month, Day);
  DecodeDate(Date2, Year1, Month1, Day1);
  DecodeTime(Date1, Hour, Min, Sec, MSec);
  DecodeTime(Date2, Hour1, Min1, Sec1, MSec1);

  Case Period of
    1:
      Result := (((((Trunc(Date1) - Trunc(Date2)) * 86400) - ((Hour1 - Hour) *
        3600))) - ((Min1 - Min) * 60)) - (Sec1 - Sec);
    2:
      Result := (((Trunc(Date1) - Trunc(Date2)) * 1440) - ((Hour1 - Hour) * 60))
        - (Min1 - Min);
    3:
      Result := ((Trunc(Date1) - Trunc(Date2)) * 24) - (Hour1 - Hour);
    4:
      Result := Trunc(Date1) - Trunc(Date2);
    5:
      Result := (Trunc(Date1) - Trunc(Date2)) div 7;
    6:
      Result := ((Year - Year1) * 12) + (Month - Month1);
    7:
      Result := Year - Year1;
  Else
    Begin
      Raise EInvalidPeriod.Create('Invalid Period Assigned To DateDiff');
      Result := 0;
    end;
  End;
End;

end.
