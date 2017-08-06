{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusStringUtils;

interface

uses Windows, sysutils, NovusUtilities, Classes, variants;

Const
  Cr = #13;
  ToUpperChars: array [0 .. 255] of Char = (#$00, #$01, #$02, #$03, #$04, #$05,
    #$06, #$07, #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F, #$10, #$11,
    #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B, #$1C, #$1D,
    #$1E, #$1F, #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29,
    #$2A, #$2B, #$2C, #$2D, #$2E, #$2F, #$30, #$31, #$32, #$33, #$34, #$35,
    #$36, #$37, #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F, #$40, #$41,
    #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D,
    #$4E, #$4F, #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59,
    #$5A, #$5B, #$5C, #$5D, #$5E, #$5F, #$60, #$41, #$42, #$43, #$44, #$45,
    #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F, #$50, #$51,
    #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$7B, #$7C, #$7D,
    #$7E, #$7F, #$80, #$81, #$82, #$81, #$84, #$85, #$86, #$87, #$88, #$89,
    #$8A, #$8B, #$8C, #$8D, #$8E, #$8F, #$80, #$91, #$92, #$93, #$94, #$95,
    #$96, #$97, #$98, #$99, #$8A, #$9B, #$8C, #$8D, #$8E, #$8F, #$A0, #$A1,
    #$A1, #$A3, #$A4, #$A5, #$A6, #$A7, #$A8, #$A9, #$AA, #$AB, #$AC, #$AD,
    #$AE, #$AF, #$B0, #$B1, #$B2, #$B2, #$A5, #$B5, #$B6, #$B7, #$A8, #$B9,
    #$AA, #$BB, #$A3, #$BD, #$BD, #$AF, #$C0, #$C1, #$C2, #$C3, #$C4, #$C5,
    #$C6, #$C7, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF, #$D0, #$D1,
    #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD,
    #$DE, #$DF, #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9,
    #$CA, #$CB, #$CC, #$CD, #$CE, #$CF, #$D0, #$D1, #$D2, #$D3, #$D4, #$D5,
    #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF);

Type
  WStrRec = packed record
    Length: Longint;
  end;

const
  StrOffset = SizeOf(WStrRec);

Type
  LStrRec = record
    AllocSize: Longint;
    RefCount: Longint;
    Length: Longint;
  end;

Type
  TNovusStringUtils = class(TNovusUtilities)
  public
    class function FormatMessStrOptions(aString: String;
      aFormatOptions: Integer): String;
    class function MemoryStreamToString(Stream: TMemoryStream): string;
    class function Str2Float(aStr: String): single;
    class function Str2Curr(aStr: String): Currency;
    class function Replicate(c: Char; iLen: Integer): string;
    class function PadLeft(const s: string; iLen: Integer;
      const sFillChar: Char): string;
    class function JustFilename(const aPathName: String): String;
    class function JustPathname(const aPathName: String): String;
    class function Str2DateTime(s: String): tDateTime;
    class procedure ValLongInt(s: ShortString; var LI: Longint;
      var ErrorCode: Integer);
    class function Str2LongS(const s: ShortString; var I: Longint): Boolean;
    class function IsNumber(AChar: Char): Boolean;
    class function IsAlpha(AChar: Char): Boolean;
    class function IsNumberStr(s: String): Boolean;
    class function IsAlphaStr(s: String): Boolean;
    class procedure GetNames(AText: string; AList: TStringList);
    class function SubstCharSim(P: string; OC, NC: ANSIChar): string;
    /// <summary>
    /// Receives application root path.
    /// </summary>
    class function RootDirectory: String;
    class function StripChar(s: String; Ch: Char): string;
    class function ReplaceChar(s: String; aFromCh, aToCh: Char): String;
    /// <summary>
    ///   Simple token string parser
    /// </summary>
    class function GetStrToken(const s: string; sTokens: array of string;
      var iPos: Integer; var sLastToken: String): string;
    class function GetStrTokenA(const s, sDelim: string;
      var iPos: Integer): string;

    class function GetStrRes(const Index: Integer): String;
    /// <summary>
    /// Convert a String to Integer
    /// </summary>
    /// <remarks>
    /// if the string is blank then a exception will not be raised but will
    /// converted to a zero.
    /// </remarks>
    class function Str2Int(aStr: String): Integer;
    class function Str2Int64(aStr: String): Int64;
    class function StrChInsertL(const s: AnsiString; c: ANSIChar; Pos: Cardinal)
      : AnsiString;
    class function IsIntStr(s: String): Boolean;
    class function MakeTmpFileName(AExt: string;
      AUseGUID: Boolean = false): String;
    class function ReplaceStrPos(const sString, sOldStr, sNewStr: string;
      APos: Integer = -1; AUseBlank: Boolean = false): string;
    class function IsLowerChar(Ch: Char): Boolean;
    class function IsUpperChar(Ch: Char): Boolean;
    class function UpLower(AName: String; FirstOnly: Boolean): String;
    class function UpLowerA(AName: String; FirstOnly: Boolean): String;
    class function BooleanToStr(bValue: Boolean): string;
    class function StrToBoolean(const sValue: string): Boolean;
    class function ReplaceStr(Const sString, sOldStr, sNewStr: string;
      ACheckUpper: Boolean = false): string;

    class procedure ParseStringList(sDelimeter: string; sStr: string;
      Var Lines: TStringList); overload;
    /// <summary>
    /// Convert a string into a StringList by a Delimeter char
    /// </summary>
    class procedure ParseStringList(aDelimeter: Char; sStr: string;
      Var Lines: TStringList); overload;
    class procedure String2StringList(aString: string;
      var AStringList: TStringList);
    class procedure String2Strings(aString: string; var AStrings: TStrings);
    class function FormatMessStr(aString: String): String;
//    class function TrailingBackSlash(const aFilename: string): string;
    class function VarArrayToStr(const vArray: variant): string;
    class function VarStrNull(const V: OleVariant): string;
    class function IsBoolean(const sValue: string): Boolean;
    class function StrToUInt64(const s: String): UInt64;
  end;

implementation

class function TNovusStringUtils.StripChar(s: String; Ch: Char): string;
Var
  fsStr: String;
  I: Integer;
begin
  fsStr := '';
  for I := 1 to Length(s) do
    If s[I] <> Ch then
      fsStr := fsStr + s[I];

  result := fsStr;
end;

class function TNovusStringUtils.ReplaceChar(s: String;
  aFromCh, aToCh: Char): String;
Var
  fsStr: String;
  I: Integer;
begin
  fsStr := '';
  for I := 1 to Length(s) do
    If s[I] = aFromCh then
      fsStr := fsStr + aToCh
    else
      fsStr := fsStr + s[I];

  result := fsStr;
end;

class function TNovusStringUtils.GetStrRes;
var
  buffer: array [0 .. 255] of Char;
  ls: Integer;
begin
  result := '';
  ls := LoadString(hInstance, Index, buffer, SizeOf(buffer));
  if ls <> 0 then
    result := buffer;

end;

class function TNovusStringUtils.RootDirectory;
begin
  result := ExtractFilePath(ParamStr(0));
end;

class function TNovusStringUtils.Str2Int;
begin
  result := 0;

  aStr := StripChar(aStr, #$D);
  aStr := StripChar(aStr, #$A);

  Try
    If Trim(aStr) = '' then
      Exit;

    result := sysutils.StrtoInt(aStr);
  Except
    result := 0;
  end;
end;

class function TNovusStringUtils.Str2Int64;
begin
  result := 0;

  aStr := StripChar(aStr, #$D);
  aStr := StripChar(aStr, #$A);

  Try
    If Trim(aStr) = '' then
      Exit;

    result := sysutils.StrtoInt64(aStr);
  Except
    result := 0;
  end;
end;

class function TNovusStringUtils.ReplaceStrPos;
var
  iPos: Integer; { position of the old string }
  iLenOld: Integer; { length of the old string }
begin
  result := sString;
  iLenOld := Length(sOldStr);

  if (AUseBlank = True) and (sNewStr = '') then
  begin
    result := '';

    Exit;
  end;

  if (sNewStr <> sOldStr) then
  begin
    if iPos <> -1 then
      iPos := Pos(sOldStr, result)
    else
      iPos := APos;

    while (iPos > 0) do
    begin
      Delete(result, iPos, iLenOld);

      if (sNewStr <> '') then
        Insert(sNewStr, result, iPos);

      iPos := Pos(sOldStr, result);
    end; { while }
  end; { if }
end;

class procedure TNovusStringUtils.GetNames(AText: string; AList: TStringList);
var
  P: Integer;
  s: string;
begin
  AList.Clear;
  repeat
    AText := Trim(AText);
    P := Pos('="', AText);
    if P > 0 then
    begin
      s := Copy(AText, 1, P - 1);
      AList.Add(s);
      Delete(AText, 1, P + 1);
      P := Pos('"', AText);
      if P > 0 then
        Delete(AText, 1, P);
    end;
  until P = 0;
end;

class function TNovusStringUtils.StrChInsertL(const s: AnsiString; c: ANSIChar;
  Pos: Cardinal): AnsiString;
var
  Temp: string;
begin
  SetLength(Temp, 1);
  // {$IFNDEF VER200}
  // Temp[1] := C;
  // {$ELSE}
  Temp := c;
  // {$ENDIF}

  result := s;
  System.Insert(Temp, result, Pos);
end;

class function TNovusStringUtils.SubstCharSim(P: string;
  OC, NC: ANSIChar): string;
var
  step: Integer;
  lOC: ANSIChar;
begin
  for step := 1 to Length(P) do
  begin
    lOC := #0;

    // {$IFNDEF VER200}
    // if P[step] = OC then
    // P[step] := NC;
    // {$ELSE}
    lOC := ANSIChar(P[step]);
    if (lOC = OC) then
      P[step] := Char(NC);
    // {$ENDIF}
  end;
  result := P;
end;

class function TNovusStringUtils.IsNumber;
begin
  result := false;

  If AChar in ['0' .. '9', '.', #8, '-'] then
    result := True
end;

class function TNovusStringUtils.IsAlpha;
begin
  result := false;

  If AChar in ['A' .. 'Z', 'a' .. 'z'] then
    result := True
end;

class procedure TNovusStringUtils.ValLongInt(s: ShortString; var LI: Longint;
  var ErrorCode: Integer);
var
  LenS: byte absolute s;
  Offset: Integer;
  NBCInx: Integer;
begin
  { trim trailing spaces }
  while (LenS > 0) and (s[LenS] = ' ') do
    dec(LenS);
  { empty strings are invalid }
  if (LenS = 0) then
  begin
    LI := 0;
    ErrorCode := -1;
  end;
  { from now on S must have at least one non-blank char }

  { find the first non-blank char }
  NBCInx := 1;
  while (s[NBCInx] = ' ') do
    inc(NBCInx);

  { check for a string of the form nnnnH }
  Offset := 0;
  if (upcase(s[LenS]) = 'H') then
  begin
    { if the first non-blank char is the final character, then the
      string is just of the form <spaces>H and is invalid }
    if (NBCInx = LenS) then
    begin
      LI := 0;
      ErrorCode := LenS;
      Exit;
    end;
    Move(s[NBCInx], s[NBCInx + 1], LenS - NBCInx);
    s[NBCInx] := '$';
    Offset := -1;
  end
  { check for a string of the form 0Xnnnn }
  else
  begin
    if (NBCInx < LenS) and (s[NBCInx] = '0') and (upcase(s[NBCInx + 1]) = 'X')
    then
    begin
      s[NBCInx] := ' ';
      s[NBCInx + 1] := '$';
    end;
  end;
  Val(s, LI, ErrorCode);
  if (ErrorCode <> 0) then
  begin
    LI := 0;
    inc(ErrorCode, Offset);
  end;
end;

class function TNovusStringUtils.IsNumberStr;
var
  I: Longint;
begin
  result := false;
  for I := 1 to Length(s) do
  begin
    If Not IsNumber(s[I]) then
    begin
      result := false;

      Exit;
    end
    else
      result := True;
  end;
end;

class function TNovusStringUtils.IsAlphaStr;
var
  I: Longint;
begin
  result := false;
  for I := 1 to Length(s) do
  begin
    If Not IsAlpha(s[I]) then
    begin
      result := false;

      Exit;
    end
    else
      result := True;
  end;
end;

class function TNovusStringUtils.Str2LongS(const s: ShortString;
  var I: Longint): Boolean;
var
  ec: Integer;
begin
  ValLongInt(s, I, ec);
  if (ec = 0) then
    result := True
  else
  begin
    result := false;
    if (ec < 0) then
      I := succ(Length(s))
    else
      I := ec;
  end;
end;

class function TNovusStringUtils.IsIntStr(s: String): Boolean;
begin
  try
    StrtoInt(s);
    result := True;
  except
    result := false;
  end;
end;

class function TNovusStringUtils.Str2DateTime(s: String): tDateTime;
begin
  Try
    result := StrToDateTime(s)
  except
    result := 0;
  End;
end;

class function TNovusStringUtils.MakeTmpFileName;
var
  tmpStr: String;
  H, M, s, MS: Word;
  Present: tDateTime;
  UID: TGUID;
begin
  result := '';

  If Not AUseGUID then
  begin
    Present := Now;

    DecodeTime(Present, H, M, s, MS);

    result := Copy(IntToStr(H) + IntToStr(M) + IntToStr(s) +
      IntToStr(MS), 1, 8);
  end
  else
  begin
    CreateGUID(UID);
    result := GUIDToString(UID);
    result := StringReplace(result, '{', '', [rfReplaceAll]);
    result := StringReplace(result, '}', '', [rfReplaceAll]);
  end;

  If AExt <> '' then
    result := result + '.' + AExt;
end;

class function TNovusStringUtils.JustFilename;
var
  I: Longint;
begin
  result := '';
  if aPathName = '' then
    Exit;
  I := succ(Length(aPathName));
  repeat
    dec(I);
  until (I = 0) or (aPathName[I] in DosDelimSet);
  result := Copy(aPathName, succ(I), StMaxFileLen);
end;


class function TNovusStringUtils.GetStrToken(const s: string; sTokens: array of string;
  var iPos: Integer; var sLastToken: String): string;
var
  sTemp: string;
  iEndPos: Integer;

  function FindEndPos: Integer;
  var
    liEndPos: Integer;
    I: integer;
  begin
     liEndPos := 0;
     i := 0;
     while( liEndPos = 0) do
       begin
         sLastToken := sTokens[i];
         liEndPos := Pos(sLastToken, sTemp);

         Inc(i);
         if I > Length(sTokens) then break
       end;

    Result := liEndPos;
  end;

begin
  result := '';
  if (iPos <= 0) or (iPos > Length(s)) then
    Exit;

  sTemp := Copy(s, iPos, Length(s) + 1 - iPos);

  iEndPos := FindEndPos;
  if iEndPos <= 0 then
  begin
    result := sTemp;
    iPos := -1;
  end
  else
  begin
    result := Copy(sTemp, 1, iEndPos - 1);

    iPos := iPos + iEndPos + Length(sLastToken) - 1;
  end
end;


class function TNovusStringUtils.GetStrTokenA(const s, sDelim: string;
  var iPos: Integer): string;
var
  sTemp: string;
  iEndPos: Integer;
begin
  result := '';
  if (iPos <= 0) or (iPos > Length(s)) then
    Exit;

  sTemp := Copy(s, iPos, Length(s) + 1 - iPos);
  iEndPos := Pos(sDelim, sTemp);
  if iEndPos <= 0 then
  begin
    result := sTemp;
    iPos := -1;
  end
  else
  begin
    result := Copy(sTemp, 1, iEndPos - 1);
    iPos := iPos + iEndPos + Length(sDelim) - 1;
  end
end;

class function TNovusStringUtils.IsLowerChar;
begin
  result := Ch in ['a' .. 'z']
end;

class function TNovusStringUtils.IsUpperChar;
begin
  result := Ch in ['A' .. 'Z']
end;

class function TNovusStringUtils.UpLowerA;
begin
  AName := Lowercase(AName);

  result := UpLower(AName, FirstOnly);
end;

class function TNovusStringUtils.UpLower;
var
  I: Integer;
  CapitalizeNextLetter: Boolean;
  tStr: String;
begin
  result := Trim(AName);
  CapitalizeNextLetter := True;
  for I := 1 to Length(result) do
  begin
    If ((FirstOnly = True) and (TNovusStringUtils.IsUpperChar(result[I]) = True))
    then
      Break;

    If CapitalizeNextLetter and TNovusStringUtils.IsLowerChar(result[I]) then
    begin
      result[I] := upcase(result[I]);

      if FirstOnly = True then
        Break;
    end;

    CapitalizeNextLetter := false;
    If result[I] in [' ', '/', '\', '|', '.', ',', ';', ':', '"', '-', '^'] Then
      CapitalizeNextLetter := True
    else If (result[I] = '''') and (I = 2) Then
      CapitalizeNextLetter := True

  end;

  tStr := 'O' + #39;
  I := Pos(tStr, Uppercase(result));
  If I <> 0 then
  Begin
    Delete(result, I, 2);
    Insert(tStr, result, I);
  end;

  I := Pos('MC', Uppercase(result));
  If { (I <> 0) } (I = 1) then
  begin
    Delete(result, I, 2);
    Insert('Mc', result, I);
  end;

end;

class function TNovusStringUtils.BooleanToStr(bValue: Boolean): string;
begin
  if bValue then
    result := 'True'
  else
    result := 'False';
end;

class function TNovusStringUtils.StrToBoolean(const sValue: string): Boolean;
begin
  result := (Pos(Uppercase(sValue), 'TRUE|T|YES|Y|1') > 0);
end;

class function TNovusStringUtils.IsBoolean(const sValue: string): Boolean;
begin
  result := ((Pos(Uppercase(sValue), 'TRUE|T|YES|Y|1') > 0) or
    (Pos(Uppercase(sValue), 'FALSE|F|NO|N|0') > 0));
end;

class function TNovusStringUtils.ReplaceStr(Const sString, sOldStr,
  sNewStr: string; ACheckUpper: Boolean = false): string;
var
  iPos: Integer; { position of the old string }
  iLenOld: Integer; { length of the old string }
  lsOldStr: String;
  lsNewStr: String;
begin
  if ACheckUpper then
  begin
    result := Uppercase(sString);
    lsOldStr := Uppercase(sOldStr);
    lsNewStr := Uppercase(sNewStr);
  end
  else
  begin
    result := sString;
    lsOldStr := sOldStr;
    lsNewStr := sNewStr;
  end;

  iLenOld := Length(lsOldStr);

  if (lsNewStr <> lsOldStr) then
  begin
    iPos := Pos(lsOldStr, result);
    while (iPos > 0) do
    begin
      Delete(result, iPos, iLenOld);
      if (lsNewStr <> '') then
        Insert(sNewStr, result, iPos);

      iPos := Pos(lsOldStr, result);
    end; { while }
  end; { if }
end;

class function TNovusStringUtils.PadLeft(const s: string; iLen: Integer;
  const sFillChar: Char): string;
var
  iLength: Integer;
begin
  iLength := Length(s);
  if (iLength <> iLen) then
    if iLength > iLen then
      result := Copy(s, 1, iLen)
    else
      result := TNovusStringUtils.Replicate(sFillChar, (iLen - iLength)) + s
  else
    result := s;
end;

class function TNovusStringUtils.Replicate(c: Char; iLen: Integer): string;
var
  I: Integer;
begin
  result := '';
  for I := 1 to iLen do
    result := result + c;
end;

class procedure TNovusStringUtils.ParseStringList(sDelimeter: string;
  sStr: string; Var Lines: TStringList);
var
  I: Integer;
begin
  if Not Assigned(Lines) then
    Exit;

  Lines.Clear;
  repeat
    I := Pos(sDelimeter, sStr);
    if I = 0 then
    begin
      Lines.Add(Trim(sStr));
      sStr := '';
    end
    else
    begin
      Lines.Add(Trim(System.Copy(sStr, 1, I - 1)));
      System.Delete(sStr, 1, I);
    end;
  until sStr = '';
end;

class procedure TNovusStringUtils.ParseStringList(aDelimeter: Char;
  sStr: string; Var Lines: TStringList);
var
  I: Integer;
begin
  if Not Assigned(Lines) then
    Exit;

  Lines.Clear;
  repeat
    I := Pos(aDelimeter, sStr);
    if I = 0 then
    begin
      Lines.Add(Trim(sStr));
      sStr := '';
    end
    else
    begin
      Lines.Add((System.Copy(sStr, 1, I - 1)));
      System.Delete(sStr, 1, I);
    end;
  until sStr = '';
end;

class procedure TNovusStringUtils.String2StringList(aString: string;
  var AStringList: TStringList);
Var
  Ch: Char;
  I: Integer;
  lsLine: String;
begin
  AStringList.Clear;

  aString := StripChar(aString, #10);

  I := 0;
  lsLine := '';
  Repeat
    Ch := aString[I];

    if Ch = #0 then
    begin
      if Ch = #13 then
      begin
        AStringList.Add(lsLine);

        lsLine := '';
      end
      else if Ch <> #13 then
        lsLine := lsLine + Ch;
    end;

    inc(I);
  Until I > Length(aString);
end;

class procedure TNovusStringUtils.String2Strings(aString: string;
  var AStrings: TStrings);
Var
  Ch: Char;
  I: Integer;
  lsLine: String;
begin
  AStrings.Clear;

  aString := StripChar(aString, #10);

  I := 0;
  lsLine := '';
  Repeat
    Ch := aString[I];

    if Ch <> #0 then
    begin
      if Ch = #13 then
      begin
        AStrings.Add(lsLine);

        lsLine := '';
      end
      else if Ch <> #13 then
        lsLine := lsLine + Ch;
    end;

    inc(I);
  Until I > Length(aString);
end;

class function TNovusStringUtils.Str2Float;
begin
  Try
    result := StrToFloat(aStr);
  Except
    result := 0.0;
  end;
end;

class function TNovusStringUtils.Str2Curr;
begin
  Try
    result := StrToCurr(aStr);
  Except
    result := 0.0;
  end;
end;

class function TNovusStringUtils.JustPathname(const aPathName: String): string;
var
  I: Longint;
begin
  result := '';

  if Trim(aPathName) = '' then
    Exit;

  result := Trim(ExtractFilePath(aPathName));
end;

(*
class function TNovusStringUtils.TrailingBackSlash(const aFilename
  : string): string;
begin
  result := '';

  if Trim(aFilename) <> '' then
    result := IncludeTrailingPathDelimiter(aFilename);
end;
*)

class function TNovusStringUtils.MemoryStreamToString
  (Stream: TMemoryStream): string;
var
  MS: TMemoryStream;
begin
  result := '';
  MS := TMemoryStream.Create;
  try
    MS.LoadFromStream(Stream);
    SetString(result, PChar(MS.memory), MS.Size div SizeOf(Char));
  finally
    MS.free;
  end;
end;


// Win32 API FormatMessage function
// http://msdn.microsoft.com/en-us/library/windows/desktop/ms679351%28v=vs.85%29.aspx

class function TNovusStringUtils.FormatMessStr(aString: String): String;
const
  TextBufLength = 256;
var
  sText: array [0 .. TextBufLength] of Char;
begin
  ZeroMemory(@sText, TextBufLength);

  FormatMessage(FORMAT_MESSAGE_FROM_STRING, PWideChar(aString), 0, 0, sText,
    TextBufLength, nil);
  result := String(sText);

end;

class function TNovusStringUtils.FormatMessStrOptions(aString: String;
  aFormatOptions: Integer): String;
Var
  lStr: String;
begin
  case aFormatOptions of
    0:
      begin
        lStr := TNovusStringUtils.ReplaceChar(aString, #$D, '$');

        result := TNovusStringUtils.ReplaceStr(lStr, '$', '%r', false);
      end;
    1:
      begin
        lStr := TNovusStringUtils.ReplaceChar(aString, #$A, '$');

        result := TNovusStringUtils.ReplaceStr(lStr, '$', '%n', false);
      end;
  end;
end;

class function TNovusStringUtils.VarArrayToStr(const vArray: variant): string;

  function _VarToStr(const V: variant): string;
  var
    Vt: Integer;
  begin
    Vt := VarType(V);
    case Vt of
      varSmallint, varInteger:
        result := IntToStr(Integer(V));
      varSingle, varDouble, varCurrency:
        result := FloatToStr(Double(V));
      varDate:
        result := VarToStr(V);
      varOleStr:
        result := WideString(V);
      varBoolean:
        result := VarToStr(V);
      varVariant:
        result := VarToStr(variant(V));
      varByte:
        result := Char(byte(V));
      varString:
        result := String(V);
      varArray:
        result := VarArrayToStr(variant(V));
    end;
  end;

var
  I: Integer;
begin
  result := '[';
  if (VarType(vArray) and varArray) = 0 then
    result := _VarToStr(vArray)
  else
    for I := VarArrayLowBound(vArray, 1) to VarArrayHighBound(vArray, 1) do
      if I = VarArrayLowBound(vArray, 1) then
        result := result + _VarToStr(vArray[I])
      else
        result := result + _VarToStr(vArray[I]);

  result := result + ']';
end;

class function TNovusStringUtils.VarStrNull(const V: OleVariant): string;
// avoid problems with null strings
begin
  result := '';
  if not VarIsNull(V) then
  begin
    if VarIsArray(V) then
      result := VarArrayToStr(V)
    else
      result := VarToStr(V);
  end;
end;

class function TNovusStringUtils.StrToUInt64(const s: String): UInt64;
var
  c: Cardinal;
  P: PChar;
begin
  P := Pointer(s);
  if P = nil then
  begin
    result := 0;
    Exit;
  end;
  if ord(P^) in [1 .. 32] then
    repeat
      inc(P)
    until not(ord(P^) in [1 .. 32]);
  c := ord(P^) - 48;
  if c > 9 then
    result := 0
  else
  begin
    result := c;
    inc(P);
    repeat
      c := ord(P^) - 48;
      if c > 9 then
        Break
      else
        result := result * 10 + c;
      inc(P);
    until false;
  end;
end;

end.
