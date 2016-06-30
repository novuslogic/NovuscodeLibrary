unit NovusStringUtils;

interface


uses Windows, sysutils, Forms, NovusUtilities, Classes, variants;

Const
  Cr = #13;
  ToUpperChars: array [0..255] of Char =
  (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,
    #$60, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$7B, #$7C, #$7D, #$7E, #$7F,
    #$80, #$81, #$82, #$81, #$84, #$85, #$86, #$87, #$88, #$89, #$8A, #$8B, #$8C, #$8D, #$8E, #$8F,
    #$80, #$91, #$92, #$93, #$94, #$95, #$96, #$97, #$98, #$99, #$8A, #$9B, #$8C, #$8D, #$8E, #$8F,
    #$A0, #$A1, #$A1, #$A3, #$A4, #$A5, #$A6, #$A7, #$A8, #$A9, #$AA, #$AB, #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B2, #$A5, #$B5, #$B6, #$B7, #$A8, #$B9, #$AA, #$BB, #$A3, #$BD, #$BD, #$AF,
    #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF,
    #$D0, #$D1, #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF,
    #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF,
    #$D0, #$D1, #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF);

Type
  WStrRec = packed record
    Length    : Longint;
  end;

const
  StrOffset = SizeOf(WStrRec);


Type
  LStrRec = record
    AllocSize : Longint;
    RefCount  : Longint;
    Length    : Longint;
  end;

Type
  TNovusStringUtils = class(TNovusUtilities)
  public
    class function FormatMessStrOptions(aString: String; aFormatOptions: Integer): String;
    class function MemoryStreamToString(Stream: TMemoryStream): string;
    class function Str2Float(loStr: String):single;
    class function Str2Int(AStr: String):integer;
    class function Replicate(c: Char; iLen: integer): string;
    class function PadLeft(const s: string; iLen: integer; const sFillChar: char): string;
    class function JustFilename(const PathName : String) : String;
    class function JustPathname(const PathName : String) : String;
    class function Str2DateTime(S: String): tDateTime;
    class procedure ValLongInt(S : ShortString; var LI : Longint; var ErrorCode : integer);
    class function Str2LongS(const S : ShortString; var I : LongInt) : Boolean;
    class function IsNumber(AChar: Char) : boolean;
    class function IsAlpha(AChar: Char): Boolean;
    class function IsNumberStr(S : String): Boolean;
    class function IsAlphaStr(S : String): Boolean;
//    class function PosStr(const FindString, SourceString: string; StartPos: Integer = 1): Integer;
//    class function PosText(const FindString, SourceString: string; StartPos: Integer = 1): Integer;
//    class function GetValue(const AText, AName: string): string;
    class procedure GetNames(AText: string; AList: TStringList);
//    class procedure TagsToCSV(Src, Dst: TStringList);
    class function SubstCharSim(P : string; OC, NC : ANSIChar) : string;
    class function RootDirectory: String;
    class function StripChar(s : String; Ch : Char) : string;
    class function ReplaceChar(s : String; aFromCh, aToCh : Char): String;
    class function GetStrTokenA(const s, sDelim: string; var iPos: integer): string;
    class function GetStrRes(const Index: integer): String;
    class function StrToInt(AStr : String):Integer;
//    class function StrChPosL(const P : AnsiString; C : AnsiChar; var Pos : Cardinal) : Boolean;
    class function StrChInsertL(const S : AnsiString; C : AnsiChar; Pos : Cardinal) : AnsiString;
    class function IsIntStr(S: String):Boolean;
    class function MakeTmpFileName(AExt: string;AUseGUID: Boolean = false): String;
    class function ReplaceStrPos(const sString, sOldStr, sNewStr: string; APos: Integer = -1; AUseBlank: Boolean = False): string;
    class function IsLowerChar(ch : Char) : Boolean;
    class function IsUpperChar(ch : Char) : Boolean;
    class function UpLower(AName:String;FirstOnly: Boolean): String;
    class function UpLowerA(AName:String;FirstOnly: Boolean): String;
    class function BooleanToStr(bValue: boolean): string;
    class function StrToBoolean(const sValue: string): boolean;
    class function ReplaceStr(Const sString, sOldStr, sNewStr: string; ACheckUpper: Boolean = false): string;
    class procedure ParseStringList(sDelimeter: string; sStr: string; Var Lines: TStringList); overload;
    class procedure ParseStringList(aDelimeter: Char; sStr: string; Var Lines: TStringList); overload;
    class procedure String2StringList(AString: string; var AStringList: TStringList);
    class procedure String2Strings(AString: string; var AStrings: TStrings);
    class function FormatMessStr(aString: String): String;
//    class function TrailingBackSlash(const aFilename: string): string;
    class function VarArrayToStr(const vArray: variant): string;
    class function VarStrNull(const V:OleVariant):string; //avoid problems with null strings
    class function IsBoolean(const sValue: string): boolean;
    class function StrToUInt64(const S: String): UInt64;
  end;

implementation

class function TNovusStringUtils.StripChar(s : String; Ch : Char) : string;
Var
  fsStr: String;
  I: integer;
begin
  fsStr := '';
  for i := 1 to Length(s) do
    If s[i] <> Ch then  fsStr := fsStr + s[i];

  result := fsStr;
end;

class function TNovusStringUtils.ReplaceChar(s : String; aFromCh, aToCh : Char): String;
Var
  fsStr: String;
  I: integer;
begin
  fsStr := '';
  for i := 1 to Length(s) do
    If s[i] = aFromCh then  fsStr := fsStr + aToCh
    else fsStr := fsStr + s[i];

  result := fsStr;
end;

class function TNovusStringUtils.GetStrRes;
var
  buffer : array[0..255] of char;
  ls : integer;
begin
  Result := '';
  ls := LoadString(hInstance,
                   Index,
                   buffer,
                   sizeof(buffer));
  if ls <> 0 then
    Result := buffer;

end;

class function TNovusStringUtils.RootDirectory;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

class function TNovusStringUtils.StrToInt;
begin
  Result := 0;

  AStr := StripChar(AStr, #$D);
  AStr := StripChar(AStr, #$A);

  Try
    If Trim(AStr) = '' then Exit;

    Result := SysUtils.StrtoInt(AStr);
  Except
    Result := 0;
  end;
end;

(*
class function TNovusStringUtils.GetValue(const AText, AName: string): string;
var
  P, P2, L: Integer;
begin
  Result := '';
  L := Length(AName) + 2;
  P := PosText(AName + '="', AText);
  if P = 0 then
    Exit;
  P2 := PosStr('"', AText, P + L);
  if P2 = 0 then
    Exit;
  Result := Copy(AText, P + L, P2 - (P + L));
  Result := StringReplace(Result, '~~', Cr, [rfReplaceAll]);
end;
*)

class function TNovusStringUtils.ReplaceStrPos;
var
  iPos: integer; {position of the old string}
  iLenOld: integer; {length of the old string}
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
    else iPos := APos;

    while (iPos > 0) do
    begin
      Delete(result, iPos, iLenOld);

      if (sNewStr <> '') then
        Insert(sNewStr, result, iPos);

      iPos := Pos(sOldStr, result);
    end; {while}
  end; {if}
end;

(*
class procedure TNovusStringUtils.TagsToCSV(Src, Dst: TStringList);
var
  I, FI, FC: Integer;
  Names: TStringList;
  Rec: TStringList;
  S: string;
begin
  Dst.Clear;
  if Src.Count < 1 then
    Exit;
  Names := TStringList.Create;
  Rec := TStringList.Create;
  try
    GetNames(Src[0], Names);
    FC := Names.Count;
    if FC > 0 then
    begin
      Dst.Add(Names.CommaText);
      for I := 0 to Src.Count - 1 do
      begin
        S := '';
        Rec.Clear;
        for FI := 0 to FC - 1 do
          Rec.Add(GetValue(Src[I], Names[FI]));
        Dst.Add(Rec.CommaText);
      end;
    end;
  finally
    Rec.Free;
    Names.Free;
  end;
end;
*)

class procedure TNovusStringUtils.GetNames(AText: string; AList: TStringList);
var
  P: Integer;
  S: string;
begin
  AList.Clear;
  repeat
    AText := Trim(AText);
    P := Pos('="', AText);
    if P > 0 then
    begin
      S := Copy(AText, 1, P - 1);
      AList.Add(S);
      Delete(AText, 1, P + 1);
      P := Pos('"', AText);
      if P > 0 then
        Delete(AText, 1, P);
    end;
  until P = 0;
end;

(*
class function TNovusStringUtils.PosText(const FindString, SourceString: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        NOP
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        PUSH    EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        PUSH    EAX
        SUB     EDX,ECX
        JNG     @@qtx
        ADD     EDI,ECX
        MOV     ECX,EDX
        MOV     EDX,EAX
        MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
@@lp1:  MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JE      @@uu
@@fr:   INC     EDI
        DEC     ECX
        JNE     @@lp1
@@qtx:  ADD     ESP,$08
@@qt0:  XOR     EAX,EAX
        JMP     @@qt
@@ms:   MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOV     EDX,[ESP]
        JMP     @@fr
        NOP
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     BL,BYTE PTR [ESI+EDX]
        MOV     AH,BYTE PTR [EDI+EDX]
        CMP     BL,AH
        JE      @@eq
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOVZX   EBX,AH
        XOR     AL,BYTE PTR [EBX+ToUpperChars]
        JNE     @@ms
@@eq:   DEC     EDX
        JNZ     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        POP     ECX
        SUB     EAX,[ESP]
        POP     ECX
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;
*)

(*
class function TNovusStringUtils.PosStr(const FindString, SourceString: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        SUB     EDX,ECX
        JNG     @@qt0
        MOV     EBX,EAX
        XCHG    EAX,EDX
        NOP
        ADD     EDI,ECX
        MOV     ECX,EAX
        MOV     AL,BYTE PTR [ESI]
@@lp1:  CMP     AL,BYTE PTR [EDI]
        JE      @@uu
@@fr:   INC     EDI
        DEC     ECX
        JNZ     @@lp1
@@qt0:  XOR     EAX,EAX
        JMP     @@qt
@@ms:   MOV     AL,BYTE PTR [ESI]
        MOV     EBX,EDX
        JMP     @@fr
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     AL,BYTE PTR [ESI+EBX]
        XOR     AL,BYTE PTR [EDI+EBX]
        JNE     @@ms
        DEC     EBX
        JNE     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        SUB     EAX,[ESP]
@@qt:   POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;
*)

(*
class function TNovusStringUtils.StrChPosL(const P : AnsiString; C : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
asm
  push  ebx             { Save registers }
  push  edi

  or    eax, eax        { Protect against null string }
  jz    @@NotFound

  xor   edi, edi        { Zero counter }
  mov   ebx, [eax-StrOffset].LStrRec.Length  { Get input length }

@@Loop:
  inc   edi             { Increment counter }
  cmp   [eax], dl       { Did we find it? }
  jz    @@Found
  inc   eax             { Increment pointer }

  cmp   edi, ebx        { End of string? }
  jnz   @@Loop          { If not, loop }

@@NotFound:
  xor   eax, eax        { Not found, zero EAX for False }
  mov   [ecx], eax
  jmp   @@Done

@@Found:
  mov   [ecx], edi      { Set Pos }
  mov   eax, 1          { Set EAX to True }

@@Done:
  pop   edi             { Restore registers }
  pop   ebx
end;
*)

class function TNovusStringUtils.StrChInsertL(const S : AnsiString; C : AnsiChar; Pos : Cardinal) : AnsiString;
  {-Insert a character into a string at a specified position.}
var
  Temp : string;
begin
  SetLength(Temp, 1);
 // {$IFNDEF VER200}
 // Temp[1] := C;
 // {$ELSE}
  Temp := C;
//  {$ENDIF}

  Result := S;
  System.Insert(Temp, Result, Pos);
end;

class function TNovusStringUtils.SubstCharSim(P : string; OC, NC : ANSIChar) : string;
var
  step : integer;
  lOC: ANSIChar;
begin
  for step := 1 to Length(P) do
  begin
    lOC := #0;

  //  {$IFNDEF VER200}
  //  if P[step] = OC then
  //    P[step] := NC;
  //  {$ELSE}
    lOC := AnsiChar(P[step]);
    if (lOC = OC) then
      P[step] := Char(NC);
  //  {$ENDIF}
  end;
  Result := P;
end;

class function TNovusStringUtils.IsNumber;
begin
  Result := False;

  If AChar in['0'..'9', '.', #8, '-'] then Result := True
end;

class function TNovusStringUtils.IsAlpha;
begin
  Result := False;

  If AChar in['A'..'Z', 'a'..'z'] then Result := True
end;


class procedure TNovusStringUtils.ValLongInt(S : ShortString; var LI : Longint; var ErrorCode : integer);
var
  LenS   : byte absolute S;
  Offset : Integer;
  NBCInx : Integer;
begin
  {trim trailing spaces}
  while (LenS > 0) and (S[LenS] = ' ') do
    dec(LenS);
  {empty strings are invalid}
  if (LenS = 0) then begin
    LI := 0;
    ErrorCode := -1;
  end;
  {from now on S must have at least one non-blank char}

  {find the first non-blank char}
  NBCInx := 1;
  while (S[NBCInx] = ' ') do
    inc(NBCInx);

  {check for a string of the form nnnnH}
  Offset := 0;
  if (upcase(S[LenS]) = 'H') then begin
    {if the first non-blank char is the final character, then the
     string is just of the form <spaces>H and is invalid}
    if (NBCInx = LenS) then begin
      LI := 0;
      ErrorCode := LenS;
      Exit;
    end;
    Move(S[NBCInx], S[NBCInx+1], LenS-NBCInx);
    S[NBCInx] := '$';
    Offset := -1;
  end
  {check for a string of the form 0Xnnnn}
  else begin
    if (NBCInx < LenS) and
       (S[NBCInx] = '0') and (upcase(S[NBCInx+1]) = 'X') then begin
      S[NBCInx] := ' ';
      S[NBCInx+1] := '$';
    end;
  end;
  Val(S, LI, ErrorCode);
  if (ErrorCode <> 0) then begin
    LI := 0;
    Inc(ErrorCode, Offset);
  end;
end;

class function TNovusStringUtils.IsNumberStr;
var
  I : LongInt;
begin
  Result := False;
  for I := 1 to Length(S) do
    begin
      If Not IsNumber(S[I]) then
        begin
          Result := False;

          Exit;
        end
      else Result := True;
    end;
end;

class function TNovusStringUtils.IsAlphaStr;
var
  I : LongInt;
begin
  Result := False;
  for I := 1 to Length(S) do
    begin
      If Not IsAlpha(S[I]) then
        begin
          Result := False;

          Exit;
        end
      else Result := True;
    end;
end;

class function TNovusStringUtils.Str2LongS(const S : ShortString; var I : LongInt) : Boolean;
var
  ec : Integer;
begin
  ValLongint(S, I, ec);
  if (ec = 0) then
    Result := true
  else begin
    Result := false;
    if (ec < 0) then
      I := succ(length(S))
    else
      I := ec;
  end;
end;

class function TNovusStringUtils.IsIntStr(S:String):Boolean;
begin
  try
    StrToInt(S);
    Result := True;
  except
    Result := False;
  end;
end;

class function TNovusStringUtils.Str2DateTime(S: String): tDateTime;
begin
  Try
    Result := StrToDateTime(S)
  except
    Result := 0;
  End;
end;

class function TNovusStringUtils.MakeTmpFileName;
var tmpStr    : String;
    H,M,S,MS  : Word;
    Present   : TDateTime;
    UID : TGUID;
begin
  Result := '';

  If Not AUseGUID then
    begin
      Present := Now;

      DecodeTime(Present, H, M, S, MS);

      Result := Copy(IntToStr(H) + IntToStr(M) + IntToStr(S) + IntToStr(Ms), 1, 8);
    end
  else
    begin
      CreateGUID(UID);
      Result := GUIDToString(UID);
      Result := StringReplace(Result, '{', '', [rfReplaceAll]);
      Result := StringReplace(Result, '}', '', [rfReplaceAll]);
    end;

  If AExt <> '' then
    Result := Result + '.' + Aext;
end;

class function TNovusStringUtils.JustFilename;
var
  I : Longint;
begin
  Result := '';
  if PathName = '' then
    Exit;
  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);
  Result := Copy(PathName, Succ(I), StMaxFileLen);
end;


class function TNovusStringUtils.GetStrTokenA(const s, sDelim: string; var iPos: integer): string;
var
  sTemp: string;
  iEndPos: integer;
begin
  result := '';
  if (iPos <= 0) or (iPos > Length(s)) then exit;

  sTemp := Copy(s, iPos, Length(s) + 1 - iPos);
  iEndPos := Pos(sDelim, sTemp);
  if iEndPos <= 0 then begin
    result := sTemp;
    iPos := -1;
  end else begin
    result := Copy(sTemp, 1, iEndPos - 1);
    iPos := iPos + iEndPos + Length(sDelim) - 1;
  end
end;

class function TNovusStringUtils.IsLowerChar;
begin
  Result := Ch in['a'..'z']
end;

class function TNovusStringUtils.IsUpperChar;
begin
  Result := Ch in['A'..'Z']
end;

class function TNovusStringUtils.UpLowerA;
begin
  AName := Lowercase(AName);
  
  Result := UpLower(AName, FirstOnly);
end;

class function TNovusStringUtils.UpLower;
var
  I : Integer;
  CapitalizeNextLetter : Boolean;
  tStr : String;
begin
  Result := Trim(AName);
  CapitalizeNextLetter := True;
  for I := 1 to Length(Result) do
    begin
      If ((FirstOnly = True) and (tNovusStringUtils.IsUpperChar(Result[I]) = True)) then
        Break;

      If CapitalizeNextLetter and tNovusStringUtils.IsLowerChar(Result[I]) then
        begin
          Result[I] := UpCase(Result[I]);

          if FirstOnly = True then Break;
        end;

      CapitalizeNextLetter := False;
      If Result[I] in [' ', '/', '\', '|', '.', ',',';',':','"', '-', '^'] Then
        CapitalizeNextLetter := True
      else If (Result[I] = '''') and (I = 2) Then
        CapitalizeNextLetter := True

    end;

  tStr := 'O' + #39;
  I := Pos(tStr, Uppercase(Result));
  If I <> 0 then
    Begin
      Delete(Result, I, 2);
      Insert(tStr, Result, I);
    end;

  I := Pos('MC', Uppercase(Result));
  If {(I <> 0)} (I = 1) then
    begin
      Delete(Result, I, 2);
      Insert('Mc', Result, I);
    end;

end;

class function TNovusStringUtils.BooleanToStr(bValue: boolean): string;
begin
  if bValue then result := 'True' else result := 'False';
end;

class function TNovusStringUtils.StrToBoolean(const sValue: string): boolean;
begin
  result := (Pos(UpperCase(sValue), 'TRUE|T|YES|Y|1') > 0);
end;

class function TNovusStringUtils.IsBoolean(const sValue: string): boolean;
begin
  result := ((Pos(UpperCase(sValue), 'TRUE|T|YES|Y|1') > 0) or (Pos(UpperCase(sValue), 'FALSE|F|NO|N|0') > 0));
end;

class function TNovusStringUtils.ReplaceStr(Const sString, sOldStr, sNewStr: string; ACheckUpper: Boolean = false): string;
var
  iPos: integer; {position of the old string}
  iLenOld: integer; {length of the old string}
  lsOldStr: String;
  lsNewStr: String;
begin
  if ACheckUpper then
    begin
      result :=Uppercase(sString);
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
    end; {while}
  end; {if}
end;


class function TNovusStringUtils.PadLeft(const s: string; iLen: integer; const sFillChar: char): string;
var
  iLength: integer;
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

class function TNovusStringUtils.Replicate(c: Char; iLen: integer): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to iLen do
    result := result + c;
end;


class procedure TNovusStringUtils.ParseStringList(sDelimeter: string; sStr: string; Var Lines: TStringList);
var
  i: integer;
begin
  if Not Assigned(Lines) then  Exit;

  Lines.clear;
  repeat
    i := pos(sDelimeter, sStr);
    if i = 0 then
    begin
      Lines.Add(Trim(sStr));
      sStr := '';
    end
    else
    begin
      Lines.Add(Trim(system.copy(sStr, 1, i - 1)));
      system.Delete(sStr, 1, i);
    end;
  until sStr = '';
end;

class procedure TNovusStringUtils.ParseStringList(aDelimeter: char; sStr: string; Var Lines: TStringList);
var
  i: integer;
begin
  if Not Assigned(Lines) then  Exit;

  Lines.clear;
  repeat
    i := pos(aDelimeter, sStr);
    if i = 0 then
    begin
      Lines.Add(Trim(sStr));
      sStr := '';
    end
    else
    begin
      Lines.Add((system.copy(sStr, 1, i - 1)));
      system.Delete(sStr, 1, i);
    end;
  until sStr = '';
end;

class procedure TNovusStringUtils.String2StringList(AString: string; var AStringList: TStringList);
Var
  Ch: Char;
  I: Integer;
  lsLine: String;
begin
  AStringList.clear;

  AString := StripChar(AString, #10);

  I := 0;
  lsLine := '';
  Repeat
    Ch := AString[i];

    if ch = #0 then
      begin
        if ch = #13 then
          begin
            AStringList.Add(lsLine);

            lsLine := '';
          end
        else
        if Ch <> #13 then lsLine := lsLine + ch;
      end;

    Inc(i);
  Until I > Length(AString);
end;

class procedure TNovusStringUtils.String2Strings(AString: string; var AStrings: TStrings);
Var
  Ch: Char;
  I: Integer;
  lsLine: String;
begin
  AStrings.clear;

  AString := StripChar(AString, #10);

  I := 0;
  lsLine := '';
  Repeat
    Ch := AString[i];

    if ch <> #0 then
      begin
        if ch = #13 then
          begin
            AStrings.Add(lsLine);

            lsLine := '';
          end
        else
        if Ch <> #13 then lsLine := lsLine + ch;
      end;

    Inc(i);
  Until I > Length(AString);
end;



class function TNovusStringUtils.Str2Float;
begin
  Try
    Result := StrToFloat(loStr);
  Except
    Result := 0.0;
  end;
end;

class function TNovusStringUtils.Str2Int;
begin
  Try
    Result := StrToInt(AStr);
  Except
    Result := 0;
  end;
end;


class function TNovusStringUtils.JustPathname;
var
  I : Longint;
begin
  Result := '';
  
  if Trim(PathName) = '' then Exit;

  I := Succ(Length(PathName));
  repeat
    Dec(I);
  until (I = 0) or (PathName[I] in DosDelimSet);                         {!!.01}

  if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = '\') then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := Copy(PathName, 1, I);
end;


(*
class function TNovusStringUtils.TrailingBackSlash(const aFilename: string): string;
begin
  Result := '';

  if Trim(aFilename) <> '' then
    Result := IncludeTrailingPathDelimiter(aFilename);
end;
*)

class function TNovusStringUtils.MemoryStreamToString(Stream: TMemoryStream): string;
var
  ms : TMemoryStream;
begin
  Result := '';
  ms := TMemoryStream.Create;
  try
    ms.LoadFromStream(Stream);
    SetString(Result,PChar(ms.memory),ms.Size div SizeOf(Char) );
  finally
    ms.free;
  end;
end;


// Win32 API FormatMessage function
//http://msdn.microsoft.com/en-us/library/windows/desktop/ms679351%28v=vs.85%29.aspx

class function TNovusStringUtils.FormatMessStr(aString: String): String;
const
    TextBufLength=   256;
var
    sText:   array   [0..TextBufLength]   of   char;
begin
    ZeroMemory(@sText,   TextBufLength);

    FormatMessage(
        FORMAT_MESSAGE_FROM_STRING,
        PWideChar(aString),
        0,
        0,
        sText,
        TextBufLength,
        nil
    );
  Result := String(sText);

end;

class function TNovusStringUtils.FormatMessStrOptions(aString: String; aFormatOptions: Integer): String;
Var
  lStr: String;
begin
  case aFormatOptions of
  0: begin
       LStr := TNovusStringUtils.ReplaceChar(aString, #$D, '$');

       Result := TNovusStringUtils.ReplaceStr(LStr, '$', '%r', False);
     end;
   1: begin
       LStr := TNovusStringUtils.ReplaceChar(aString, #$A, '$');

       Result := TNovusStringUtils.ReplaceStr(LStr, '$', '%n', False);
     end;
  end;
end;

class function TNovusStringUtils.VarArrayToStr(const vArray: variant): string;

    function _VarToStr(const V: variant): string;
    var
    Vt: integer;
    begin
    Vt := VarType(V);
        case Vt of
          varSmallint,
          varInteger  : Result := IntToStr(integer(V));
          varSingle,
          varDouble,
          varCurrency : Result := FloatToStr(Double(V));
          varDate     : Result := VarToStr(V);
          varOleStr   : Result := WideString(V);
          varBoolean  : Result := VarToStr(V);
          varVariant  : Result := VarToStr(Variant(V));
          varByte     : Result := char(byte(V));
          varString   : Result := String(V);
          varArray    : Result := VarArrayToStr(Variant(V));
        end;
    end;

var
i : integer;
begin
    Result := '[';
     if (VarType(vArray) and VarArray)=0 then
       Result := _VarToStr(vArray)
    else
    for i := VarArrayLowBound(vArray, 1) to VarArrayHighBound(vArray, 1) do
     if i=VarArrayLowBound(vArray, 1)  then
      Result := Result+_VarToStr(vArray[i])
     else
      Result := Result+ _VarToStr(vArray[i]);

    Result:=Result+']';
end;

class function TNovusStringUtils.VarStrNull(const V:OleVariant):string; //avoid problems with null strings
begin
  Result:='';
  if not VarIsNull(V) then
  begin
    if VarIsArray(V) then
       Result:=VarArrayToStr(V)
    else
    Result:=VarToStr(V);
  end;
end;

class function TNovusStringUtils.StrToUInt64(const S: String): UInt64;
var c: cardinal;
    P: PChar;
begin
  P := Pointer(S);
  if P=nil then begin
    result := 0;
    exit;
  end;
  if ord(P^) in [1..32] then repeat inc(P) until not(ord(P^) in [1..32]);
  c := ord(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := ord(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
end;

end.



