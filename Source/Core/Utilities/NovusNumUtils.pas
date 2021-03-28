{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusNumUtils;

interface

uses Windows, sysutils, NovusUtilities, System.StrUtils,
     System.Classes;

Type
  TNovusNumUtils = class(TNovusUtilities)
  protected
  public
    class function HexToInt64(HexStr: String): Int64;
    class function HexToUInt64(HexStr: String): UInt64;
    class function StrToUInt64(aStr: String): UInt64;
    class function StrToUInt8(aStr: String): UInt8;
    class function HexToUint16(HexStr: String): UInt16;
    class function HexToUint8(HexStr: String): UInt8;
    class function Int64ToBin(IValue: Int64; NumBits: word = 64): string;
    class function BinToInt64(BinStr: string): Int64;
    class function BinToUInt64(BinStr: string): UInt64;
    class function BinToUInt8(BinStr: string): UInt8;
    class function BinToUInt16(BinStr: string): UInt16;
    class function StrToUInt16(aStr: String): UInt16;
    class function CreateWord(const aHiByte, aLoByte: byte): word;
    class function HexStrToBytes(aHexStr: String; aLittle_Endian: boolean = False): tBytes;
    class function ReverseBytesOrder(aBytes: TBytes): tBytes;

  end;

implementation

class function TNovusNumUtils.Int64ToBin(IValue: Int64;
  NumBits: word = 64): string;
var
  RetVar: string;
begin
  RetVar := '';

  case NumBits of
    32:
      IValue := dword(IValue);
    16:
      IValue := word(IValue);
    8:
      IValue := byte(IValue);
  end;

  while IValue <> 0 do
  begin
    RetVar := char(48 + (IValue and 1)) + RetVar;
    IValue := IValue shr 1;
  end;

  if RetVar = '' then
    RetVar := '0';
  result := RetVar;
end;

class function TNovusNumUtils.BinToInt64(BinStr: string): Int64;
var
  i: byte;
  RetVar: Int64;
begin
  Result := 0;
  if Trim(BinStr) = '' then Exit;

  BinStr := UpperCase(BinStr);
  if BinStr[length(BinStr)] = 'B' then
    Delete(BinStr, length(BinStr), 1);
  RetVar := 0;
  for i := 1 to length(BinStr) do
  begin
    if not(BinStr[i] in ['0', '1']) then
    begin
      RetVar := 0;
      Break;
    end;
    RetVar := (RetVar shl 1) + (byte(BinStr[i]) and 1);
  end;

  result := RetVar;
end;

class function TNovusNumUtils.BinToUInt64(BinStr: string): UInt64;
var
  i: byte;
  RetVar: UInt64;
begin
  Result := 0;
  if Trim(BinStr) = '' then Exit;

  BinStr := UpperCase(BinStr);
  if BinStr[length(BinStr)] = 'B' then
    Delete(BinStr, length(BinStr), 1);
  RetVar := 0;
  for i := 1 to length(BinStr) do
  begin
    if not(BinStr[i] in ['0', '1']) then
    begin
      RetVar := 0;
      Break;
    end;
    RetVar := (RetVar shl 1) + (byte(BinStr[i]) and 1);
  end;

  result := RetVar;
end;

class function TNovusNumUtils.BinToUInt8(BinStr: string): UInt8;
var
  i: byte;
  RetVar: UInt8;
begin
  Result := 0;
  if Trim(BinStr) = '' then Exit;

  BinStr := UpperCase(BinStr);
  if BinStr[length(BinStr)] = 'B' then
    Delete(BinStr, length(BinStr), 1);
  RetVar := 0;
  for i := 1 to length(BinStr) do
  begin
    if not(BinStr[i] in ['0', '1']) then
    begin
      RetVar := 0;
      Break;
    end;
    RetVar := (RetVar shl 1) + (byte(BinStr[i]) and 1);
  end;

  result := RetVar;
end;

class function TNovusNumUtils.BinToUInt16(BinStr: string): UInt16;
var
  i: byte;
  RetVar: UInt16;
begin
  Result := 0;
  if Trim(BinStr) = '' then Exit;

  BinStr := UpperCase(BinStr);
  if BinStr[length(BinStr)] = 'B' then
    Delete(BinStr, length(BinStr), 1);
  RetVar := 0;
  for i := 1 to length(BinStr) do
  begin
    if not(BinStr[i] in ['0', '1']) then
    begin
      RetVar := 0;
      Break;
    end;
    RetVar := (RetVar shl 1) + (byte(BinStr[i]) and 1);
  end;

  result := RetVar;
end;


class function TNovusNumUtils.HexToInt64(HexStr: String): Int64;
var
  RetVar: Int64;
  i: byte;
begin
  Result := 0;
  if Trim(HexStr) = '' then Exit;

  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
    Delete(HexStr, length(HexStr), 1);
  RetVar := 0;

  for i := 1 to length(HexStr) do
  begin
    RetVar := RetVar shl 4;
    if HexStr[i] in ['0' .. '9'] then
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    else if HexStr[i] in ['A' .. 'F'] then
      RetVar := RetVar + (byte(HexStr[i]) - 55)
    else
    begin
      RetVar := 0;
      Break;
    end;
  end;

  result := RetVar;
end;

class function TNovusNumUtils.HexToUInt64(HexStr: String): UInt64;
var
  RetVar: UInt64;
  i: byte;
begin
  Result := 0;
  if Trim(HexStr) = '' then Exit;

  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
    Delete(HexStr, length(HexStr), 1);
  RetVar := 0;

  for i := 1 to length(HexStr) do
  begin
    RetVar := RetVar shl 4;
    if HexStr[i] in ['0' .. '9'] then
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    else if HexStr[i] in ['A' .. 'F'] then
      RetVar := RetVar + (byte(HexStr[i]) - 55)
    else
    begin
      RetVar := 0;
      Break;
    end;
  end;

  result := RetVar;
end;


class function TNovusNumUtils.StrToUInt64(aStr: String): UInt64;
begin
  Result := 0;
  if Trim(aStr) = '' then Exit;

  Try
    Result := StrToUInt(aStr);
  Except
    Result := 0;
  End;

end;

class function TNovusNumUtils.StrToUInt8(aStr: String): UInt8;
begin
  Result := 0;
  if Trim(aStr) = '' then Exit;

  Try
    Result := StrToUInt(aStr);
  Except
    Result := 0;
  End;

end;

class function TNovusNumUtils.StrToUInt16(aStr: String): UInt16;
begin
  Result := 0;
  if Trim(aStr) = '' then Exit;

  Try
    Result := StrToUInt(aStr);
  Except
    Result := 0;
  End;

end;



class function TNovusNumUtils.HexToUInt16(HexStr: String): Uint16;
var
  RetVar: uInt16;
  i: byte;
begin
  Result := 0;
  if Trim(HexStr) = '' then Exit;

  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
    Delete(HexStr, length(HexStr), 1);
  RetVar := 0;

  for i := 1 to length(HexStr) do
  begin
    RetVar := RetVar shl 4;
    if HexStr[i] in ['0' .. '9'] then
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    else if HexStr[i] in ['A' .. 'F'] then
      RetVar := RetVar + (byte(HexStr[i]) - 55)
    else
    begin
      RetVar := 0;
      Break;
    end;
  end;

  result := RetVar;
end;

class function TNovusNumUtils.HexToUInt8(HexStr: String): Uint8;
var
  RetVar: uInt8;
  i: byte;
begin
  Result := 0;
  if Trim(HexStr) = '' then Exit;

  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
    Delete(HexStr, length(HexStr), 1);
  RetVar := 0;

  for i := 1 to length(HexStr) do
  begin
    RetVar := RetVar shl 4;
    if HexStr[i] in ['0' .. '9'] then
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    else if HexStr[i] in ['A' .. 'F'] then
      RetVar := RetVar + (byte(HexStr[i]) - 55)
    else
    begin
      RetVar := 0;
      Break;
    end;
  end;

  result := RetVar;
end;


class function TNovusNumUtils.CreateWord(const aHiByte, aLoByte: byte): word;
var
  WR: WordRec;
begin
  WR.Hi := aHiByte;
  WR.Lo := aLoByte;
  result := word(WR);
end;

class function TNovusNumUtils.HexStrToBytes(aHexStr: String; aLittle_Endian: boolean): tBytes;
Var
  TmpBytes: TBytes;
begin
  SetLength(TmpBytes, Length(aHexStr) div 2);
  HexToBin(PChar(aHexStr), TmpBytes[0], Length(TmpBytes));

  if aLittle_Endian then
    TmpBytes :=  ReverseBytesOrder(TmpBytes);

  Result := TmpBytes;
end;

class function TNovusNumUtils.ReverseBytesOrder(aBytes: TBytes): tBytes;
var
  TmpBytes: TBytes;
  i, h: integer;
begin
  h := High(aBytes);
  SetLength(TmpBytes, Length(aBytes));
  for i := 0 to h do
    TmpBytes[i] := aBytes[h - i];
  Result := TmpBytes;
end;

//LittleEndian




end.
