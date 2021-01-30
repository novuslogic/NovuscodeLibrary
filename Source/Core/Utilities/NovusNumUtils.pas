{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusNumUtils;

interface

uses Windows, sysutils, NovusUtilities;

Type
  TNovusNumUtils = class(TNovusUtilities)
  protected
  public
    class function HexToInt64(HexStr: String): Int64;
    class function HexToUInt64(HexStr: String): UInt64;
    class function HexToUint16(HexStr: String): UInt16;
    class function HexToUint8(HexStr: String): UInt8;
    class function Int64ToBin(IValue: Int64; NumBits: word = 64): string;
    class function BinToInt64(BinStr: string): Int64;
    class function BinToUInt64(BinStr: string): UInt64;
    class function BinToUInt8(BinStr: string): UInt8;
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


class function TNovusNumUtils.HexToUInt16(HexStr: String): Uint16;
var
  RetVar: uInt16;
  i: byte;
begin
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



end.
