unit NovusNumUtils;

interface


uses Windows, sysutils, Forms, NovusUtilities;

Type
  TNovusNumUtils = class(TNovusUtilities)
  protected
  public
     class function HexToInt64(HexStr: String): Int64;
     class function Int64ToBin(IValue : Int64; NumBits : word = 64) : string;
     class function BinToInt64(BinStr : string) : Int64;
  end;

implementation

class function TNovusNumUtils.Int64ToBin(IValue : Int64; NumBits : word = 64) : string;
var
  RetVar : string;
begin
  retvar := '';

  case numbits of
        32 : ivalue := dword(ivalue);
        16 : ivalue := word(ivalue);
        8  : ivalue := byte(ivalue);
  end;

  while ivalue <> 0 do begin
      retvar := char(48 + (ivalue and 1)) + retvar;
      ivalue := ivalue shr 1;
  end;

  if retvar = '' then retvar := '0';
  result := retvar;
end;

class function TNovusNumUtils.BinToInt64(BinStr : string) : Int64;
var i : byte;
    RetVar : Int64;
begin
   BinStr := UpperCase(BinStr);
   if BinStr[length(BinStr)] = 'B' then Delete(BinStr,length(BinStr),1);
   RetVar := 0;
   for i := 1 to length(BinStr) do begin
     if not (BinStr[i] in ['0','1']) then begin
        RetVar := 0;
        Break;
     end;
     RetVar := (RetVar shl 1) + (byte(BinStr[i]) and 1) ;
   end;

   Result := RetVar;
end;

class function TNovusNumUtils.HexToInt64(HexStr: String): Int64;
var RetVar : Int64;
    i : byte;
begin
  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
     Delete(HexStr,length(HexStr),1);
  RetVar := 0;

  for i := 1 to length(HexStr) do begin
      RetVar := RetVar shl 4;
      if HexStr[i] in ['0'..'9'] then
         RetVar := RetVar + (byte(HexStr[i]) - 48)
      else
         if HexStr[i] in ['A'..'F'] then
            RetVar := RetVar + (byte(HexStr[i]) - 55)
         else begin
            Retvar := 0;
            break;
         end;
  end;

  Result := RetVar;
end;


end.
