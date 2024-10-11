unit NovusVariants;

interface

uses NovusUtilities, variants, SysUtils;

type
  TVarRecHelper = record
    VRec: TVarRec;
    VExtended: Extended;
    VCurrency: Currency;
    VInt64: Int64;
    VInteger: Integer;
    VBoolean: Boolean;
    VString: string;
    VAnsiString: AnsiString;
    VUnicodeString: UnicodeString;
    VVariant: Variant;
  end;

  TNovusVariants = class(TNovusUtilities)
  protected
  private
  public
    class function VarToVarRec(const V: Variant): TVarRecHelper;
    class procedure DisposeVarRec(aVarRec: TVarRec);
  end;

implementation

class procedure TNovusVariants.DisposeVarRec(aVarRec: TVarRec);
begin
  case aVarRec.VType of
    vtUnicodeString: Dispose(aVarRec.VUnicodeString);
    vtString: Dispose(aVarRec.VString)
  end;
end;

class function TNovusVariants.VarToVarRec(const V: Variant): TVarRecHelper;
begin
  // Initialize the VRec
  FillChar(Result.VRec, SizeOf(TVarRec), 0);

  // Determine the Variant type
  case VarType(V) and varTypeMask of
    varSmallint,
    varInteger,
    varShortInt,
    varByte,
    varWord,
    varLongWord:
      begin
        Result.VInteger := V;
        Result.VRec.VType := vtInteger;
        Result.VRec.VInteger := Result.VInteger;
      end;

    varInt64,
    varUInt64:
      begin
        Result.VInt64 := V;
        Result.VRec.VType := vtInt64;
        Result.VRec.VInt64 := @Result.VInt64;
      end;

    varSingle,
    varDouble:
      begin
        Result.VExtended := V;
        Result.VRec.VType := vtExtended;
        Result.VRec.VExtended := @Result.VExtended;
      end;

    varCurrency:
      begin
        Result.VCurrency := V;
        Result.VRec.VType := vtCurrency;
        Result.VRec.VCurrency := @Result.VCurrency;
      end;

    varUString:
      begin
        Result.VUnicodeString := V;
        Result.VRec.VType := vtUnicodeString;
        Result.VRec.VUnicodeString := Pointer(Result.VUnicodeString);
      end;

    varOleStr:
      begin
        Result.VUnicodeString := UnicodeString(V);
        Result.VRec.VType := vtUnicodeString;
        Result.VRec.VUnicodeString := Pointer(Result.VUnicodeString);
      end;

    varString:
      begin
        Result.VAnsiString := AnsiString(V);
        Result.VRec.VType := vtAnsiString;
        Result.VRec.VAnsiString := Pointer(Result.VAnsiString);
      end;

    varBoolean:
      begin
        Result.VBoolean := V;
        Result.VRec.VType := vtBoolean;
        Result.VRec.VBoolean := Result.VBoolean;
      end;

    varDate:
      begin
        // Dates are stored as TDateTime (Double)
        Result.VExtended := V;
        Result.VRec.VType := vtExtended;
        Result.VRec.VExtended := @Result.VExtended;
      end;

    else
      // For other types, store the Variant itself
      Result.VVariant := V;
      Result.VRec.VType := vtVariant;
      Result.VRec.VVariant := @Result.VVariant;
  end;
end;

end.
