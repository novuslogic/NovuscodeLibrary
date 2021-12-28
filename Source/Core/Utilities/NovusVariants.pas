unit NovusVariants;

interface

uses NovusUtilities, variants, SysUtils;

type
  TNovusVariants = class(TNovusUtilities)
  protected
  private
  public
    class function VarToVarRec(aValue: variant): TVarRec;
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

class function TNovusVariants.VarToVarRec(aValue: variant): TVarRec;
Var
  U: UnicodeString;
begin
  case VarType(aValue) of
       vtInteger:       ;
       vtBoolean:       ;
       vtChar:          ;
       vtExtended:      ;
       varUString:
         begin
           U := aValue;

           New(Result.VUnicodeString);
           Result.VType := vtUnicodeString;
           Result.VUnicodeString := Pointer(U);
         end;

       varOleStr,
       256,
       vtString:
          begin
            New(Result.VString);
            Result.VType := vtString;
            Result.VString^ := VarToStr(aValue);
          end;

       vtPointer:        ;
       vtPChar:          ;
       vtObject:         ;
       vtWideChar:       ;
       vtAnsiString:     ;
       vtCurrency:       ;
       vtVariant:        ;
       vtInterface:      ;
       vtWideString:     ;

       vtInt64:         ;
       vtUnicodeString: ;
  else
      Raise Exception.Create('TNovusVariants.VarToVarRec: Unrecognized variant type: '+ VarTypeAsText(VarType(aValue)) + ':' + IntToStr(VarType(aValue)));
  end;
end;

end.
