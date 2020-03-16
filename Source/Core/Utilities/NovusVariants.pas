unit NovusVariants;

interface

uses NovusUtilities, variants;

type
  TNovusVariants = class(TNovusUtilities)
  protected
  private
  public
    class function VarToVarRec(aValue: variant): TVarRec;
  end;

implementation

class function TNovusVariants.VarToVarRec(aValue: variant): TVarRec;
var
  VString: PShortString;
  VWideString: PWideString;
  VVariant: PVariant;
begin
   New(VString);
   VString^ := String(aValue);
   Result.VType := vtString;
   Result.VString := VString;
   Dispose(VString);


            (*

  case VarType(aValue) of
       vtInteger:       ;
       vtBoolean:       ;
       vtChar:          ;
       vtExtended:      ;
       vtString:
          begin
            New(VString);
            VString^ := String(aValue);
            Result.VType := vtString;
            Result.VString := VString;
            Dispose(VString);
          end;

       vtPointer:        ;
       vtPChar:          ;
       vtObject:         ;
       vtWideChar:       ;
       vtAnsiString:     ;
       vtCurrency:       ;
       vtVariant:
         begin
           New(VVariant);
           VVariant^ := aValue;
           Result.VType := vtVariant;
           Result.VVariant := VVariant;
           Dispose(VVariant);
         end;
       vtInterface:     ;
       vtWideString:
         begin
           New(VWideString);
           VWideString^ := String(aValue);
           Result.VType := vtWideString;
           Result.VWideString := VWideString;
           Dispose(VWideString);
         end;

       vtInt64:         ;
       vtUnicodeString: ;
  else
    begin
      Raise Exception.Create('TNovusVariants.VarToVarRec: Unrecognized variant type');
    end;

  end;
         *)

end;

end.
