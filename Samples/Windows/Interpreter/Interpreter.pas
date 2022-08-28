unit Interpreter;

interface

Uses NovusInterpreter, NovusParser, System.Classes, System.SysUtils, NovusCommentsParserCell;

type
  tEndofStreamParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tIdentifierParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tFloatParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tIntegerParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tStringParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tPlusParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tMinusParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tMulParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tDivideParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tPowerParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tDivParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tModParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tLessThanParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tLessThanOrEqualParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tMoreThanParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tMoreThanOrEqualParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tNotEqualParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tRightParenthesisParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tLeftParenthesisParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tLeftBrackeParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tRightBrackeParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tLeftCurleyBrackeParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tRightCurleyBrackeParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tEqualsParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tEquivalenceParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tApostrophyParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tDollarParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tSemicolonParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tColonParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tCommaParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tAndParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tOrParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tNoParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tXorParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tEndParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tIfParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tThenParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tElseParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tFalseParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tTrueParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tForParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tDoParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tToParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tDownToParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tWhileParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tRepeaParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tUntilParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tOfParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tBreakParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;

  tFunctionParserCell = class(tNovusParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; override;
  end;






  tIfKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tDoKeyword =  class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tTokeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tOrKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tOfKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tEndKeyword  = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tForKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tAndKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tXorKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tNotKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tDivKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tModKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tThenKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tElseKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tTrueKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tFalseKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tWhileKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tUntilKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tBreakKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tRepeatKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  TDowntoKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tFunctionKeyword = class(tNovusKeyword)
  private
  protected
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;
  end;

  tInterpreter = class(tNovusInterpreter)
  private
  protected
    foLog: tstringList;
    fbIsMultiLineComment: boolean;
    function SkipCommentsToken: Char;
  public
    constructor Create; overload;
    destructor Destroy; override;

    function ParseNextToken: Char; override;
    procedure Execute; override;

    procedure AddKeywords; override;

    property IsMultiLineComment: boolean
      read fbIsMultiLineComment;
  end;

implementation

//tIfKeyword
class function tIfKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tIfKeyword.Create(aParserCell);
end;

//tDoKeyword
class function tDoKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tDoKeyword.Create(aParserCell);
end;


//tTokeyword
class function tTokeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tTokeyword.Create(aParserCell);
end;

//tOrKeyword
class function tOrKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tOrKeyword.Create(aParserCell);
end;

//tOfKeyword
class function tOfKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tOfKeyword.Create(aParserCell);
end;

//tEndKeyword
class function tEndKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tEndKeyword.Create(aParserCell);
end;
//tForKeyword
class function tForKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tForKeyword.Create(aParserCell);
end;

//tAndKeyword
class function tAndKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tAndKeyword.Create(aParserCell);
end;

// tXorKeyword
class function tXorKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tXorKeyword.Create(aParserCell);
end;
//tNotKeyword
class function tNotKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tNotKeyword.Create(aParserCell);
end;

//tDivKeyword
class function tDivKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tDivKeyword.Create(aParserCell);
end;
//tModKeyword
class function tModKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tModKeyword.Create(aParserCell);
end;

//tThenKeyword
class function tThenKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tModKeyword.Create(aParserCell);
end;

//tElseKeyword
class function tElseKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tElseKeyword.Create(aParserCell);
end;

//tTrueKeyword
class function tTrueKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tTrueKeyword.Create(aParserCell);
end;
//tFalseKeyword
class function tFalseKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tTrueKeyword.Create(aParserCell);
end;

//tWhileKeyword
class function tWhileKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tWhileKeyword.Create(aParserCell);
end;
//tUntilKeyword
class function tUntilKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tUntilKeyword.Create(aParserCell);
end;

//tBreakKeyword
class function tBreakKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tBreakKeyword.Create(aParserCell);
end;

//tRepeatKeyword
class function tRepeatKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tRepeatKeyword.Create(aParserCell);
end;

//TDowntoKeyword
class function TDowntoKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := TDowntoKeyword.Create(aParserCell);
end;

//tFunctionKeyword
class function tFunctionKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tFunctionKeyword.Create(aParserCell);
end;

// tEndofStreamParserCell
class function tEndofStreamParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tEndofStreamParserCell.Create(aInterpreter);
end;

// tCommentsParserCell
(*
class function tCommentsParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tCommentsParserCell.Create(aInterpreter);
end;

function tCommentsParserCell.ParseNextToken: Char;
Var
  lch: char;
  lsRawToken: String;
  loToken: tToken;
  liStartTokenPos,
  liStartSourceLineNo,
  liStartColumnPos:integer;
begin
  With foInterpreter do
    begin
      if Token = '/' then
        begin
          if PeekJustNextToken = '/' then
            begin
              loToken := tToken.Create(Self);

              loToken.StartTokenPos := TokenPos -1;

              loToken.StartSourceLineNo := SourceLineNo;
              loToken.StartColumnPos := ColumnPos;

              Result := SkipToEOL(false);
              loToken.EndSourceLineNo := loToken.StartSourceLineNo;

              loToken.EndColumnPos := ColumnPos;

              if Result = toEOL then ColumnPos := 1;

              loToken.EndTokenPos := TokenPos;

              loToken.RawToken := CopyParseString(loToken.StartTokenPos, TokenPos);

              oInterpreter.AddToken(loToken);

              Exit;
            end
          else
          if PeekJustNextToken = '*' then
            begin
              fbIsMultiLineComment := true;

              liStartTokenPos := TokenPos -1 ;
              liStartSourceLineNo := SourceLineNo;
              liStartColumnPos := ColumnPos;

              lch := NextToken;
              while True do
                 begin
                 while (lch <> '*') and (lch <> toEOF) do
                    lch := NextToken;
                 if lch = toEOF then
                    exit;
                 lch := NextToken;
                 if lch = '/' then
                  begin
                    loToken := tToken.Create(self);

                    fbIsMultiLineComment := False;

                    loToken.StartTokenPos := liStartTokenPos;
                    loToken.StartSourceLineNo := liStartSourceLineNo;
                    loToken.StartColumnPos := liStartColumnPos;

                    loToken.EndSourceLineNo := SourceLineNo;
                    loToken.EndColumnPos := ColumnPos;

                    loToken.RawToken := CopyParseString(loToken.StartTokenPos, TokenPos);

                    oInterpreter.AddToken(loToken);

                    break;
                  end;

                 end;
              end;
        end;
    end;
end;
*)

// tFloatParserCell
class function tFloatParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tFloatParserCell.Create(aInterpreter);
end;

// tIntegerParserCell
class function tIntegerParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tIntegerParserCell.Create(aInterpreter);
end;

// tStringParserCell
class function tStringParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tStringParserCell.Create(aInterpreter);
end;

// tPlusParserCell
class function tPlusParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tPlusParserCell.Create(aInterpreter);
end;

// tMinusParserCell
class function tMinusParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tMinusParserCell.Create(aInterpreter);
end;

// tMulParserCell
class function tMulParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tMulParserCell.Create(aInterpreter);
end;

// tDivideParserCell
class function tDivideParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tDivideParserCell.Create(aInterpreter);
end;

// tPowerParserCell
class function tPowerParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tPowerParserCell.Create(aInterpreter);
end;

// tDivParserCell
class function tDivParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tDivParserCell.Create(aInterpreter);
end;

// tModParserCell
class function tModParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tModParserCell.Create(aInterpreter);
end;

// tLessThanParserCell
class function tLessThanParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tLessThanParserCell.Create(aInterpreter);
end;


// tLessThanOrEqualParserCell
class function tLessThanOrEqualParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tLessThanOrEqualParserCell.Create(aInterpreter);
end;

// tMoreThanParserCell
class function tMoreThanParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tMoreThanParserCell.Create(aInterpreter);
end;

// tMoreThanOrEqualParserCell
class function tMoreThanOrEqualParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tMoreThanOrEqualParserCell.Create(aInterpreter);
end;

// tNotEqualParserCell
class function tNotEqualParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tMoreThanOrEqualParserCell.Create(aInterpreter);
end;

// tRightParenthesisParserCell
class function tRightParenthesisParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tRightParenthesisParserCell.Create(aInterpreter);
end;

// tLeftParenthesisParserCell
class function tLeftParenthesisParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tLeftParenthesisParserCell.Create(aInterpreter);
end;

// tLeftBrackeParserCell
class function tLeftBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tLeftBrackeParserCell.Create(aInterpreter);
end;

// tRightBrackeParserCell
class function tRightBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tRightBrackeParserCell.Create(aInterpreter);
end;

// tLeftCurleyBrackeParserCell
class function tLeftCurleyBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tRightBrackeParserCell.Create(aInterpreter);
end;

// tRightCurleyBrackeParserCell
class function tRightCurleyBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tRightCurleyBrackeParserCell.Create(aInterpreter);
end;

// tEqualsParserCell
class function tEqualsParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tEqualsParserCell.Create(aInterpreter);
end;

// tEquivalenceParserCell
class function tEquivalenceParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tEquivalenceParserCell.Create(aInterpreter);
end;

// tApostrophyParserCell
class function tApostrophyParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tApostrophyParserCell.Create(aInterpreter);
end;

// tDollarParserCell
class function tDollarParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tDollarParserCell.Create(aInterpreter);
end;

// tSemicolonParserCell
class function tSemicolonParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tSemicolonParserCell.Create(aInterpreter);
end;

// tColonParserCell
class function tColonParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tColonParserCell.Create(aInterpreter);
end;

// tCommaParserCell
class function tCommaParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tCommaParserCell.Create(aInterpreter);
end;

// tAndParserCell
class function tAndParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tAndParserCell.Create(aInterpreter);
end;

// tOrParserCell
class function tOrParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tOrParserCell.Create(aInterpreter);
end;

// tNoParserCell
class function tNoParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tNoParserCell.Create(aInterpreter);
end;

// tXorParserCell
class function tXorParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tXorParserCell.Create(aInterpreter);
end;

// tEndParserCell
class function tEndParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tEndParserCell.Create(aInterpreter);
end;

// tIfParserCell
class function tIfParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tIfParserCell.Create(aInterpreter);
end;

// tThenParserCell
class function tThenParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tThenParserCell.Create(aInterpreter);
end;

// tElseParserCell
class function tElseParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tElseParserCell.Create(aInterpreter);
end;

// tFalseParserCell
class function tFalseParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tFalseParserCell.Create(aInterpreter);
end;

// tTrueParserCell
class function tTrueParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tTrueParserCell.Create(aInterpreter);
end;

// tForParserCell
class function tForParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tForParserCell.Create(aInterpreter);
end;

// tDoParserCell
class function tDoParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tDoParserCell.Create(aInterpreter);
end;

// tToParserCell
class function tToParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tToParserCell.Create(aInterpreter);
end;

// tDownToParserCell
class function tDownToParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tDownToParserCell.Create(aInterpreter);
end;

// tWhileParserCell
class function tWhileParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tWhileParserCell.Create(aInterpreter);
end;

// tRepeaParserCell
class function tRepeaParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tRepeaParserCell.Create(aInterpreter);
end;

// tUntilParserCell
class function tUntilParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tUntilParserCell.Create(aInterpreter);
end;


// tOfParserCell
class function tOfParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tOfParserCell.Create(aInterpreter);
end;

// tBreakParserCell
class function tBreakParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tBreakParserCell.Create(aInterpreter);
end;


// tFunctionParserCell
class function tFunctionParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tFunctionParserCell.Create(aInterpreter);
end;

// tIdentifierParserCell
class function tIdentifierParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := tIdentifierParserCell.Create(aInterpreter);
end;

function tInterpreter.SkipCommentsToken: Char;
Var
  foCommentsKeyword: tNovusKeyword;
  lch: Char;
begin
  foCommentsKeyword := FindKeyword(tNovusCommentsKeyword.classname);
  if Assigned(foCommentsKeyword) then
   lch := foCommentsKeyword.oParserCell.ParseNextToken;

  fbIsMultiLineComment := tNovusCommentsKeyword(foCommentsKeyword as tNovusCommentsKeyword).IsMultiLineComment;

  Result := Token;
end;


// tInterpreter
constructor tInterpreter.Create;
begin
  foLog := tstringList.Create;

  inherited Create;
end;

destructor tInterpreter.Destroy;
begin
  foLog.Free;

  inherited Destroy;
end;


function tInterpreter.ParseNextToken: Char;
begin
  SkipCommentsToken;

  SkipBlanks;

  fiStartTokenPos := TokenPos -1 ;
  fiStartSourceLineNo := SourceLineNo;
  fiStartColumnPos := ColumnPos;

  Result := inherited ;

  GetKeyword;

end;

procedure tInterpreter.Execute;
begin
  inherited ;

  if fbIsMultiLineComment then
    begin
      foLog.Add('detected unterminated comment, expecting "*/"');
    end;
end;

procedure tInterpreter.AddKeywords;
begin
   AddKeyword(tNovusCommentsKeyword.Init(tNovusCommentsParserCell.Init(Self,tncPascal)));
   AddKeyword('if', tIfKeyword.Init(tIfParserCell.Init(Self)));
   AddKeyword('do', tDoKeyword.Init(tDoParserCell.Init(Self)));
   AddKeyword('to', TToKeyword.Init(tToParserCell.Init(Self)));
   AddKeyword('or', TOrKeyword.Init(tOrParserCell.Init(Self)));
   AddKeyword('of', TOfKeyword.Init(tOfParserCell.Init(Self)));
   AddKeyword('end', TendKeyword.Init(tEndParserCell.Init(Self)));
   AddKeyword('for', TforKeyword.Init(tForParserCell.Init(Self)));
   AddKeyword('and', TandKeyword.Init(tForParserCell.Init(Self)));
   AddKeyword('xor', TxorKeyword.Init(tXorParserCell.Init(Self)));
   AddKeyword('not', TNotKeyword.Init(tNoParserCell.Init(Self)));
   AddKeyword('div', TDivKeyword.Init(tDivParserCell.Init(Self)));
   AddKeyword('mod', TModKeyword.Init(tModParserCell.Init(Self)));

   AddKeyword('then', TThenKeyword.Init(tThenParserCell.Init(Self)));
   AddKeyword('else', tElseKeyword.Init(tElseParserCell.Init(Self)));
   AddKeyword('True', tTrueKeyword.Init(tTrueParserCell.Init(Self)));
   AddKeyword('False', tFalseKeyword.Init(tFalseParserCell.Init(Self)));
   AddKeyword('while', tWhileKeyword.Init(tWhileParserCell.Init(Self)));
   AddKeyword('until', tUntilKeyword.Init(tUntilParserCell.Init(Self)));
   AddKeyword('break', tBreakKeyword.Init(tBreakParserCell.Init(Self)));
   AddKeyword('repeat', tRepeatKeyword.Init(tRepeaParserCell.Init(Self)));
   AddKeyword('downto', tDownToKeyword.Init(tDownToParserCell.Init(Self)));
   AddKeyword('function', tFunctionKeyword.Init(tFunctionParserCell.Init(Self)));
end;






end.
