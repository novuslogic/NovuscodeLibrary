unit Interpreter;

interface

Uses NovusInterpreter, NovusParser, System.Classes, System.SysUtils;

type
  tEndofStreamParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tCommentsParserCell = class(tParserCell)
  protected
  private
    fbIsMultiLineComment: boolean;
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
    function ParseNextToken: Char; override;

    property IsMultiLineComment: boolean
      read fbIsMultiLineComment;
  end;

  tIdentifierParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tFloatParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tIntegerParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tStringParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tPlusParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tMinusParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tMulParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tDivideParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tPowerParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tDivParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tModParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tLessThanParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tLessThanOrEqualParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tMoreThanParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tMoreThanOrEqualParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tNotEqualParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tRightParenthesisParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tLeftParenthesisParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tLeftBrackeParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tRightBrackeParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tLeftCurleyBrackeParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tRightCurleyBrackeParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tEqualsParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tEquivalenceParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tApostrophyParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tDollarParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tSemicolonParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tColonParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tCommaParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tAndParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tOrParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tNoParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tXorParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tEndParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tIfParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tThenParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tElseParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tFalseParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tTrueParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tForParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tDoParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tToParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tDownToParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tWhileParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tRepeaParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tUntilParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tOfParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tBreakParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;

  tFunctionParserCell = class(tParserCell)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tParserCell; override;
  end;






  tIfKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tCommentsKeyword =  class(tKeyword)
  private
  protected
    function GetIsMultiLineComment: boolean;
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;

    property IsMultiLineComment: boolean
      read GetIsMultiLineComment;
  end;

  tDoKeyword =  class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tTokeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tOrKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tOfKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tEndKeyword  = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tForKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tAndKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tXorKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tNotKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tDivKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tModKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tThenKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tElseKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tTrueKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tFalseKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tWhileKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tUntilKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tBreakKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tRepeatKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  TDowntoKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tFunctionKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aParserCell: tParserCell): tKeyword; override;
  end;

  tInterpreter = class(tNovusInterpreter)
  private
  protected
    fiStartTokenPos: Integer;
    fiStartSourceLineNo: Integer;
    fiStartColumnPos: Integer;
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
class function tIfKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tIfKeyword.Create(aParserCell);
end;

//tDoKeyword
class function tDoKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tDoKeyword.Create(aParserCell);
end;

//tCommentskeyword
class function tCommentskeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tCommentskeyword.Create(aParserCell);
end;

function tCommentskeyword.GetIsMultiLineComment: boolean;
begin
  Result :=  (foParserCell as tCommentsParserCell).IsMultiLineComment;
end;

//tTokeyword
class function tTokeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tTokeyword.Create(aParserCell);
end;

//tOrKeyword
class function tOrKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tOrKeyword.Create(aParserCell);
end;

//tOfKeyword
class function tOfKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tOfKeyword.Create(aParserCell);
end;

//tEndKeyword
class function tEndKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tEndKeyword.Create(aParserCell);
end;
//tForKeyword
class function tForKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tForKeyword.Create(aParserCell);
end;

//tAndKeyword
class function tAndKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tAndKeyword.Create(aParserCell);
end;

// tXorKeyword
class function tXorKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tXorKeyword.Create(aParserCell);
end;
//tNotKeyword
class function tNotKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tNotKeyword.Create(aParserCell);
end;

//tDivKeyword
class function tDivKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tDivKeyword.Create(aParserCell);
end;
//tModKeyword
class function tModKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tModKeyword.Create(aParserCell);
end;

//tThenKeyword
class function tThenKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tModKeyword.Create(aParserCell);
end;

//tElseKeyword
class function tElseKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tElseKeyword.Create(aParserCell);
end;

//tTrueKeyword
class function tTrueKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tTrueKeyword.Create(aParserCell);
end;
//tFalseKeyword
class function tFalseKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tTrueKeyword.Create(aParserCell);
end;

//tWhileKeyword
class function tWhileKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tWhileKeyword.Create(aParserCell);
end;
//tUntilKeyword
class function tUntilKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tUntilKeyword.Create(aParserCell);
end;

//tBreakKeyword
class function tBreakKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tBreakKeyword.Create(aParserCell);
end;

//tRepeatKeyword
class function tRepeatKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tRepeatKeyword.Create(aParserCell);
end;

//TDowntoKeyword
class function TDowntoKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := TDowntoKeyword.Create(aParserCell);
end;

//tFunctionKeyword
class function tFunctionKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := tFunctionKeyword.Create(aParserCell);
end;

// tEndofStreamParserCell
class function tEndofStreamParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tEndofStreamParserCell.Create(aInterpreter);
end;

// tCommentsParserCell
class function tCommentsParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
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

// tFloatParserCell
class function tFloatParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tFloatParserCell.Create(aInterpreter);
end;

// tIntegerParserCell
class function tIntegerParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tIntegerParserCell.Create(aInterpreter);
end;

// tStringParserCell
class function tStringParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tStringParserCell.Create(aInterpreter);
end;

// tPlusParserCell
class function tPlusParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tPlusParserCell.Create(aInterpreter);
end;

// tMinusParserCell
class function tMinusParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tMinusParserCell.Create(aInterpreter);
end;

// tMulParserCell
class function tMulParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tMulParserCell.Create(aInterpreter);
end;

// tDivideParserCell
class function tDivideParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tDivideParserCell.Create(aInterpreter);
end;

// tPowerParserCell
class function tPowerParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tPowerParserCell.Create(aInterpreter);
end;

// tDivParserCell
class function tDivParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tDivParserCell.Create(aInterpreter);
end;

// tModParserCell
class function tModParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tModParserCell.Create(aInterpreter);
end;

// tLessThanParserCell
class function tLessThanParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tLessThanParserCell.Create(aInterpreter);
end;


// tLessThanOrEqualParserCell
class function tLessThanOrEqualParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tLessThanOrEqualParserCell.Create(aInterpreter);
end;

// tMoreThanParserCell
class function tMoreThanParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tMoreThanParserCell.Create(aInterpreter);
end;

// tMoreThanOrEqualParserCell
class function tMoreThanOrEqualParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tMoreThanOrEqualParserCell.Create(aInterpreter);
end;

// tNotEqualParserCell
class function tNotEqualParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tMoreThanOrEqualParserCell.Create(aInterpreter);
end;

// tRightParenthesisParserCell
class function tRightParenthesisParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tRightParenthesisParserCell.Create(aInterpreter);
end;

// tLeftParenthesisParserCell
class function tLeftParenthesisParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tLeftParenthesisParserCell.Create(aInterpreter);
end;

// tLeftBrackeParserCell
class function tLeftBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tLeftBrackeParserCell.Create(aInterpreter);
end;

// tRightBrackeParserCell
class function tRightBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tRightBrackeParserCell.Create(aInterpreter);
end;

// tLeftCurleyBrackeParserCell
class function tLeftCurleyBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tRightBrackeParserCell.Create(aInterpreter);
end;

// tRightCurleyBrackeParserCell
class function tRightCurleyBrackeParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tRightCurleyBrackeParserCell.Create(aInterpreter);
end;

// tEqualsParserCell
class function tEqualsParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tEqualsParserCell.Create(aInterpreter);
end;

// tEquivalenceParserCell
class function tEquivalenceParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tEquivalenceParserCell.Create(aInterpreter);
end;

// tApostrophyParserCell
class function tApostrophyParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tApostrophyParserCell.Create(aInterpreter);
end;

// tDollarParserCell
class function tDollarParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tDollarParserCell.Create(aInterpreter);
end;

// tSemicolonParserCell
class function tSemicolonParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tSemicolonParserCell.Create(aInterpreter);
end;

// tColonParserCell
class function tColonParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tColonParserCell.Create(aInterpreter);
end;

// tCommaParserCell
class function tCommaParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tCommaParserCell.Create(aInterpreter);
end;

// tAndParserCell
class function tAndParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tAndParserCell.Create(aInterpreter);
end;

// tOrParserCell
class function tOrParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tOrParserCell.Create(aInterpreter);
end;

// tNoParserCell
class function tNoParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tNoParserCell.Create(aInterpreter);
end;

// tXorParserCell
class function tXorParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tXorParserCell.Create(aInterpreter);
end;

// tEndParserCell
class function tEndParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tEndParserCell.Create(aInterpreter);
end;

// tIfParserCell
class function tIfParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tIfParserCell.Create(aInterpreter);
end;

// tThenParserCell
class function tThenParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tThenParserCell.Create(aInterpreter);
end;

// tElseParserCell
class function tElseParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tElseParserCell.Create(aInterpreter);
end;

// tFalseParserCell
class function tFalseParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tFalseParserCell.Create(aInterpreter);
end;

// tTrueParserCell
class function tTrueParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tTrueParserCell.Create(aInterpreter);
end;

// tForParserCell
class function tForParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tForParserCell.Create(aInterpreter);
end;

// tDoParserCell
class function tDoParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tDoParserCell.Create(aInterpreter);
end;

// tToParserCell
class function tToParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tToParserCell.Create(aInterpreter);
end;

// tDownToParserCell
class function tDownToParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tDownToParserCell.Create(aInterpreter);
end;

// tWhileParserCell
class function tWhileParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tWhileParserCell.Create(aInterpreter);
end;

// tRepeaParserCell
class function tRepeaParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tRepeaParserCell.Create(aInterpreter);
end;

// tUntilParserCell
class function tUntilParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tUntilParserCell.Create(aInterpreter);
end;


// tOfParserCell
class function tOfParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tOfParserCell.Create(aInterpreter);
end;

// tBreakParserCell
class function tBreakParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tBreakParserCell.Create(aInterpreter);
end;


// tFunctionParserCell
class function tFunctionParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tFunctionParserCell.Create(aInterpreter);
end;

// tIdentifierParserCell
class function tIdentifierParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := tIdentifierParserCell.Create(aInterpreter);
end;

function tInterpreter.SkipCommentsToken: Char;
Var
  foCommentsKeyword: tKeyword;
  lch: Char;
begin
  foCommentsKeyword := FindKeyword(tCommentsKeyword.classname);
  if Assigned(foCommentsKeyword) then
   lch := foCommentsKeyword.oParserCell.ParseNextToken;

  fbIsMultiLineComment := tCommentsKeyword(foCommentsKeyword as tCommentsKeyword).IsMultiLineComment;

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
   AddKeyword(tCommentsKeyword.Init(tCommentsParserCell.Init(Self)));
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
