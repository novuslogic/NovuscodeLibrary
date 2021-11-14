unit Interpreter;

interface

Uses NovusInterpreter, NovusParser, System.Classes, System.SysUtils;

type
  tEndofStreamTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tCommentsTokenType = class(tTokenType)
  protected
  private
    fbIsMultiLineComment: boolean;
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
    function ParseNextToken: Char; override;

    property IsMultiLineComment: boolean
      read fbIsMultiLineComment;
  end;

  tIdentifierTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tFloatTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tIntegerTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tStringTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tPlusTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tMinusTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tMultTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tDivideTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tPowerTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tDivTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tModTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tLessThanTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tLessThanOrEqualTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tMoreThanTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tMoreThanOrEqualTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tNotEqualTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tRightParenthesisTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tLeftParenthesisTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tLeftBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tRightBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tLeftCurleyBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tRightCurleyBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tEqualsTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tEquivalenceTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tApostrophyTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tDollarTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tSemicolonTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tColonTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tCommaTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tAndTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tOrTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tNotTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tXorTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tEndTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tIfTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tThenTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tElseTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tFalseTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tTrueTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tForTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tDoTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tToTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tDownToTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tWhileTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tRepeatTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tUntilTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tOfTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tBreakTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;

  tFunctionTokenType = class(tTokenType)
  protected
  private
  public
    class function Init(aInterpreter: tNovusInterpreter): tTokenType; override;
  end;






  tIfKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tCommentsKeyword =  class(tKeyword)
  private
  protected
    function GetIsMultiLineComment: boolean;
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;

    property IsMultiLineComment: boolean
      read GetIsMultiLineComment;
  end;

  tDoKeyword =  class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tTokeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tOrKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tOfKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tEndKeyword  = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tForKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tAndKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tXorKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tNotKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tDivKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tModKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tThenKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tElseKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tTrueKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tFalseKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tWhileKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tUntilKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tBreakKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tRepeatKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  TDowntoKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tFunctionKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
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
class function tIfKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tIfKeyword.Create(aTokenType);
end;

//tDoKeyword
class function tDoKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tDoKeyword.Create(aTokenType);
end;

//tCommentskeyword
class function tCommentskeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tCommentskeyword.Create(aTokenType);
end;

function tCommentskeyword.GetIsMultiLineComment: boolean;
begin
  Result :=  (foTokenType as tCommentsTokenType).IsMultiLineComment;
end;

//tTokeyword
class function tTokeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tTokeyword.Create(aTokenType);
end;

//tOrKeyword
class function tOrKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tOrKeyword.Create(aTokenType);
end;

//tOfKeyword
class function tOfKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tOfKeyword.Create(aTokenType);
end;

//tEndKeyword
class function tEndKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tEndKeyword.Create(aTokenType);
end;
//tForKeyword
class function tForKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tForKeyword.Create(aTokenType);
end;

//tAndKeyword
class function tAndKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tAndKeyword.Create(aTokenType);
end;

// tXorKeyword
class function tXorKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tXorKeyword.Create(aTokenType);
end;
//tNotKeyword
class function tNotKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tNotKeyword.Create(aTokenType);
end;

//tDivKeyword
class function tDivKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tDivKeyword.Create(aTokenType);
end;
//tModKeyword
class function tModKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tModKeyword.Create(aTokenType);
end;

//tThenKeyword
class function tThenKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tModKeyword.Create(aTokenType);
end;

//tElseKeyword
class function tElseKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tElseKeyword.Create(aTokenType);
end;

//tTrueKeyword
class function tTrueKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tTrueKeyword.Create(aTokenType);
end;
//tFalseKeyword
class function tFalseKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tTrueKeyword.Create(aTokenType);
end;

//tWhileKeyword
class function tWhileKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tWhileKeyword.Create(aTokenType);
end;
//tUntilKeyword
class function tUntilKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tUntilKeyword.Create(aTokenType);
end;

//tBreakKeyword
class function tBreakKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tBreakKeyword.Create(aTokenType);
end;

//tRepeatKeyword
class function tRepeatKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tRepeatKeyword.Create(aTokenType);
end;

//TDowntoKeyword
class function TDowntoKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := TDowntoKeyword.Create(aTokenType);
end;

//tFunctionKeyword
class function tFunctionKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tFunctionKeyword.Create(aTokenType);
end;

// tEndofStreamTokenType
class function tEndofStreamTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tEndofStreamTokenType.Create(aInterpreter);
end;

// tCommentsTokenType
class function tCommentsTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tCommentsTokenType.Create(aInterpreter);
end;

function tCommentsTokenType.ParseNextToken: Char;
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

// tFloatTokenType
class function tFloatTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tFloatTokenType.Create(aInterpreter);
end;

// tIntegerTokenType
class function tIntegerTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tIntegerTokenType.Create(aInterpreter);
end;

// tStringTokenType
class function tStringTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tStringTokenType.Create(aInterpreter);
end;

// tPlusTokenType
class function tPlusTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tPlusTokenType.Create(aInterpreter);
end;

// tMinusTokenType
class function tMinusTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tMinusTokenType.Create(aInterpreter);
end;

// tMultTokenType
class function tMultTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tMultTokenType.Create(aInterpreter);
end;

// tDivideTokenType
class function tDivideTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tDivideTokenType.Create(aInterpreter);
end;

// tPowerTokenType
class function tPowerTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tPowerTokenType.Create(aInterpreter);
end;

// tDivTokenType
class function tDivTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tDivTokenType.Create(aInterpreter);
end;

// tModTokenType
class function tModTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tModTokenType.Create(aInterpreter);
end;

// tLessThanTokenType
class function tLessThanTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tLessThanTokenType.Create(aInterpreter);
end;


// tLessThanOrEqualTokenType
class function tLessThanOrEqualTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tLessThanOrEqualTokenType.Create(aInterpreter);
end;

// tMoreThanTokenType
class function tMoreThanTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tMoreThanTokenType.Create(aInterpreter);
end;

// tMoreThanOrEqualTokenType
class function tMoreThanOrEqualTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tMoreThanOrEqualTokenType.Create(aInterpreter);
end;

// tNotEqualTokenType
class function tNotEqualTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tMoreThanOrEqualTokenType.Create(aInterpreter);
end;

// tRightParenthesisTokenType
class function tRightParenthesisTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tRightParenthesisTokenType.Create(aInterpreter);
end;

// tLeftParenthesisTokenType
class function tLeftParenthesisTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tLeftParenthesisTokenType.Create(aInterpreter);
end;

// tLeftBracketTokenType
class function tLeftBracketTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tLeftBracketTokenType.Create(aInterpreter);
end;

// tRightBracketTokenType
class function tRightBracketTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tRightBracketTokenType.Create(aInterpreter);
end;

// tLeftCurleyBracketTokenType
class function tLeftCurleyBracketTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tRightBracketTokenType.Create(aInterpreter);
end;

// tRightCurleyBracketTokenType
class function tRightCurleyBracketTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tRightCurleyBracketTokenType.Create(aInterpreter);
end;

// tEqualsTokenType
class function tEqualsTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tEqualsTokenType.Create(aInterpreter);
end;

// tEquivalenceTokenType
class function tEquivalenceTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tEquivalenceTokenType.Create(aInterpreter);
end;

// tApostrophyTokenType
class function tApostrophyTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tApostrophyTokenType.Create(aInterpreter);
end;

// tDollarTokenType
class function tDollarTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tDollarTokenType.Create(aInterpreter);
end;

// tSemicolonTokenType
class function tSemicolonTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tSemicolonTokenType.Create(aInterpreter);
end;

// tColonTokenType
class function tColonTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tColonTokenType.Create(aInterpreter);
end;

// tCommaTokenType
class function tCommaTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tCommaTokenType.Create(aInterpreter);
end;

// tAndTokenType
class function tAndTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tAndTokenType.Create(aInterpreter);
end;

// tOrTokenType
class function tOrTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tOrTokenType.Create(aInterpreter);
end;

// tNotTokenType
class function tNotTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tNotTokenType.Create(aInterpreter);
end;

// tXorTokenType
class function tXorTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tXorTokenType.Create(aInterpreter);
end;

// tEndTokenType
class function tEndTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tEndTokenType.Create(aInterpreter);
end;

// tIfTokenType
class function tIfTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tIfTokenType.Create(aInterpreter);
end;

// tThenTokenType
class function tThenTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tThenTokenType.Create(aInterpreter);
end;

// tElseTokenType
class function tElseTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tElseTokenType.Create(aInterpreter);
end;

// tFalseTokenType
class function tFalseTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tFalseTokenType.Create(aInterpreter);
end;

// tTrueTokenType
class function tTrueTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tTrueTokenType.Create(aInterpreter);
end;

// tForTokenType
class function tForTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tForTokenType.Create(aInterpreter);
end;

// tDoTokenType
class function tDoTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tDoTokenType.Create(aInterpreter);
end;

// tToTokenType
class function tToTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tToTokenType.Create(aInterpreter);
end;

// tDownToTokenType
class function tDownToTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tDownToTokenType.Create(aInterpreter);
end;

// tWhileTokenType
class function tWhileTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tWhileTokenType.Create(aInterpreter);
end;

// tRepeatTokenType
class function tRepeatTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tRepeatTokenType.Create(aInterpreter);
end;

// tUntilTokenType
class function tUntilTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tUntilTokenType.Create(aInterpreter);
end;


// tOfTokenType
class function tOfTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tOfTokenType.Create(aInterpreter);
end;

// tBreakTokenType
class function tBreakTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tBreakTokenType.Create(aInterpreter);
end;


// tFunctionTokenType
class function tFunctionTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tFunctionTokenType.Create(aInterpreter);
end;

// tIdentifierTokenType
class function tIdentifierTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := tIdentifierTokenType.Create(aInterpreter);
end;

function tInterpreter.SkipCommentsToken: Char;
Var
  foCommentsKeyword: tKeyword;
  lch: Char;
begin
  foCommentsKeyword := FindKeyword(tCommentsKeyword.classname);
  if Assigned(foCommentsKeyword) then
   lch := foCommentsKeyword.oTokenType.ParseNextToken;

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

  Result := inherited ;
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
   AddKeyword(tCommentsKeyword.Init(tCommentsTokenType.Init(Self)));
   AddKeyword('if', tIfKeyword.Init(tIfTokenType.Init(Self)));
   AddKeyword('do', tDoKeyword.Init(tDoTokenType.Init(Self)));
   AddKeyword('to', TToKeyword.Init(tToTokenType.Init(Self)));
   AddKeyword('or', TOrKeyword.Init(tOrTokenType.Init(Self)));
   AddKeyword('of', TOfKeyword.Init(tOfTokenType.Init(Self)));
   AddKeyword('end', TendKeyword.Init(tEndTokenType.Init(Self)));
   AddKeyword('for', TforKeyword.Init(tForTokenType.Init(Self)));
   AddKeyword('and', TandKeyword.Init(tForTokenType.Init(Self)));
   AddKeyword('xor', TxorKeyword.Init(tXorTokenType.Init(Self)));
   AddKeyword('not', TNotKeyword.Init(tNotTokenType.Init(Self)));
   AddKeyword('div', TDivKeyword.Init(tDivTokenType.Init(Self)));
   AddKeyword('mod', TModKeyword.Init(tModTokenType.Init(Self)));

   AddKeyword('then', TThenKeyword.Init(tThenTokenType.Init(Self)));
   AddKeyword('else', tElseKeyword.Init(tElseTokenType.Init(Self)));
   AddKeyword('True', tTrueKeyword.Init(tTrueTokenType.Init(Self)));
   AddKeyword('False', tFalseKeyword.Init(tFalseTokenType.Init(Self)));
   AddKeyword('while', tWhileKeyword.Init(tWhileTokenType.Init(Self)));
   AddKeyword('until', tUntilKeyword.Init(tUntilTokenType.Init(Self)));
   AddKeyword('break', tBreakKeyword.Init(tBreakTokenType.Init(Self)));
   AddKeyword('repeat', tRepeatKeyword.Init(tRepeatTokenType.Init(Self)));
   AddKeyword('downto', tDownToKeyword.Init(tDownToTokenType.Init(Self)));
   AddKeyword('function', tFunctionKeyword.Init(tFunctionTokenType.Init(Self)));
end;






end.
