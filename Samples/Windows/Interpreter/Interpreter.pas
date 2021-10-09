unit Interpreter;

interface

Uses NovusInterpreter, NovusParser;

type
  tEndofStreamTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tCommentsTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tIdentifierTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tFloatTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tIntegerTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tStringTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tPlusTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tMinusTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tMultTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tDivideTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tPowerTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tDivTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tModTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tLessThanTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tLessThanOrEqualTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tMoreThanTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tMoreThanOrEqualTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tNotEqualTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tRightParenthesisTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tLeftParenthesisTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tLeftBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tRightBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tLeftCurleyBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tRightCurleyBracketTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tEqualsTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tEquivalenceTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tApostrophyTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tDollarTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tSemicolonTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tColonTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tCommaTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tAndTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tOrTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tNotTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tXorTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tEndTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tIfTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tThenTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tElseTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tFalseTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tTrueTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tForTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tDoTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tToTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tDownToTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tWhileTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tRepeatTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tUntilTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tOfTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tBreakTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
  end;

  tFunctionTokenType = class(tTokenType)
  protected
  private
  public
    class function Init: tTokenType; override;
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
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
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
    fbIsMultiLineComment: boolean;
    function SkipCommentsToken: Char;
  public
    function ParseNextToken: Char; override;
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
class function tEndofStreamTokenType.Init: tTokenType;
begin
  Result := tEndofStreamTokenType.Create;
end;

// tCommentsTokenType
class function tCommentsTokenType.Init: tTokenType;
begin
  Result := tCommentsTokenType.Create;
end;

// tFloatTokenType
class function tFloatTokenType.Init: tTokenType;
begin
  Result := tFloatTokenType.Create;
end;

// tIntegerTokenType
class function tIntegerTokenType.Init: tTokenType;
begin
  Result := tIntegerTokenType.Create;
end;

// tStringTokenType
class function tStringTokenType.Init: tTokenType;
begin
  Result := tStringTokenType.Create;
end;

// tPlusTokenType
class function tPlusTokenType.Init: tTokenType;
begin
  Result := tPlusTokenType.Create;
end;

// tMinusTokenType
class function tMinusTokenType.Init: tTokenType;
begin
  Result := tMinusTokenType.Create;
end;

// tMultTokenType
class function tMultTokenType.Init: tTokenType;
begin
  Result := tMultTokenType.Create;
end;

// tDivideTokenType
class function tDivideTokenType.Init: tTokenType;
begin
  Result := tDivideTokenType.Create;
end;

// tPowerTokenType
class function tPowerTokenType.Init: tTokenType;
begin
  Result := tPowerTokenType.Create;
end;

// tDivTokenType
class function tDivTokenType.Init: tTokenType;
begin
  Result := tDivTokenType.Create;
end;

// tModTokenType
class function tModTokenType.Init: tTokenType;
begin
  Result := tModTokenType.Create;
end;

// tLessThanTokenType
class function tLessThanTokenType.Init: tTokenType;
begin
  Result := tLessThanTokenType.Create;
end;


// tLessThanOrEqualTokenType
class function tLessThanOrEqualTokenType.Init: tTokenType;
begin
  Result := tLessThanOrEqualTokenType.Create;
end;

// tMoreThanTokenType
class function tMoreThanTokenType.Init: tTokenType;
begin
  Result := tMoreThanTokenType.Create;
end;

// tMoreThanOrEqualTokenType
class function tMoreThanOrEqualTokenType.Init: tTokenType;
begin
  Result := tMoreThanOrEqualTokenType.Create;
end;

// tNotEqualTokenType
class function tNotEqualTokenType.Init: tTokenType;
begin
  Result := tMoreThanOrEqualTokenType.Create;
end;

// tRightParenthesisTokenType
class function tRightParenthesisTokenType.Init: tTokenType;
begin
  Result := tRightParenthesisTokenType.Create;
end;

// tLeftParenthesisTokenType
class function tLeftParenthesisTokenType.Init: tTokenType;
begin
  Result := tLeftParenthesisTokenType.Create;
end;

// tLeftBracketTokenType
class function tLeftBracketTokenType.Init: tTokenType;
begin
  Result := tLeftBracketTokenType.Create;
end;

// tRightBracketTokenType
class function tRightBracketTokenType.Init: tTokenType;
begin
  Result := tRightBracketTokenType.Create;
end;

// tLeftCurleyBracketTokenType
class function tLeftCurleyBracketTokenType.Init: tTokenType;
begin
  Result := tRightBracketTokenType.Create;
end;

// tRightCurleyBracketTokenType
class function tRightCurleyBracketTokenType.Init: tTokenType;
begin
  Result := tRightCurleyBracketTokenType.Create;
end;

// tEqualsTokenType
class function tEqualsTokenType.Init: tTokenType;
begin
  Result := tEqualsTokenType.Create;
end;

// tEquivalenceTokenType
class function tEquivalenceTokenType.Init: tTokenType;
begin
  Result := tEquivalenceTokenType.Create;
end;

// tApostrophyTokenType
class function tApostrophyTokenType.Init: tTokenType;
begin
  Result := tApostrophyTokenType.Create;
end;

// tDollarTokenType
class function tDollarTokenType.Init: tTokenType;
begin
  Result := tDollarTokenType.Create;
end;

// tSemicolonTokenType
class function tSemicolonTokenType.Init: tTokenType;
begin
  Result := tSemicolonTokenType.Create;
end;

// tColonTokenType
class function tColonTokenType.Init: tTokenType;
begin
  Result := tColonTokenType.Create;
end;

// tCommaTokenType
class function tCommaTokenType.Init: tTokenType;
begin
  Result := tCommaTokenType.Create;
end;

// tAndTokenType
class function tAndTokenType.Init: tTokenType;
begin
  Result := tAndTokenType.Create;
end;

// tOrTokenType
class function tOrTokenType.Init: tTokenType;
begin
  Result := tOrTokenType.Create;
end;

// tNotTokenType
class function tNotTokenType.Init: tTokenType;
begin
  Result := tNotTokenType.Create;
end;

// tXorTokenType
class function tXorTokenType.Init: tTokenType;
begin
  Result := tXorTokenType.Create;
end;

// tEndTokenType
class function tEndTokenType.Init: tTokenType;
begin
  Result := tEndTokenType.Create;
end;

// tIfTokenType
class function tIfTokenType.Init: tTokenType;
begin
  Result := tIfTokenType.Create;
end;

// tThenTokenType
class function tThenTokenType.Init: tTokenType;
begin
  Result := tThenTokenType.Create;
end;

// tElseTokenType
class function tElseTokenType.Init: tTokenType;
begin
  Result := tElseTokenType.Create;
end;

// tFalseTokenType
class function tFalseTokenType.Init: tTokenType;
begin
  Result := tFalseTokenType.Create;
end;

// tTrueTokenType
class function tTrueTokenType.Init: tTokenType;
begin
  Result := tTrueTokenType.Create;
end;

// tForTokenType
class function tForTokenType.Init: tTokenType;
begin
  Result := tForTokenType.Create;
end;

// tDoTokenType
class function tDoTokenType.Init: tTokenType;
begin
  Result := tDoTokenType.Create;
end;

// tToTokenType
class function tToTokenType.Init: tTokenType;
begin
  Result := tToTokenType.Create;
end;

// tDownToTokenType
class function tDownToTokenType.Init: tTokenType;
begin
  Result := tDownToTokenType.Create;
end;

// tWhileTokenType
class function tWhileTokenType.Init: tTokenType;
begin
  Result := tWhileTokenType.Create;
end;

// tRepeatTokenType
class function tRepeatTokenType.Init: tTokenType;
begin
  Result := tRepeatTokenType.Create;
end;

// tUntilTokenType
class function tUntilTokenType.Init: tTokenType;
begin
  Result := tUntilTokenType.Create;
end;


// tOfTokenType
class function tOfTokenType.Init: tTokenType;
begin
  Result := tOfTokenType.Create;
end;

// tBreakTokenType
class function tBreakTokenType.Init: tTokenType;
begin
  Result := tBreakTokenType.Create;
end;


// tFunctionTokenType
class function tFunctionTokenType.Init: tTokenType;
begin
  Result := tFunctionTokenType.Create;
end;

// tIdentifierTokenType
class function tIdentifierTokenType.Init: tTokenType;
begin
  Result := tIdentifierTokenType.Create;
end;

function tInterpreter.SkipCommentsToken: Char;
Var
  Fch: char;
begin
  Fch := #0;

  if Token = '/' then
    begin
      if PeekJustNextToken = '/' then
        begin
          Result := SkipToEOL;

          Exit;
        end
      else
      if PeekJustNextToken = '*' then
        begin
          fbIsMultiLineComment := true;

          Fch := NextToken;
          while True do
             begin
             while (Fch <> '*') and (Fch <> toEOL) do
                FCh := NextToken;
             if FCh = toEOL then
                exit;
             FCh := NextToken;
             if FCh = '/' then
              begin
                Fch := NextToken;
                fbIsMultiLineComment := False;

                 break;
              end;

             end;
          end;



    end;
end;


function tInterpreter.ParseNextToken: Char;
begin
  SkipCommentsToken;


  Result := inherited ;






end;




procedure tInterpreter.AddKeywords;
begin
   AddKeyword(tCommentsKeyword.Init(tCommentsTokenType.Init));
   AddKeyword('if', tIfKeyword.Init(tIfTokenType.Init));
   AddKeyword('do', tDoKeyword.Init(tDoTokenType.Init));
   AddKeyword('to', TToKeyword.Init(tToTokenType.Init));
   AddKeyword('or', TOrKeyword.Init(tOrTokenType.Init));
   AddKeyword('of', TOfKeyword.Init(tOfTokenType.Init));
   AddKeyword('end', TendKeyword.Init(tEndTokenType.Init));
   AddKeyword('for', TforKeyword.Init(tForTokenType.Init));
   AddKeyword('and', TandKeyword.Init(tForTokenType.Init));
   AddKeyword('xor', TxorKeyword.Init(tXorTokenType.Init));
   AddKeyword('not', TNotKeyword.Init(tNotTokenType.Init));
   AddKeyword('div', TDivKeyword.Init(tDivTokenType.Init));
   AddKeyword('mod', TModKeyword.Init(tModTokenType.Init));

   AddKeyword('then', TThenKeyword.Init(tThenTokenType.Init));
   AddKeyword('else', tElseKeyword.Init(tElseTokenType.Init));
   AddKeyword('True', tTrueKeyword.Init(tTrueTokenType.Init));
   AddKeyword('False', tFalseKeyword.Init(tFalseTokenType.Init));
   AddKeyword('while', tWhileKeyword.Init(tWhileTokenType.Init));
   AddKeyword('until', tUntilKeyword.Init(tUntilTokenType.Init));
   AddKeyword('break', tBreakKeyword.Init(tBreakTokenType.Init));
   AddKeyword('repeat', tRepeatKeyword.Init(tRepeatTokenType.Init));
   AddKeyword('downto', tDownToKeyword.Init(tDownToTokenType.Init));
   AddKeyword('function', tFunctionKeyword.Init(tFunctionTokenType.Init));
end;






end.
