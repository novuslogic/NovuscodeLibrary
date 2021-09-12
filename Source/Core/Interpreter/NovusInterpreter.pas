unit NovusInterpreter;

interface

uses Novuslist, NovusObject;

type
  tTokenType = class(tobject)
  private
  protected
  public
    class function Init: tTokenType; virtual;
  end;

  tKeyword = class(tobject)
  private
  protected
    foTokenType: tTokenType;
    fiIndex: Integer;
    fsKeyName: String;
  public
    property Index: Integer
      read fiIndex
      write fiIndex;

    property KeyName: String
      read fsKeyName
      write fsKeyName;

    constructor Create(aTokenType: tTokenType);
    destructor Destroy; override;

    class function Init(aTokenType: tTokenType): tKeyword; virtual;
  end;

  tEndofStreamTokenType = class(tTokenType)
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


  tNovusInterpreter = class(tNovusObject)
  private
  protected
    fKeyWordslist: tNovuslist;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddKeywords; virtual;
    function AddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
  end;

implementation

constructor tNovusInterpreter.Create;
begin
  fKeyWordslist := tNovuslist.Create(tKeyword);

  AddKeywords;
end;

destructor tNovusInterpreter.Destroy;
begin
  fKeyWordslist.Free;

  inherited Destroy;
end;



procedure tNovusInterpreter.AddKeywords;
begin

end;

function tNovusInterpreter.AddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
Var
  liIndex: Integer;
begin
  Result := NIL;

  if Assigned(aKeyword) then
    begin
      liIndex := fKeyWordslist.Add(aKeyName, aKeyword);

      Result := aKeyword;

      Result.Index := liIndex;
      Result.KeyName := aKeyName;
    end;
end;

// tTokenType
class function tTokenType.Init: tTokenType;
begin
  Result := NIL;
end;

// tKeyword
class function tKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := NIL;
end;

constructor tKeyword.Create(aTokenType: tTokenType);
begin
  foTokenType := aTokenType;
end;

destructor tKeyword.Destroy;
begin
  if Assigned(foTokenType) then foTokenType.Free;

  inherited Destroy;
end;

// tEndofStreamTokenType
class function tEndofStreamTokenType.Init: tTokenType;
begin
  Result := tEndofStreamTokenType.Create;
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

end.
