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

  tEndofStreamTokenType = class(tTokenType);
  tIdentifierTokenType = class(tTokenType);
  tFloatTokenType = class(tTokenType);
  tIntegerTokenType = class(tTokenType);
  tStringTokenType = class(tTokenType);
  tPlusTokenType = class(tTokenType);
  tMinusTokenType = class(tTokenType);
  tMultTokenType = class(tTokenType);
  tDivideTokenType = class(tTokenType);
  tPowerTokenType = class(tTokenType);
  tDivTokenType = class(tTokenType);
  tModTokenType = class(tTokenType);
  tLessThanTokenType = class(tTokenType);
  tLessThanOrEqualTokenType = class(tTokenType);
  tMoreThanTokenType = class(tTokenType);
  tMoreThanOrEqualTokenType = class(tTokenType);
  tNotEqualTokenType = class(tTokenType);
  tRightParenthesisTokenType = class(tTokenType);
  tLeftParenthesisTokenType = class(tTokenType);
  tLeftBracketTokenType = class(tTokenType);
  tRightBracketTokenType = class(tTokenType);
  tLeftCurleyBracketTokenType = class(tTokenType);
  tRightCurleyBracketTokenType = class(tTokenType);
  tEqualsTokenType = class(tTokenType);
  tEquivalenceTokenType = class(tTokenType);
  tApostrophyTokenType = class(tTokenType);
  tDollarTokenType = class(tTokenType);
  tSemicolonTokenType = class(tTokenType);
  tColonTokenType = class(tTokenType);
  tCommaTokenType = class(tTokenType);
  tAndTokenType = class(tTokenType);
  tOrTokenType = class(tTokenType);
  tNotTokenType = class(tTokenType);
  tXorTokenType = class(tTokenType);
  tEndTokenType = class(tTokenType);
  tIfTokenType = class(tTokenType);
  tThenTokenType = class(tTokenType);
  tElseTokenType = class(tTokenType);
  tFalseTokenType = class(tTokenType);
  tTrueTokenType = class(tTokenType);
  tForTokenType = class(tTokenType);
  tDoTokenType = class(tTokenType);
  tToTokenType = class(tTokenType);
  tDownToTokenType = class(tTokenType);
  tWhileTokenType = class(tTokenType);
  tRepeatTokenType = class(tTokenType);
  tUntilTokenType = class(tTokenType);
  tOfTokenType = class(tTokenType);
  tBreakTokenType = class(tTokenType);
  tFunctionTokenType = class(tTokenType);


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

end.
