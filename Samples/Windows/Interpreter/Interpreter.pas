unit Interpreter;

interface

Uses NovusInterpreter;

type
  tIfKeyword = class(tKeyword)
  private
  protected
  public
    class function Init(aTokenType: tTokenType): tKeyword; override;
  end;

  tDoKeyword =  class(tKeyword);
  tTokeyword = class(tKeyword);
  tOrKeyword = class(tKeyword);
  tOfKeyword = class(tKeyword);
  tEndKeyword  = class(tKeyword);
  tForKeyword = class(tKeyword);
  tAndKeyword = class(tKeyword);
  tXorKeyword = class(tKeyword);
  tNotKeyword = class(tKeyword);
  tDivKeyword = class(tKeyword);
  tModKeyword = class(tKeyword);
  tThenKeyword = class(tKeyword);
  tElseKeyword = class(tKeyword);
  tTrueKeyword = class(tKeyword);
  tFalseKeyword = class(tKeyword);
  tWhileKeyword = class(tKeyword);
  tUntilKeyword = class(tKeyword);
  tBreakKeyword = class(tKeyword);
  tRepeatKeyword = class(tKeyword);
  TDowntoKeyword = class(tKeyword);
  tFunctionKeyword = class(tKeyword);



  tInterpreter = class(tNovusInterpreter)
  private
  protected
  public
    procedure AddKeywords; override;
  end;

implementation

//tIfKeyword
class function tIfKeyword.Init(aTokenType: tTokenType): tKeyword;
begin
  Result := tIfKeyword.Create(aTokenType);
end;

procedure tInterpreter.AddKeywords;
begin
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
