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
  public
    procedure AddKeywords; override;
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
