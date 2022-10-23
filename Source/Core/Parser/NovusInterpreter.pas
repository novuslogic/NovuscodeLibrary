unit NovusInterpreter;

interface

uses Novuslist, NovusObject, NovusParser, System.Classes, NovusStringUtils,
     System.AnsiStrings, NovusTemplate;

type
  tTokenList = class(tNovusList);

  tNovusInterpreter = class;

  tNovusParserCell = class;

  tTokenType = (ttOperator, ttKeyword, ttIdentifier);

  tToken = class(tobject)
  private
    fTokenType: tTokenType;
    fsRawToken: string;
    foParserCell: tNovusParserCell;
    fiStartTokenPos: Integer;
    fiEndTokenPos: Integer;
    fiStartSourceLineNo: Integer;
    fiStartColumnPos: Integer;
    fiEndSourceLineNo: Integer;
    fiEndColumnPos: Integer;
  protected
  public
    constructor Create(aParserCell: tNovusParserCell);
    destructor Destroy; override;

    property RawToken: string
      read fsRawToken
      write fsRawToken;

    property oParserCell: tNovusParserCell
      read foParserCell
      write foParserCell;

    property StartSourceLineNo: Integer
      read  fiStartSourceLineNo
      write fiStartSourceLineNo;

    property StartColumnPos: Integer
      read fiStartColumnPos
      write fiStartColumnPos;

    property EndSourceLineNo: Integer
      read  fiEndSourceLineNo
      write fiEndSourceLineNo;

    property EndColumnPos: Integer
      read fiEndColumnPos
      write fiEndColumnPos;

    property StartTokenPos: Integer
      read fiStartTokenPos
      write fiStartTokenPos;

    property EndTokenPos: Integer
      read fiEndTokenPos
      write fiEndTokenPos;

    property TokenType: tTokenType
       read fTokenType
       write fTokenType;
  end;

  tNovusParserCell = class(tobject)
  private
  protected
    foInterpreter: tNovusInterpreter;
  public
    constructor Create(aInterpreter: tNovusInterpreter);

    class function Init(aInterpreter: tNovusInterpreter): tNovusParserCell; virtual;

    function ParseNextToken: Char; virtual;

    property oInterpreter: tNovusInterpreter
       read foInterpreter
       write foInterpreter;
  end;

  tOperator = class(tobject)
  private
    fsOperatorName: string;
  protected
  public
    property OperatorName: String
      read fsOperatorName
      write fsOperatorName;
  end;

  tNovusKeyword = class(tobject)
  private
  protected
    foParserCell: tNovusParserCell;
    fiIndex: Integer;
    fsKeyName: String;
  public
    property Index: Integer
      read fiIndex
      write fiIndex;

    property KeyName: String
      read fsKeyName
      write fsKeyName;

    constructor Create(aParserCell: tNovusParserCell);
    destructor Destroy; override;

    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; virtual;

    property oParserCell: tNovusParserCell
      read foParserCell
      write foParserCell;

  end;


  tNovusInterpreter = class(TNovusParser )
  private
  protected
    fiStartTokenPos: Integer;
    fiStartSourceLineNo: Integer;
    fiStartColumnPos: Integer;
    foTokenList: tTokenList;
    foKeywordslist: tNovuslist;
    foOperatorslist: tNovuslist;
    function InternalAddKeyword(aKeyName: String;aKeyword: tNovusKeyword): tNovusKeyword;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddOperators; virtual;
    procedure AddKeywords; virtual;
    function AddKeyword(aKeyName: String;aKeyword: tNovusKeyword): tNovusKeyword; overload;
    function AddKeyword(aKeyword: tNovusKeyword): tNovusKeyword; overload;
    function FindKeyword(aKeyName: String): tNovusKeyword;

    function AddOperator(aOperatorName: string): tOperator; overload;
    function AddOperator(aOperatorName: string; aOperator: tOperator): tOperator; Overload;
    function FindOperator(aOperatorName: string): tOperator;

    procedure GetKeyword; virtual;


    function AddToken(aToken: tToken): tToken; overload;

    function ParseNextToken: Char; virtual;

    function ParseInterpreter: boolean; virtual;

    property oTokenList: tTokenList
      read foTokenList;

    property StartTokenPos: Integer
      read fiStartTokenPos
      write fiStartTokenPos;

    property StartSourceLineNo: Integer
      read fiStartSourceLineNo
      write fiStartSourceLineNo;

    property StartColumnPos: Integer
      read fiStartColumnPos
      write fiStartColumnPos;

  end;

implementation

constructor tNovusInterpreter.Create;
begin
  inherited Create;

  foKeywordslist := tNovuslist.Create(tNovusKeyword);

  foKeywordslist.InSensitiveKey := True;

  foTokenList := tTokenList.Create(tToken);

  foOperatorslist := tNovuslist.Create(tOperator);

  foOperatorslist.InSensitiveKey := True;

  AddKeywords;
  AddOperators;
end;

destructor tNovusInterpreter.Destroy;
begin
  inherited Destroy;

  foOperatorslist.Free;
  foKeyWordslist.Free;
  foTokenList.Free;
end;

procedure tNovusInterpreter.AddKeywords;
begin
end;

procedure tNovusInterpreter.AddOperators;
begin
   AddOperator('+');
   AddOperator('^');
   AddOperator('(');
   AddOperator(')');
   AddOperator('[');
   AddOperator(']');
   AddOperator('{');
   AddOperator('}');
   AddOperator('!');
   AddOperator('>');
   AddOperator('<');
   AddOperator(';');
   AddOperator(':');
   AddOperator(',');
   AddOperator('''');
   AddOperator('-');
   AddOperator('/');
   AddOperator('*');
end;

function tNovusInterpreter.AddKeyword(aKeyName: String;aKeyword: tNovusKeyword): tNovusKeyword;
begin
  Result := InternalAddKeyword(aKeyName,aKeyword);
end;

function tNovusInterpreter.AddKeyword(aKeyword: tNovusKeyword): tNovusKeyword;
begin
  Result := InternalAddKeyword(aKeyword.ClassName,aKeyword);
end;


function  tNovusInterpreter.AddToken(aToken: tToken): tToken;
begin
  foTokenList.Add(aToken);

  Result := aToken;
end;

function tNovusInterpreter.FindKeyword(aKeyName: String): tNovusKeyword;
var
  lokeyword: tNovusKeyword;
begin
  Result := NIL;

  lokeyword := foKeyWordslist.FindItem(aKeyname)  as tNovusKeyword ;
  if Assigned(lokeyword) then Result := lokeyword;
end;

function tNovusInterpreter.InternalAddKeyword(aKeyName: String;aKeyword: tNovusKeyword): tNovusKeyword;
Var
  liIndex: Integer;
begin
  Result := NIL;

  if Assigned(aKeyword) then
    begin
      liIndex := foKeywordslist.Add(aKeyName, aKeyword);

      Result := aKeyword;

      Result.Index := liIndex;
      Result.KeyName := aKeyName;
    end;
end;

function tNovusInterpreter.ParseNextToken: Char;
begin
  Result := NextToken;
end;

function tNovusInterpreter.ParseInterpreter: boolean;
begin
  Result := true;

  Reset;

  while True do
    begin
      while not(Token in [toEOF]) do
        begin
          ParseNextToken;
        end;

      if Token = toEOF then
        begin
          Break;
        end;
    end;
end;

function tNovusInterpreter.AddOperator(aOperatorName: string; aOperator: tOperator): tOperator;
begin
  Result := NIL;
end;

function tNovusInterpreter.AddOperator(aOperatorName: string): tOperator;
begin
  Result := tOperator.Create;

  Result.OperatorName := aOperatorName;

  foOperatorslist.Add(aOperatorName, Result);
end;

function tNovusInterpreter.FindOperator(aOperatorName: string): tOperator;
var
  loOperator: tOperator;
begin
  Result := NIL;

  loOperator := foOperatorslist.FindItem(aOperatorName)  as tOperator;
  if Assigned(loOperator) then Result := loOperator;
end;





procedure tNovusInterpreter.GetKeyword;
var
  lch: char;
  lsKeyName: String;
  lokeyword: tNovusKeyword;
  loOperator: tOperator;
  loToken: tToken;
begin
  lsKeyName := '';
  lokeyword := nil;

  case Token of
     'a'..'z','A'..'Z','_' :
       begin
         while (TNovusStringUtils.IsAlphaChar(token) or
               TNovusStringUtils.IsNumericChar(Token)) do
            begin
              lsKeyName := lsKeyName + Token;

              if Token = toEOF then  exit;
              lch := NextToken;
            end;

         if lsKeyName <> '' then
           begin
             lokeyword := FindKeyword(lsKeyName);
             if Assigned(lokeyword) then
               begin
                               (*
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
              *)

               end;
             loOperator := FindOperator(lsKeyName);
             if Assigned(loOperator) then
               begin



               end;


             loToken := tToken.Create(NIL);

             loToken.StartTokenPos := StartTokenPos;

             loToken.StartSourceLineNo := StartSourceLineNo;
             loToken.StartColumnPos := StartColumnPos;

             lch := NextToken;
             while (lch in ['a'..'z','A'..'Z','_']) do
               lch := NextToken;



             loToken.EndSourceLineNo := Self.SourceLineNo;

             loToken.EndColumnPos := ColumnPos;

             loToken.EndTokenPos := TokenPos -1;

             loToken.RawToken := Trim(CopyParseString(loToken.StartTokenPos, loToken.EndTokenPos));

              (*
              Result := SkipToEOL(false);
              loToken.EndSourceLineNo := loToken.StartSourceLineNo;

              loToken.EndColumnPos := ColumnPos;

              if Result = toEOL then ColumnPos := 1;

              loToken.EndTokenPos := TokenPos;

              loToken.RawToken := CopyParseString(loToken.StartTokenPos, TokenPos);

              oInterpreter.AddToken(loToken);
             *)




           end





       end;
  end;



end;



// tParserCell
constructor tNovusParserCell.Create(aInterpreter: tNovusInterpreter);
begin
  foInterpreter := aInterpreter;
end;


class function tNovusParserCell.Init(aInterpreter: tNovusInterpreter): tNovusParserCell;
begin
  Result := NIL;
end;

function tNovusParserCell.ParseNextToken: Char;
begin
  result := #0;
end;

// tKeyword
class function tNovusKeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := NIL;
end;

constructor tNovusKeyword.Create(aParserCell: tNovusParserCell);
begin
  foParserCell := aParserCell;
end;

destructor tNovusKeyword.Destroy;
begin
  if Assigned(foParserCell) then foParserCell.Free;

  inherited Destroy;
end;

// tToken
constructor tToken.Create(aParserCell: tNovusParserCell);
begin
  foParserCell := aParserCell;
end;

destructor tToken.Destroy;
begin
  foParserCell := NIL;

  inherited Destroy;
end;





end.
