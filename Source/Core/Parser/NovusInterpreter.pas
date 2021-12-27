unit NovusInterpreter;

interface

uses Novuslist, NovusObject, NovusParser, System.Classes, NovusStringUtils,
     System.AnsiStrings;

type
  tTokenList = class(tNovusList);

  tNovusInterpreter = class;

  tParserCell = class;

  tTokenType = (ttOperator, ttKeyword, ttIdentifier);

  tToken = class(tobject)
  private
    fTokenType: tTokenType;
    fsRawToken: string;
    foParserCell: tParserCell;
    fiStartTokenPos: Integer;
    fiEndTokenPos: Integer;
    fiStartSourceLineNo: Integer;
    fiStartColumnPos: Integer;
    fiEndSourceLineNo: Integer;
    fiEndColumnPos: Integer;
  protected
  public
    constructor Create(aParserCell: tParserCell);
    destructor Destroy; override;

    property RawToken: string
      read fsRawToken
      write fsRawToken;

    property oParserCell: tParserCell
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

  tParserCell = class(tobject)
  private
  protected
    foInterpreter: tNovusInterpreter;
  public
    constructor Create(aInterpreter: tNovusInterpreter);

    class function Init(aInterpreter: tNovusInterpreter): tParserCell; virtual;

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

  tKeyword = class(tobject)
  private
  protected
    foParserCell: tParserCell;
    fiIndex: Integer;
    fsKeyName: String;
  public
    property Index: Integer
      read fiIndex
      write fiIndex;

    property KeyName: String
      read fsKeyName
      write fsKeyName;

    constructor Create(aParserCell: tParserCell);
    destructor Destroy; override;

    class function Init(aParserCell: tParserCell): tKeyword; virtual;

    property oParserCell: tParserCell
      read foParserCell
      write foParserCell;

  end;


  tNovusInterpreter = class(tNovusParser)
  private
  protected
    fiStartTokenPos: Integer;
    fiStartSourceLineNo: Integer;
    fiStartColumnPos: Integer;
    foTokenList: tTokenList;
    foKeywordslist: tNovuslist;
    foOperatorslist: tNovuslist;
    function InternalAddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddOperators; virtual;
    procedure AddKeywords; virtual;
    function AddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword; overload;
    function AddKeyword(aKeyword: tkeyword): tkeyword; overload;
    function FindKeyword(aKeyName: String): tkeyword;

    function AddOperator(aOperatorName: string): tOperator; overload;
    function AddOperator(aOperatorName: string; aOperator: tOperator): tOperator; Overload;
    function FindOperator(aOperatorName: string): tOperator;

    procedure GetKeyword; virtual;


    function AddToken(aToken: tToken): tToken; overload;

    function ParseNextToken: Char; virtual;

    procedure Execute; virtual;

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
  foKeywordslist := tNovuslist.Create(tKeyword);

  foKeywordslist.InSensitiveKey := True;

  foTokenList := tTokenList.Create(tToken);

  foOperatorslist := tNovuslist.Create(tOperator);

  foOperatorslist.InSensitiveKey := True;

  AddKeywords;
  AddOperators;
end;

destructor tNovusInterpreter.Destroy;
begin
  foOperatorslist.Free;
  foKeyWordslist.Free;
  foTokenList.Free;

  inherited Destroy;
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

function tNovusInterpreter.AddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
begin
  Result := InternalAddKeyword(aKeyName,aKeyword);
end;

function tNovusInterpreter.AddKeyword(aKeyword: tkeyword): tkeyword;
begin
  Result := InternalAddKeyword(aKeyword.ClassName,aKeyword);
end;


function  tNovusInterpreter.AddToken(aToken: tToken): tToken;
begin
  foTokenList.Add(aToken);

  Result := aToken;
end;

function tNovusInterpreter.FindKeyword(aKeyName: String): tkeyword;
var
  lokeyword: tkeyword;
begin
  Result := NIL;

  lokeyword := foKeyWordslist.FindItem(aKeyname)  as tkeyword ;
  if Assigned(lokeyword) then Result := lokeyword;
end;

function tNovusInterpreter.InternalAddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
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

procedure tNovusInterpreter.Execute;
begin
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
  lokeyword: tkeyword;
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
constructor tParserCell.Create(aInterpreter: tNovusInterpreter);
begin
  foInterpreter := aInterpreter;
end;


class function tParserCell.Init(aInterpreter: tNovusInterpreter): tParserCell;
begin
  Result := NIL;
end;

function tParserCell.ParseNextToken: Char;
begin
  result := #0;
end;

// tKeyword
class function tKeyword.Init(aParserCell: tParserCell): tKeyword;
begin
  Result := NIL;
end;

constructor tKeyword.Create(aParserCell: tParserCell);
begin
  foParserCell := aParserCell;
end;

destructor tKeyword.Destroy;
begin
  if Assigned(foParserCell) then foParserCell.Free;

  inherited Destroy;
end;

// tToken
constructor tToken.Create(aParserCell: tParserCell);
begin
  foParserCell := aParserCell;
end;

destructor tToken.Destroy;
begin
  foParserCell := NIL;

  inherited Destroy;
end;





end.
