unit NovusInterpreter;

interface

uses Novuslist, NovusObject, NovusParser, System.Classes, vcl.dialogs;

type
  tTokenList = class(tNovusList)

  end;

  tNovusInterpreter = class;

  tParserCell = class;

  tToken = class(tobject)
  private
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
    foTokenList: tTokenList;
    foKeywordslist: tNovuslist;
    function InternalAddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddKeywords; virtual;
    function AddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword; overload;
    function AddKeyword(aKeyword: tkeyword): tkeyword; overload;
    function FindKeyword(aKeyName: String): tkeyword;
    function AddToken(aToken: tToken): tToken; overload;


    function ParseNextToken: Char; virtual;

    procedure Execute; virtual;

    property oTokenList: tTokenList
      read foTokenList;

  end;

implementation

constructor tNovusInterpreter.Create;
begin
  foKeywordslist := tNovuslist.Create(tKeyword);
  foTokenList := tTokenList.Create(tToken);


  AddKeywords;
end;

destructor tNovusInterpreter.Destroy;
begin
  foKeyWordslist.Free;
  foTokenList.Free;

  inherited Destroy;
end;

procedure tNovusInterpreter.AddKeywords;
begin
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
