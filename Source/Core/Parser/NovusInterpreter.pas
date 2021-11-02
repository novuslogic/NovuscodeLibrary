unit NovusInterpreter;

interface

uses Novuslist, NovusObject, NovusParser, System.Classes;

type
  tNovusInterpreter = class;

  tTokenType = class;

  tToken = class(tobject)
  private
    fsRawToken: string;
    foTokenType: tTokenType;
  protected
  public
    constructor Create(aTokenType: tTokenType);
    destructor Destroy; override;

    property RawToken: string
      read fsRawToken
      write fsRawToken;

    property oTokenType: tTokenType
      read foTokenType
      write foTokenType;
  end;

  tTokenType = class(tobject)
  private
  protected
    foInterpreter: tNovusInterpreter;
    fiStartTokenPos: Integer;
    fiEndTokenPos: Integer;
    fiStartSourceLineNo: Integer;
    fiStartColumnPos: Integer;
    fiEndSourceLineNo: Integer;
    fiEndColumnPos: Integer;
  public
    constructor Create(aInterpreter: tNovusInterpreter);

    class function Init(aInterpreter: tNovusInterpreter): tTokenType; virtual;

    function ParseNextToken: Char; virtual;

    property oInterpreter: tNovusInterpreter
       read foInterpreter
       write foInterpreter;

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

    property oTokenType: tTokenType
      read foTokenType
      write foTokenType;

  end;


  tNovusInterpreter = class(tNovusParser)
  private
  protected
    foTokenList: tNovusList;
    foKeywordslist: tNovuslist;
    function InternalAddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddKeywords; virtual;
    function AddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword; overload;
    function AddKeyword(aKeyword: tkeyword): tkeyword; overload;
    function FindKeyword(aKeyName: String): tkeyword;
    function AddToken(aToken: tToken): tToken; overload;


    function ParseNextToken: Char; virtual;

    function Execute: Boolean; virtual;
  end;

implementation

constructor tNovusInterpreter.Create;
begin
  foKeywordslist := tNovuslist.Create(tKeyword);
  foTokenList := tNovusList.Create(tToken);


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
  Result := SkipToken;
end;


function tNovusInterpreter.Execute: Boolean;
begin
  Result := False;
  Reset;

  while True do
    begin
      while not(Token in [toEOF]) do
          ParseNextToken;

      if Token = toEOF then
        begin
          Result := True;
          Break;
        end;
    end;
end;


// tTokenType
constructor tTokenType.Create(aInterpreter: tNovusInterpreter);
begin
  foInterpreter := aInterpreter;
end;


class function tTokenType.Init(aInterpreter: tNovusInterpreter): tTokenType;
begin
  Result := NIL;
end;

function tTokenType.ParseNextToken: Char;
begin
  result := #0;
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

// tToken
constructor tToken.Create(aTokenType: tTokenType);
begin
  foTokenType := aTokenType;
end;

destructor tToken.Destroy;
begin
  foTokenType := NIL;

  inherited Destroy;
end;





end.
