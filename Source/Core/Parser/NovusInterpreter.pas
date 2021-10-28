unit NovusInterpreter;

interface

uses Novuslist, NovusObject, NovusParser, System.Classes;

type
  tNovusInterpreter = class;

  tToken = class(tobject)
  private
  protected
  public
  end;

  tTokenType = class(tobject)
  private
  protected
    foInterpreter: tNovusInterpreter;
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
    fKeyWordslist: tNovuslist;
    function InternalAddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddKeywords; virtual;
    function AddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword; overload;
    function AddKeyword(aKeyword: tkeyword): tkeyword; overload;
    function FindKeyword(aKeyName: String): tkeyword;


    function ParseNextToken: Char; virtual;

    function Execute: Boolean;
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
begin
  Result := InternalAddKeyword(aKeyName,aKeyword);
end;

function tNovusInterpreter.AddKeyword(aKeyword: tkeyword): tkeyword;
begin
  Result := InternalAddKeyword(aKeyword.ClassName,aKeyword);
end;

function tNovusInterpreter.FindKeyword(aKeyName: String): tkeyword;
var
  lokeyword: tkeyword;
begin
  Result := NIL;

  lokeyword := fKeyWordslist.FindItem(aKeyname)  as tkeyword ;
  if Assigned(lokeyword) then Result := lokeyword;
end;



function tNovusInterpreter.InternalAddKeyword(aKeyName: String;aKeyword: tkeyword): tkeyword;
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


end.
