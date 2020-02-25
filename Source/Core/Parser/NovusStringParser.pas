unit NovusStringParser;

interface

Uses NovusUtilities, NovusStringUtils, NovusParser, Classes,
  SysUtils, StrUtils;

type
  tNovusStringParser = Class(tNovusParser)
  private
    fbIncludeWordSeparatorSet: Boolean;
    FItems: TStringList;
    stText: String;
    stFindString: String;
    stFindPosition: Integer;

    procedure ParseString(aIncludeWordSeparatorSet: Boolean);
    procedure SetText(const Value: String);
    function GetCount: Integer;

  Public

    constructor Create(AText: String; aIncludeWordSeparatorSet: Boolean = false);
    destructor Destroy; override;

    function Replace(fromStr, toStr: String): Integer;

    function FindFirst(search: String): Integer;

    function FindNext: Integer;

    property Text: String read stText write SetText;

    property Count: Integer read GetCount;

    property Items: TStringList read FItems write FItems;

    property IncludeWordSeparatorSet: boolean
      read fbIncludeWordSeparatorSet write fbIncludeWordSeparatorSet;
  end;

implementation

constructor tNovusStringParser.Create(AText: String; aIncludeWordSeparatorSet: Boolean = false);
begin
  FItems := TStringList.Create;

  stText := AText;
  stFindPosition := 1;
  stFindString := '';
  ParseString(aIncludeWordSeparatorSet);
end;

destructor tNovusStringParser.Destroy;
begin
  inherited Destroy;
end;

function tNovusStringParser.GetCount: Integer;
begin
  result := Items.Count;
end;

procedure tNovusStringParser.SetText(const Value: String);
begin
  stText := Value;
  stFindPosition := 1;
  ParseString(fbIncludeWordSeparatorSet);
end;

function tNovusStringParser.FindFirst(search: String): Integer;
begin
  stFindString := search;
  stFindPosition := 1;

  Result := FindNext;
end;

function tNovusStringParser.FindNext: Integer;
var
  index: Integer;
  findSize: Integer;
begin

  if Length(stFindString) = 0 then
    Result := -2
  else
  begin
    findSize := Length(stFindString);

    Result := -1;

    index := stFindPosition;

    while (index <= Length(stText)) and (Result < 0) do
    begin
      if stText[index] = stFindString[1] then
      begin
        if AnsiMidStr(stText, index, findSize) = stFindString then
          Result := index;
      end;
      Inc(index);
    end;

    stFindPosition := index
  end;
end;

function tNovusStringParser.Replace(fromStr, toStr: String): Integer;
var
  fromSize, count, index: Integer;
  newText: String;
  matched: Boolean;
begin
  fromSize := Length(fromStr);

  count := 0;

  newText := '';
  index := 1;

  while index <= Length(stText) do
  begin
    matched := false;

    if stText[index] = fromStr[1] then
    begin
      if AnsiMidStr(stText, index, fromSize) = fromStr then
      begin
        Inc(count);

        newText := newText + toStr;

        Inc(index, fromSize);

        matched := true;
      end;
    end;

    if not matched then
    begin
      newText := newText + stText[index];
      Inc(index);
    end;
  end;

  if count > 0 then
    stText := newText;

  Result := count;
end;

procedure tNovusStringParser.ParseString(aIncludeWordSeparatorSet: Boolean);
const
  LF = #10;
  TAB = #9;
  CR = #13;
  BLANK = #32;
  SEMICOL = ';';
  EQUALS = '=';
var
  lsWord: String;
  WordSeparatorSet: Set of Char;
  index: Integer;
  inWord: Boolean;
begin
  WordSeparatorSet := [LF, TAB, CR, BLANK, SEMICOL, EQUALS];

  inWord := false;

  lsWord := '';

  for index := 1 to Length(stText) do
  begin

    if stText[index] In WordSeparatorSet then
    begin
      if inWord then
      begin
        FItems.Add(lsWord);

        if aIncludeWordSeparatorSet then
          begin
            FItems.Add(stText[index]);
          end;

        lsWord := '';
      end;

      inWord := false;
    end
    else

      inWord := true;

    if inWord then
      lsWord := lsWord + stText[index];
  end;

  if inWord then
  begin
    FItems.Add(lsWord);
  end;
end;

end.
