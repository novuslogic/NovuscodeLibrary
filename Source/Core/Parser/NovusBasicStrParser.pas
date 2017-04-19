unit NovusBasicStrParser;

interface

Uses NovusUtilities, NovusStringUtils, NovusParser, Classes,
  SysUtils, StrUtils;

type
  tNovusBasicStrParser = Class(tNovusParser)
  private
    FItems: TStringList;
    stText: String;
    stWordCount: Integer;
    stFindString: String;
    stFindPosition: Integer;

    procedure GetWordCount;
    procedure SetText(const Value: String);

  published
    constructor Create(AText: String);
    destructor Destroy; override;

    function Replace(fromStr, toStr: String): Integer;
    function FindFirst(search: String): Integer;
    function FindNext: Integer;
    property Text: String read stText write SetText;

    property WordCount: Integer read stWordCount;

    property Items: TStringList read FItems write FItems;
  end;

implementation

constructor tNovusBasicStrParser.Create(AText: String);
begin
  FItems := TStringList.Create;

  stText := AText;
  stFindPosition := 1;
  stFindString := '';
  GetWordCount;
end;

destructor tNovusBasicStrParser.Destroy;
begin
  inherited Destroy;
end;

procedure tNovusBasicStrParser.SetText(const Value: String);
begin
  stText := Value;
  stFindPosition := 1;
  GetWordCount;
end;

function tNovusBasicStrParser.FindFirst(search: String): Integer;
begin
  stFindString := search;
  stFindPosition := 1;

  Result := FindNext;
end;

function tNovusBasicStrParser.FindNext: Integer;
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

function tNovusBasicStrParser.Replace(fromStr, toStr: String): Integer;
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

procedure tNovusBasicStrParser.GetWordCount;
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
  WordSeparatorSet := [LF, TAB, CR, BLANK, SEMICOL];
  stWordCount := 0;
  inWord := false;

  lsWord := '';

  for index := 1 to Length(stText) do
  begin
    if stText[index] In WordSeparatorSet then
    begin
      if inWord then
      begin
        FItems.Add(lsWord);
        Inc(stWordCount);
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

    Inc(stWordCount);
  end;
end;

end.
