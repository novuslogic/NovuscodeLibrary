unit NovusParser;

interface

Uses NovusStringUtils, Classes, SysUtils, NovusBO, NovusObject;

const
  toEOL = char(6);
  toBOF = char(7);
  TAB = #09;

type
  TSysCharSet = set of Char;

  TNovusParser = class(TNovusObject)
  private
    FsParseString: string;
    FiSourceLineNo: Integer;
    FiSourcePos: Integer;
    FiTokenPos: Integer;
    FiColumnPos: Integer;
    FToken: Char;

    function SkipBlanksEx(var aTokenPos: Integer; var aSourceLineNo: Integer): Char;
    procedure SkipBlanks;
    procedure SetSourceLineNo(Value: Integer);

    function GetParseString: string;
    function GetSourcePos: Integer;
    function GetTokenString: string;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    function LoadFromString(const aInput: string): boolean;
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromStream(const aStream: TMemoryStream): Boolean;
    function SkipToEOF: string;
    function SkipToEOLAsString: string; overload;
    function SkipToEOL(aRestColumnPos: boolean = True): Char; overload;

    function NextToken: Char;

    function SkipToken(AStartToken: char = #0; ASecondToken : Char  = #0): Char;
    function SkipTokenString: string;
    function SkipToNonAlpha: String;
    function SkipToToken(const AToken: Char; ASecondToken: Char = #0): string; overload;
    function SkipToToken(const AToken: TSysCharSet): string; overload;
    function SkipToTokenString(const ATokenString: string): string;

    property SourceLineNo: Integer read FiSourceLineNo write SetSourceLineNo;
    property SourcePos: Integer read GetSourcePos write FiSourcePos;
    property ColumnPos: Integer read fiColumnPos write fiColumnPos;
    property Token: Char read FToken write FToken;

    property TokenString: string read GetTokenString;

    property TokenPos: Integer read FiTokenPos write FiTokenPos;

    function GetToken(const s, sDelim: string; var iPos: integer): string;

    function GetLastToken(aoffset: Integer = 0): Char;
    function PeekNextToken(aTokenPos: integer): Char;
    function PeekJustNextToken: Char;

    property ParseString: string read GetParseString;
  end;

implementation

constructor TNovusParser.create;
begin
  Reset;
end;

destructor TNovusParser.destroy;
begin
  //FParseLines.Free;

  inherited destroy;
end;

function TNovusParser.GetToken(const s, sDelim: string; var iPos: integer): string;
var
  sTemp: string;
  iEndPos: integer;
begin
  result := '';
  if (iPos <= 0) or (iPos > Length(s)) then exit;

  sTemp := Copy(s, iPos, Length(s) + 1 - iPos);
  iEndPos := Pos(sDelim, sTemp);
  if iEndPos <= 0 then begin
    result := sTemp;
    iPos := -1;
  end
   else
     begin
      result := Copy(sTemp, 1, iEndPos);
      iPos := iPos + iEndPos + Length(sDelim) - 1;
     end
end;

function TNovusParser.LoadFromString(const aInput: string): Boolean;
begin
  fsParseString := aInput;

  FToken := toBOF;
  Result := True;
end;


function TNovusParser.LoadFromFile(const aFileName: string): Boolean;
var
  LParseLines: tStringList;
begin
  Result := False;
  if not FileExists(aFileName) then  Exit;

  Try
    LParseLines:= tStringList.Create;

    LParseLines.LoadFromFile(aFileName);

    fsParseString := LParseLines.Text;
  Finally
    LParseLines.Free;
  End;

  FToken := toBOF;
  Result := True;
end;

function TNovusParser.LoadFromStream(const aStream: TMemoryStream): Boolean;
Var
  LParseLines: tStringList;
begin
 Result := False;
  if not(assigned(aStream)) then Exit;

  aStream.Seek(0, soFromBeginning);
  Try
    LParseLines := tStringList.Create;

    LParseLines.LoadFromStream(aStream);

    fsParseString := LParseLines.Text;
  Finally
    LParseLines.Free;
  End;


  FToken := toBOF;
  Result := True;
end;

function TNovusParser.SkipBlanksEx(var aTokenPos: Integer; var aSourceLineNo: Integer): char;
begin
  if Trim(ParseString) = '' then Exit;

  while True do
  begin
    Result := ParseString[aTokenPos];
    case Result of
      #10:
        begin
          Inc(aSourceLineNo);
          FiColumnPos := 1;
        end;
      toEOF, #33..#255:
        Exit;
    end;
    Inc(aTokenPos);
  end;
end;

procedure TNovusParser.SkipBlanks;
begin
  FToken := SkipBlanksEx(FiTokenPos, FiSourceLineNo);
  (*
  if Trim(ParseString) = '' then Exit;

  while True do
  begin
    FToken := ParseString[FiTokenPos];
    case FToken of
      #10:
        begin
          Inc(FiSourceLineNo);
         // FLineTokens := FiTokenPos;
        end;
      toEOF, #33..#255:
        Exit;
    end;
    Inc(FiTokenPos);
  end;
  *)
end;

function TNovusParser.GetSourcePos: Integer;
begin
  Result := FiSourcePos - FiTokenPos;
end;

function TNovusParser.GetTokenString: string;
begin
  Result := Copy(ParseString, FiSourcePos, FiTokenPos - FiSourcePos);
  if (Result <> '') and (Result[Length(Result)] in [toEOF, toEOL]) then
    Result := Copy(Result, 1, Length(Result) - 1);
end;


function TNovusParser.SkipToEOF: string;
begin
  FiSourcePos := FiTokenPos;
  while not (ParseString[FiTokenPos] = toEOF) do
    Inc(FiTokenPos);
  FToken := toEOF;
  Result := GetTokenString;
end;

function TNovusParser.SkipToEOLAsString: string;
begin
  FiSourcePos := FiTokenPos;
  while not (ParseString[FiTokenPos] in [toEOF, #10]) do
    Inc(FiTokenPos);
  if ParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else
    FToken := toEOL;
  Result := GetTokenString;
end;

function TNovusParser.NextToken: Char;
begin
  FToken := ParseString[FiTokenPos];

  case FToken of
      #10:
        begin
          Inc(FiSourceLineNo);
          FiColumnPos := 1;
          Inc(FiTokenPos);

          Result := FToken;

          Exit;
        end;
      toEOF:
        Exit;
    end;

  Inc(FiTokenPos);
  inc(FiColumnPos);

  Result := FToken;
end;

function TNovusParser.SkipToEOL(aRestColumnPos: boolean = True): char;
begin
  FiSourcePos := FiTokenPos;
  while not (ParseString[FiTokenPos] in [toEOF, #10]) do
    begin
      Inc(FiTokenPos);
      Inc(fiColumnPos);
    end;
  if ParseString[FiTokenPos] = toEOF then
    begin
      FToken := toEOF;

    end
  else
    begin
      FToken := toEOL;
      Inc(FiTokenPos); // #10


      if aRestColumnPos then
        fiColumnPos := 1;

      Inc(fiSourceLineNo);
    end;

  Result := FToken;
end;


function TNovusParser.SkipToken(AStartToken: char = #0; ASecondToken : Char  = #0): Char;
const
  KeySet = ['A'..'Z', 'a'..'z', '0'..'9', '_'];
begin
  If Trim(ParseString) = '' then
    begin
      FToken := toEOF;

      Exit;
    end;

  SkipBlanks;
  FiSourcePos := FiTokenPos;
  if ParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else
  if ParseString[FiTokenPos] in KeySet then
  begin
    while ParseString[FiTokenPos] in KeySet do
      Inc(FiTokenPos);
    FToken := toSymbol;
  end
  else
  if (AStartToken <> #0) and (ASecondToken <> #0) and
     (ParseString[FiTokenPos] = AStartToken) then
    begin
      FToken := ParseString[FiTokenPos];

      If ParseString[FiTokenPos + 1] = ASecondToken then
        begin
          Inc(FiTokenPos);

          FToken := ParseString[FiTokenPos];

          Inc(FiTokenPos);
        end
      else
        begin
          while ParseString[FiTokenPos] in KeySet do
            Inc(FiTokenPos);

          if (AStartToken <> #0) and (ASecondToken <> #0) and
             (ParseString[FiTokenPos] = AStartToken) then
             Inc(FiTokenPos);

          FToken := toSymbol;
        end;
     end
  else
  begin
    FToken := ParseString[FiTokenPos];
    Inc(FiTokenPos);
  end;
  Result := FToken;

end;

function TNovusParser.SkipTokenString: string;
begin
  SkipToken;
  Result := GetTokenString;
end;

function TNovusParser.SkipToToken(const AToken: Char; ASecondToken: Char = #0): string;
begin
  FiSourcePos := FiTokenPos;

  while not (ParseString[FiTokenPos] in [toEOF, AToken, ASecondToken]) do
  begin
    if ParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FiColumnPos := 1;
    end;
    Inc(FiTokenPos);
  end;

  if ParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else
  if ASecondToken <> #0 then
    FToken := ASecondToken
  else
    FToken := AToken;

  Result := GetTokenString;
  
  if FToken <> toEOF then
    SkipToken;
end;

function TNovusParser.SkipToNonAlpha: string;
begin
  FiSourcePos := FiTokenPos;
  while not (ParseString[FiTokenPos] in [toEOF, #32..#44, #46..#47,
      #58..#64, #91..#94,#96, #123..#126, #$D, #$A]) do
  begin
    if ParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FiColumnPos := 1;
    end;
    Inc(FiTokenPos);
  end;
  if ParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else  FToken := ParseString[FiTokenPos];

  Result := GetTokenString;
  if FToken <> toEOF then
    SkipToken;
end;


function TNovusParser.SkipToToken(const AToken: TSysCharSet): string;
begin
  FiSourcePos := FiTokenPos;
  while not ((ParseString[FiTokenPos] = toEOF) or (ParseString[FiTokenPos] in AToken)) do
  begin
    if ParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FiColumnPos := 1;
    end;
    Inc(FiTokenPos);
  end;
  if ParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else
    FToken := ParseString[FiTokenPos];
  Result := GetTokenString;
  if FToken <> toEOF then
    SkipToken;


end;


function TNovusParser.SkipToTokenString(const ATokenString: string): string;
var
  CmpToken: string;
begin
  FiSourcePos := FiTokenPos;
  CmpToken := '';
  while not (ParseString[FiTokenPos] = toEOF) do
  begin
    if ParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FiColumnPos := 1;
    end;
    CmpToken := Concat(CmpToken, ParseString[FiTokenPos]);
    if Length(CmpToken) > Length(ATokenString) then
      CmpToken := Copy(CmpToken, 2, Length(ATokenString));
    if UpperCase(CmpToken) = UpperCase(ATokenString) then
      Break;
    Inc(FiTokenPos);
  end;
  FToken := ParseString[FiTokenPos];
  Result := GetTokenString;
  if FToken <> toEOF then
    SkipToken;
end;

procedure TNovusParser.Reset;
begin
  FiSourceLineNo := 1;
  FiSourcePos := 1;
  FiTokenPos := 1;
  FiColumnPos := 1;

  FToken := toBOF;
end;

procedure TNovusParser.SetSourceLineNo(Value: Integer);
begin
  FiSourceLineNo := 1;
  FiTokenPos := 1;
  FiColumnPos := 1;

  while True do
  begin
    FToken := ParseString[FiTokenPos];
    case FToken of
      #10:
        begin
          Inc(FiSourceLineNo);
          FiColumnPos := 1;
        end;
      toEOF:
        Exit;
    end;

    Inc(FiTokenPos);

    if FiSourceLineNo = Value then Exit;
  end;
end;

function TNovusParser.GetLastToken(aoffset: Integer = 0): Char;
var
  FiPrevTokenPos:Integer;
begin
  Result := #0;

  FiPrevTokenPos := FiTokenPos -1 + aOffset;
  if FiPrevTokenPos < 0 then Exit;

  Result := ParseString[FiPrevTokenPos];
end;


function tNovusParser.GetParseString: string;
begin
  Result := Copy(fsParseString, 1, Length(fsParseString) - 1);
end;

function tNovusParser.PeekJustNextToken: Char;
begin
  if (ParseString[TokenPos] = toEOF) then
    begin
      Result := toEOF;
      Exit;
    end;

  Result := ParseString[TokenPos];
end;

function tNovusParser.PeekNextToken(aTokenPos: integer): Char;
const
  KeySet = ['A'..'Z', 'a'..'z', '0'..'9', '_'];
Var
  liSourcePos: Integer;
begin
  liSourcePos := 0;

  If Trim(ParseString) = '' then
    begin
      Result := toEOF;

      Exit;
    end;

  Result := SkipBlanksEx(aTokenPos, liSourcePos);

  if ParseString[aTokenPos] = toEOF then
    Result := toEOF
  else
  if ParseString[aTokenPos] in KeySet then
  begin
    while ParseString[aTokenPos] in KeySet do
      Inc(aTokenPos);
    result := toSymbol;
  end
  else
  begin
    Result := ParseString[aTokenPos];
    Inc(aTokenPos);
  end;
end;



end.

