unit NovusParser;

interface

Uses NovusStringUtils, Classes, SysUtils, NovusBO;

const
  toEOL = char(6);
  toBOF = char(7);

type
  TSysCharSet = set of Char;

  TNovusParser = class(TNovusBO)
  private
    FParseLines: TStringList;
    FParseString: string;
    FLineTokens: Integer;
    FiSourceLineNo: Integer;
    FiSourcePos: Integer;
    FiTokenPos: Integer;
    FToken: Char;

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
    function LoadFromFile(const FileName: string): Boolean;
    function LoadFromStream(const Stream: TMemoryStream): Boolean;
    function SkipToEOF: string;
    function SkipToEOL: string;
    function SkipToken(AStartToken: char = #0; ASecondToken : Char  = #0): Char;
    function SkipTokenString: string;
    function SkipToNonAlpha: String;
    function SkipToToken(const AToken: Char; ASecondToken: Char = #0): string; overload;
    function SkipToToken(const AToken: TSysCharSet): string; overload;
    function SkipToTokenString(const ATokenString: string): string;
    property ParseString: string read GetParseString;
    property SourceLineNo: Integer read FiSourceLineNo write SetSourceLineNo;
    property SourcePos: Integer read GetSourcePos write FiSourcePos;
    property Token: Char read FToken write FToken;

    property TokenString: string read GetTokenString;

    property TokenPos: Integer read FiTokenPos write FiTokenPos;

    function GetToken(const s, sDelim: string; var iPos: integer): string;

    function GetLastToken(aoffset: Integer = 0): Char;
  end;

implementation

constructor TNovusParser.create;
begin
  Reset;
end;

destructor TNovusParser.destroy;
begin
  FParseLines.Free;

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


function TNovusParser.LoadFromFile(const FileName: string): Boolean;
//var
//  Stream: TMemoryStream;
begin
  Result := False;
  if not FileExists(FileName) then  Exit;
  FParseLines.LoadFromFile(FileName);

  FParseString := FParseLines.Text;

  FToken := toBOF;
  Result := True;
end;

function TNovusParser.LoadFromStream(const Stream: TMemoryStream): Boolean;
begin
  Result := False;
  if not(assigned(Stream)) then Exit;

  Stream.Seek(0, soFromBeginning);
  FParseLines.LoadFromStream(Stream);

  FParseString := FParseLines.Text;

  FToken := toBOF;
  Result := True;
end;



procedure TNovusParser.SkipBlanks;
begin
  while True do
  begin
    FToken := FParseString[FiTokenPos];
    case FToken of
      #10:
        begin
          Inc(FiSourceLineNo);
          FLineTokens := FiTokenPos;
        end;
      toEOF, #33..#255:
        Exit;
    end;
    Inc(FiTokenPos);
  end;
end;


function TNovusParser.GetParseString: string;
begin
  Result := Copy(FParseString, 1, Length(FParseString) - 1);
end;

function TNovusParser.GetSourcePos: Integer;
begin
  Result := FiSourcePos - FLineTokens;
end;

function TNovusParser.GetTokenString: string;
begin
  Result := Copy(FParseString, FiSourcePos, FiTokenPos - FiSourcePos);
  if (Result <> '') and (Result[Length(Result)] in [toEOF, toEOL]) then
    Result := Copy(Result, 1, Length(Result) - 1);
end;


function TNovusParser.SkipToEOF: string;
begin
  FiSourcePos := FiTokenPos;
  while not (FParseString[FiTokenPos] = toEOF) do
    Inc(FiTokenPos);
  FToken := toEOF;
  Result := GetTokenString;
end;

function TNovusParser.SkipToEOL: string;
begin
  FiSourcePos := FiTokenPos;
  while not (FParseString[FiTokenPos] in [toEOF, #10]) do
    Inc(FiTokenPos);
  if FParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else
    FToken := toEOL;
  Result := GetTokenString;
end;

function TNovusParser.SkipToken(AStartToken: char = #0; ASecondToken : Char  = #0): Char;
const
  KeySet = ['A'..'Z', 'a'..'z', '0'..'9', '_'];
begin
  SkipBlanks;
  FiSourcePos := FiTokenPos;
  if FParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else
  if FParseString[FiTokenPos] in KeySet then
  begin
    while FParseString[FiTokenPos] in KeySet do
      Inc(FiTokenPos);
    FToken := toSymbol;
  end
  else
  if (AStartToken <> #0) and (ASecondToken <> #0) and
     (FParseString[FiTokenPos] = AStartToken) then
    begin
      FToken := FParseString[FiTokenPos];

      If FParseString[FiTokenPos + 1] = ASecondToken then
        begin
          Inc(FiTokenPos);

          FToken := FParseString[FiTokenPos];

          Inc(FiTokenPos);
        end
      else
        begin
          while FParseString[FiTokenPos] in KeySet do
            Inc(FiTokenPos);

          if (AStartToken <> #0) and (ASecondToken <> #0) and
             (FParseString[FiTokenPos] = AStartToken) then
             Inc(FiTokenPos);

          FToken := toSymbol;
        end;
     end
  else
  begin
    FToken := FParseString[FiTokenPos];
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

  while not (FParseString[FiTokenPos] in [toEOF, AToken, ASecondToken]) do
  begin
    if FParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FLineTokens := FiTokenPos;
    end;
    Inc(FiTokenPos);
  end;

  if FParseString[FiTokenPos] = toEOF then
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
  while not (FParseString[FiTokenPos] in [toEOF, #32..#44, #46..#47,
      #58..#64, #91..#94,#96, #123..#126, #$D, #$A]) do
  begin
    if FParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FLineTokens := FiTokenPos;
    end;
    Inc(FiTokenPos);
  end;
  if FParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else  FToken := FParseString[FiTokenPos];

  Result := GetTokenString;
  if FToken <> toEOF then
    SkipToken;
end;


function TNovusParser.SkipToToken(const AToken: TSysCharSet): string;
begin
  FiSourcePos := FiTokenPos;
  while not ((FParseString[FiTokenPos] = toEOF) or (FParseString[FiTokenPos] in AToken)) do
  begin
    if FParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FLineTokens := FiTokenPos;
    end;
    Inc(FiTokenPos);
  end;
  if FParseString[FiTokenPos] = toEOF then
    FToken := toEOF
  else
    FToken := FParseString[FiTokenPos];
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
  while not (FParseString[FiTokenPos] = toEOF) do
  begin
    if FParseString[FiTokenPos] = #10 then
    begin
      Inc(FiSourceLineNo);
      FLineTokens := FiTokenPos;
    end;
    CmpToken := Concat(CmpToken, FParseString[FiTokenPos]);
    if Length(CmpToken) > Length(ATokenString) then
      CmpToken := Copy(CmpToken, 2, Length(ATokenString));
    if UpperCase(CmpToken) = UpperCase(ATokenString) then
      Break;
    Inc(FiTokenPos);
  end;
  FToken := FParseString[FiTokenPos];
  Result := GetTokenString;
  if FToken <> toEOF then
    SkipToken;
end;

procedure TNovusParser.Reset;
begin
  if Assigned(FParseLines) then FParseLines.Free;
  FParseLines:= TStringList.Create;

  FiSourceLineNo := 1;
  FiSourcePos := 1;
  FiTokenPos := 1;
  FToken := toBOF;
end;

procedure TNovusParser.SetSourceLineNo(Value: Integer);
begin
  FiSourceLineNo := 1;
  FiTokenPos := 1;

  while True do
  begin
    FToken := FParseString[FiTokenPos];
    case FToken of
      #10:
        begin
          Inc(FiSourceLineNo);
          FLineTokens := FiTokenPos;
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

  Result := FParseString[FiPrevTokenPos];
end;




end.

