unit NovusTokenProcessor;

interface

Uses SysUtils, Classes;

type
   tNovusTokenProcessor = class(TStringList)
   private
     fsLine: String;
     fiTokenIndex: Integer;
   protected
   public
     constructor Create; overload;

     function ToString: String; override;
     function IsEqualsToken: Boolean;
     procedure FirstToken;
     procedure NextToken;
     function GetFirstToken(aIgnoreNextToken: Boolean = false): string;
     function GetNextToken(aIgnoreNextToken: Boolean = false): string; overload;
     function GetNextToken(aTokenIndex: Integer): string; overload;
     function IsNextTokenEquals: boolean;
     function IsNextTokenOpenBracket: boolean;
     function IsNextTokenSemicolon: boolean;
     function IsNextTokenColon: boolean;
     function IsNextToken(aToken: String): boolean;
     function FindToken(aToken: string): Boolean;
     function CurrentToken: String;

     function EOF: Boolean;
     function BOF: boolean;

     property TokenIndex: Integer
         read fiTokenIndex
         write fiTokenIndex;

   end;

implementation

// Token Processor
constructor tNovusTokenProcessor.Create;
begin
  fiTokenIndex:= 0;
end;


function tNovusTokenProcessor.BOF: boolean;
begin
  Result := (fiTokenIndex = -1);
end;

function tNovusTokenProcessor.EOF: Boolean;
begin
  Result := (fiTokenIndex >= Count);
end;

function tNovusTokenProcessor.GetNextToken(aIgnoreNextToken: Boolean): String;
begin
  Result := '';

  if fiTokenIndex > Count then  Exit;

  if Count > 0 then
    Result := Trim(Strings[fiTokenIndex]);

  if aIgnoreNextToken = false then Inc(fiTokenIndex);
end;


function tNovusTokenProcessor.GetFirstToken(aIgnoreNextToken: Boolean = false): string;
begin
  fiTokenIndex := 0;
  if Count =0 then Exit;
  Result := Trim(Strings[fiTokenIndex]);

  if aIgnoreNextToken = false then Inc(fiTokenIndex);
end;

function tNovusTokenProcessor.GetNextToken(aTokenIndex: Integer): string;
begin
  Result := '';

  fiTokenIndex := aTokenIndex;

  if aTokenIndex > Count then  Exit;

  if Count > 0 then
    Result := Trim(Strings[aTokenIndex]);
end;


function tNovusTokenProcessor.IsNextTokenEquals: boolean;
begin
  Result := GetNextToken = '=';
end;

function tNovusTokenProcessor.IsNextTokenOpenBracket: Boolean;
begin
  Result := GetNextToken = '(';
end;


function tNovusTokenProcessor.IsNextTokenSemicolon: boolean;
begin
  Result := GetNextToken = ';';
end;

function tNovusTokenProcessor.IsNextTokenColon: boolean;
begin
  Result := GetNextToken = ':';
end;

function tNovusTokenProcessor.IsNextToken(aToken: String): boolean;
begin
  Result := GetNextToken = aToken;
end;

function tNovusTokenProcessor.FindToken(aToken: string): boolean;
begin
  fiTokenIndex := (Self.IndexOf(aToken));
  Result := Not BOF;
end;

function tNovusTokenProcessor.IsEqualsToken: Boolean;
begin
   Result := False;
   FirstToken;
   while(not EOF) do
     begin

       if IsNextTokenEquals then
         begin
           Result := true;
           Break;
         end;
     end;
end;


function tNovusTokenProcessor.ToString: String;
Var
  I: Integer;
begin
  Result := '';

  for I := 0 to Self.Count -1 do
     Result := Result +Trim(Strings[i]);
end;

procedure tNovusTokenProcessor.FirstToken;
begin
  fiTokenIndex :=0;
end;

function tNovusTokenProcessor.CurrentToken: string;
begin
  Result := '';
  if EOF then Exit;

  Result := Strings[fiTokenIndex];
end;


procedure tNovusTokenProcessor.NextToken;
begin
  Inc(fiTokenIndex);

  if fiTokenIndex > Count then
    fiTokenIndex := (Count -1);
end;

end.
