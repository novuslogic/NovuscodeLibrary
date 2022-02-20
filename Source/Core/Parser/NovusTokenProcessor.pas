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
     procedure First;
     procedure Next;
     function GetFirstToken: string;
     function GetNextToken(aIgnoreNextToken: Boolean = false): string; overload;
     function GetNextToken(aTokenIndex: Integer): string; overload;
     function IsNextTokenEquals: boolean;
     function IsNextTokenOpenBracket: boolean;
     function IsNextTokenSemicolon: boolean;
     function IsNextTokenColon: boolean;

     function EOF: Boolean;

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

  if Not aIgnoreNextToken then
    begin
      Inc(fiTokenIndex);
    end;
end;


function tNovusTokenProcessor.GetFirstToken: string;
begin
  First;
  if Count =0 then Exit;
  Result := Trim(Strings[fiTokenIndex]);
  Inc(fiTokenIndex);
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

function tNovusTokenProcessor.IsEqualsToken: Boolean;
begin
   Result := False;
   First;
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

procedure tNovusTokenProcessor.First;
begin
  fiTokenIndex :=0;
end;

procedure tNovusTokenProcessor.Next;
begin
  Inc(fiTokenIndex);

  if fiTokenIndex > Count then
    fiTokenIndex := (Count -1);
end;

end.
