unit NovusCommandLine;

interface

uses
  NovusConsole, SysUtils, Classes, NovusList, System.StrUtils, vcl.dialogs;

type
  TNovusCommandLineResult = class(TObject)
  protected
  private
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);
  public
    property Errors: Boolean read GetErrors write SetErrors;
  end;

  tNovusCommandLineParam = class(TObject)
  protected
    fsHelp: string;
    fsCommandName: String;
    fsShortCommandName: String;
  private
    function GetCommandName: string;
    procedure SetCommandName(Value: string);
    function GetShortCommandName: string;
    procedure SetShortCommandName(Value: string);
    function GetHelp: string;
    procedure SetHelp(Value: string);
  public
    property CommandName: string read GetCommandName write SetCommandName;
    property ShortCommandName: string read GetShortCommandName
      write SetShortCommandName;
    property Help: string read GetHelp write SetHelp;
    function Execute: Boolean; virtual;
  end;

  tNovusCommandLine = class
  protected
  class var
    FCommandList: TNovusList;
    FParamStrList: tStringlist;
    fiParamIndex: Integer;
  private
    class function ParamStrToStringList: tStringlist;
    class function GetNextCommand: string;
    class function FindCommandName(aName: string): tNovusCommandLineParam;
  public
    class constructor Create;
    class destructor Destroy;

    class function Parse: TNovusCommandLineResult;

    class function RegisterCommand(const aCommandName: string;
      const aShortCommandName: string; const aHelp: String;
      aCommandLineParam: tNovusCommandLineParam): tNovusCommandLineParam;



  end;

implementation

class constructor tNovusCommandLine.Create;
begin
  FCommandList := TNovusList.Create;
end;

class destructor tNovusCommandLine.Destroy;
begin
  if Assigned(FParamStrList) then
    FParamStrList.Free;

  FCommandList.Free;
end;

class function tNovusCommandLine.Parse: TNovusCommandLineResult;
Var
  lsCommandName: string;
begin
  Result := NIL;

  FParamStrList := tNovusCommandLine.ParamStrToStringList;
  fiParamIndex := 0;

  lsCommandName := tNovusCommandLine.GetNextCommand;

end;

class function tNovusCommandLine.ParamStrToStringList: tStringlist;
Var
  i: Integer;
begin
  Result := tStringlist.Create;

  if ParamCount > 0 then
  begin
    for i := 1 to ParamCount do
      Result.Add(ParamStr(i));
  end;
end;

class function tNovusCommandLine.FindCommandName(aName: string)
  : tNovusCommandLineParam;
Var
  fCommandLineParam: tNovusCommandLineParam;
  i: Integer;
begin
  Result := NIL;
  For i := 0 to FCommandList.count - 1 do
  begin
    fCommandLineParam := FCommandList.items[i] as tNovusCommandLineParam;

    if (Uppercase(fCommandLineParam.CommandName) = Uppercase(aName)) or
      (Uppercase(fCommandLineParam.ShortCommandName) = Uppercase(aName)) then
    begin
      Result := fCommandLineParam;

      break;
    end;

  end;
end;

class function tNovusCommandLine.GetNextCommand: string;
Var
  i: integer;
  lsParamValue: string;
  lCommandLineParam: tNovusCommandLineParam;
begin
  Result := '';

  for I := 0 to FParamStrList.Count -1 do
    begin
      lsParamValue := FParamStrList.Strings[i];

      if lsParamValue = '' then
        begin
          Inc(fiParamIndex, 1);
          continue;
        end;
       if StartsStr('--', lsParamValue) then
          Delete(lsParamValue, 1, 2)
        else if StartsStr('@', lsParamValue) then
          Delete(lsParamValue, 1, 1)
        else if StartsStr('/', lsParamValue) then
          Delete(lsParamValue, 1, 1)
        else if StartsStr('-', lsParamValue) then
          Delete(lsParamValue, 1, 1);

      lCommandLineParam := tNovusCommandLine.FindCommandName(lsParamValue);

      Inc(fiParamIndex, 1);







    end;

  (*
  if StartsStr('--', Value) then
    Delete(Value, 1, 2)
  else if StartsStr('-', Value) then
    Delete(Value, 1, 1)
  else if StartsStr('/', Value) then
    Delete(Value, 1, 1)
  *)
    // FCommandList
end;

// tNovusCommandLine

class function tNovusCommandLine.RegisterCommand(const aCommandName: string;
  const aShortCommandName: string; const aHelp: String;
  aCommandLineParam: tNovusCommandLineParam): tNovusCommandLineParam;
begin

  if not Assigned(aCommandLineParam) then
    aCommandLineParam := tNovusCommandLineParam.Create;

  aCommandLineParam.CommandName := aCommandName;
  aCommandLineParam.ShortCommandName := aShortCommandName;
  aCommandLineParam.Help := aHelp;

  FCommandList.Add(aCommandLineParam);
end;



// tNovusCommandLineParam

function tNovusCommandLineParam.GetCommandName: string;
begin
  Result := fsCommandName;
end;

procedure tNovusCommandLineParam.SetCommandName(Value: string);
begin
  fsCommandName := Value;
end;

function tNovusCommandLineParam.GetShortCommandName: string;
begin
  Result := fsShortCommandName;
end;

procedure tNovusCommandLineParam.SetShortCommandName(Value: string);
begin
  fsShortCommandName := Value;
end;

function tNovusCommandLineParam.Execute: Boolean;
begin

end;

procedure tNovusCommandLineParam.SetHelp(Value: string);
begin
  fsHelp := Value;
end;

function tNovusCommandLineParam.GetHelp: string;
begin
  Result := fsHelp;
end;



// tNovusCommandlineResult

function TNovusCommandLineResult.GetErrors: Boolean;
begin

end;

procedure TNovusCommandLineResult.SetErrors(Value: Boolean);
begin

end;

end.
