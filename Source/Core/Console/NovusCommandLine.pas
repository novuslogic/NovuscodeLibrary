unit NovusCommandLine;

interface

uses
  NovusConsole, SysUtils, Classes, NovusList, System.StrUtils, vcl.dialogs;

type
   INovusCommandLineResult = interface
    ['{4E2886DD-4120-4376-B778-0D803E621850}']
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);

    property Errors: Boolean read GetErrors write SetErrors;

  end;

  TNovusCommandLineResult = class(TInterfacedObject, INovusCommandLineResult)
  protected
  private
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);
  public
    property Errors: Boolean read GetErrors write SetErrors;
  end;

  INovusCommandLineOption = interface
    ['{2195EF48-095A-4AF1-94F6-3B8B953584CF}']
    function GetOptionName: string;
    procedure SetOptionName(Value: string);

    function Execute: Boolean;

  end;

  INovusCommandLineCommand = interface
    ['{0CC48B45-F78D-42A5-9C49-C36C3BEE853E}']
    function GetCommandName: string;
    procedure SetCommandName(Value: string);
    function GetShortCommandName: string;
    procedure SetShortCommandName(Value: string);
    function GetHelp: string;
    procedure SetHelp(Value: string);

    property CommandName: string read GetCommandName write SetCommandName;
    property ShortCommandName: string read GetShortCommandName
      write SetShortCommandName;
    property Help: string read GetHelp write SetHelp;

    function Execute: Boolean;
  end;

  tNovusCommandLineCommand = class(TInterfacedObject, INovusCommandLineCommand)
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
    class function FindCommandName(aName: string): tNovusCommandLineCommand;
  public
    class constructor Create;
    class destructor Destroy;

    class function Parse: TNovusCommandLineResult;

    class function RegisterCommand(const aCommandName: string;
      const aShortCommandName: string; const aHelp: String;
      aCommandLineParam: tNovusCommandLineCommand): tNovusCommandLineCommand;



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
  : tNovusCommandLineCommand;
Var
  fCommandLineParam: tNovusCommandLineCommand;
  i: Integer;
begin
  Result := NIL;
  For i := 0 to FCommandList.count - 1 do
  begin
    fCommandLineParam := FCommandList.items[i] as tNovusCommandLineCommand;

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
  lCommandLineParam: tNovusCommandLineCommand;
begin
  Result := '';

  for I := 0 to FParamStrList.Count -1 do
    begin
      lsParamValue := Trim(FParamStrList.Strings[i]);

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

end;

// tNovusCommandLine

class function tNovusCommandLine.RegisterCommand(const aCommandName: string;
  const aShortCommandName: string; const aHelp: String;
  aCommandLineParam: tNovusCommandLineCommand): tNovusCommandLineCommand;
begin

  if not Assigned(aCommandLineParam) then
    aCommandLineParam := tNovusCommandLineCommand.Create;

  aCommandLineParam.CommandName := aCommandName;
  aCommandLineParam.ShortCommandName := aShortCommandName;
  aCommandLineParam.Help := aHelp;

  FCommandList.Add(aCommandLineParam);
end;



// tNovusCommandLineCommand

function tNovusCommandLineCommand.GetCommandName: string;
begin
  Result := fsCommandName;
end;

procedure tNovusCommandLineCommand.SetCommandName(Value: string);
begin
  fsCommandName := Value;
end;

function tNovusCommandLineCommand.GetShortCommandName: string;
begin
  Result := fsShortCommandName;
end;

procedure tNovusCommandLineCommand.SetShortCommandName(Value: string);
begin
  fsShortCommandName := Value;
end;

function tNovusCommandLineCommand.Execute: Boolean;
begin

end;

procedure tNovusCommandLineCommand.SetHelp(Value: string);
begin
  fsHelp := Value;
end;

function tNovusCommandLineCommand.GetHelp: string;
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
