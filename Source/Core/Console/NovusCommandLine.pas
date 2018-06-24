unit NovusCommandLine;

interface

uses
  NovusConsole, SysUtils, Classes, NovusList, System.StrUtils, vcl.dialogs;

type
   INovusCommandLineResult = interface
    ['{4E2886DD-4120-4376-B778-0D803E621850}']
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringList);
    function GetIsCommandEmpty: boolean;
    procedure SetIsCommandEmpty(Value: Boolean);

    procedure AddError(aErrorMessage: string);

    property IsCommandEmpty: boolean read GetIsCommandEmpty write SetIsCommandEmpty;
    property Errors: Boolean read GetErrors write SetErrors;
    property ErrorMessages: TStringList read GetErrorMessages write SetErrorMessages;

  end;

  TNovusCommandLineResult = class(TInterfacedObject, INovusCommandLineResult)
  protected
    fbIsCommandEmpty: boolean;
    fbErrors: boolean;
    fErrorMessages: TStringlist;
  private
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringList);
    function GetIsCommandEmpty: boolean;
    procedure SetIsCommandEmpty(Value: Boolean);
  public
    constructor Create;
    destructor Destroy;

    procedure AddError(aErrorMessage: string);

    property IsCommandEmpty: boolean read GetIsCommandEmpty write SetIsCommandEmpty;
    property Errors: Boolean read GetErrors write SetErrors;
    property ErrorMessages: TStringList read GetErrorMessages write SetErrorMessages;
  end;

  INovusCommandLineOption = interface
    ['{2195EF48-095A-4AF1-94F6-3B8B953584CF}']
    function GetOptionName: string;
    procedure SetOptionName(Value: string);

    function Execute: Boolean;

    property OptionName: string  read GetOptionName write SetOptionName;
  end;

  tNovusCommandLineOption = class(TInterfacedObject, INovusCommandLineOption)
  protected
    fsOptionName: string;
  private
    function GetOptionName: string;
    procedure SetOptionName(Value: string);
  public
    function Execute: Boolean; virtual;

    property OptionName: string  read GetOptionName write SetOptionName;
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
    function Parse(var aParamIndex: integer): Boolean;

    function RegisterOption(const aOptionName: string;
      aCommandLineOption: tNovusCommandLineOption): tNovusCommandLineOption;
  end;

  tNovusCommandLineCommand = class(TInterfacedObject, INovusCommandLineCommand)
  protected
    fsHelp: string;
    fsCommandName: String;
    fsShortCommandName: String;
    fCommandLineOptionList: tNovuslist;
  private
    function GetCommandName: string;
    procedure SetCommandName(Value: string);
    function GetShortCommandName: string;
    procedure SetShortCommandName(Value: string);
    function GetHelp: string;
    procedure SetHelp(Value: string);
  public
    constructor Create; virtual;
    destructor Destroy;

    property CommandName: string read GetCommandName write SetCommandName;
    property ShortCommandName: string read GetShortCommandName
      write SetShortCommandName;
    property Help: string read GetHelp write SetHelp;
    function Execute: Boolean; virtual;
    function Parse(var aParamIndex: integer): Boolean;

    function RegisterOption(const aOptionName: string;
      aCommandLineOption: tNovusCommandLineOption): tNovusCommandLineOption;

  end;


  tNovusCommandLine = class
  protected
  class var
    FCommandList: TNovusList;
    FParamStrList: tStringlist;
    fiParamIndex: Integer;
  private
    class function ParamStrToStringList: tStringlist;
    class function InternalParse: TNovusCommandLineResult;
    class function FindCommandName(aName: string): tNovusCommandLineCommand;
  public
    class constructor Create;
    class destructor Destroy;

    class function Parse: TNovusCommandLineResult;

    class function RegisterCommand(const aCommandName: string;
      const aShortCommandName: string; const aHelp: String;
      aCommandLineCommand: tNovusCommandLineCommand): tNovusCommandLineCommand;



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

class function tNovusCommandLine.Parse;
Var
  lsCommandName: string;
begin
  Result := NIL;

  FParamStrList := tNovusCommandLine.ParamStrToStringList;

  Result := tNovusCommandLine.InternalParse;
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

class function tNovusCommandLine.InternalParse: TNovusCommandLineResult;
Var
  lsParamValue: string;
  lNovusCommandLineCommand: tNovusCommandLineCommand;
begin
  Result := TNovusCommandLineResult.Create;

  if FParamStrList.Count = 0 then
    begin
      Result.IsCommandEmpty := true;

      Exit;
    end;

  fiParamIndex := 0;
  While(fiParamIndex < FParamStrList.Count -1)  do
    begin
      lsParamValue := Trim(FParamStrList.Strings[fiParamIndex]);

      if lsParamValue = '' then
        begin
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

      lNovusCommandLineCommand := tNovusCommandLine.FindCommandName(lsParamValue);
      if Assigned(lNovusCommandLineCommand) then
        begin
          if not lNovusCommandLineCommand.Parse(fiParamIndex) then ;
        end
      else inc(fiParamIndex);

      Result.AddError('Unknown command line option: ' + FParamStrList.Strings[fiParamIndex]);

    end;

end;

// tNovusCommandLine

class function tNovusCommandLine.RegisterCommand(const aCommandName: string;
  const aShortCommandName: string; const aHelp: String;
  aCommandLineCommand: tNovusCommandLineCommand): tNovusCommandLineCommand;
begin

  if not Assigned(aCommandLineCommand) then
    aCommandLineCommand := tNovusCommandLineCommand.Create;

  aCommandLineCommand.CommandName := aCommandName;
  aCommandLineCommand.ShortCommandName := aShortCommandName;
  aCommandLineCommand.Help := aHelp;

  FCommandList.Add(aCommandLineCommand);
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
  result := false;
end;


function tNovusCommandLineCommand.Parse(var aParamIndex: Integer): Boolean;
begin
  result := false;
end;

procedure tNovusCommandLineCommand.SetHelp(Value: string);
begin
  fsHelp := Value;
end;

function tNovusCommandLineCommand.GetHelp: string;
begin
  Result := fsHelp;
end;


constructor tNovusCommandLineCommand.Create;
begin
  fCommandLineOptionList:= tNovuslist.Create;
end;

destructor tNovusCommandLineCommand.Destroy;
begin
  fCommandLineOptionList.Free;
end;

function tNovusCommandLineCommand.RegisterOption(const aOptionName: string;
      aCommandLineOption: tNovusCommandLineOption): tNovusCommandLineOption;
var
  fCommandLineOption: tNovusCommandLineOption;
begin

  if Assigned(aCommandLineOption) then
    fCommandLineOption := aCommandLineOption
  else
    fCommandLineOption := tNovusCommandLineOption.Create;


  fCommandLineOption:= tNovusCommandLineOption.Create;
  fCommandLineOption.OptionName:= aOptionName;
  fCommandLineOptionList.Add(fCommandLineOption);
end;


// tNovusCommandlineResult
constructor TNovusCommandLineResult.Create;
begin
  fErrorMessages:= TStringlist.Create;
end;

destructor TNovusCommandLineResult.Destroy;
begin
  fErrorMessages.Free;
end;

function TNovusCommandLineResult.GetIsCommandEmpty: boolean;
begin
  result := fbIsCommandEmpty;
end;

procedure TNovusCommandLineResult.SetIsCommandEmpty(Value: Boolean);
begin
  fbIsCommandEmpty := Value;
end;

function TNovusCommandLineResult.GetErrors: Boolean;
begin
  result := fbErrors;
end;

procedure TNovusCommandLineResult.SetErrors(Value: Boolean);
begin
  fbErrors := Value;
end;

function TNovusCommandLineResult.GetErrorMessages: TStringlist;
begin
  result := fErrorMessages;
end;

procedure TNovusCommandLineResult.SetErrorMessages(Value: TStringList);
begin
  fErrorMessages := Value;
end;


procedure TNovusCommandLineResult.AddError(aErrorMessage: string);
begin
  fbErrors := true;
  fErrorMessages.Add(aErrorMessage)
end;

//tNovusCommandLineOption
function tNovusCommandLineOption.GetOptionName: string;
begin
  result := fsOptionName;
end;

procedure tNovusCommandLineOption.SetOptionName(Value: string);
begin
  fsOptionName := Value;
end;

function tNovusCommandLineOption.Execute: Boolean;
begin
  result := false;
end;

end.
