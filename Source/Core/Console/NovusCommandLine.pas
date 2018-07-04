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
    procedure SetErrorMessages(Value: TStringlist);
    function GetIsCommandEmpty: Boolean;
    procedure SetIsCommandEmpty(Value: Boolean);

    procedure AddError(aErrorMessage: string);

    property IsCommandEmpty: Boolean read GetIsCommandEmpty
      write SetIsCommandEmpty;
    property Errors: Boolean read GetErrors write SetErrors;
    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;

  end;

  TNovusCommandLineResult = class(TInterfacedObject, INovusCommandLineResult)
  protected
    fbIsCommandEmpty: Boolean;
    fbErrors: Boolean;
    fErrorMessages: TStringlist;
  private
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringlist);
    function GetIsCommandEmpty: Boolean;
    procedure SetIsCommandEmpty(Value: Boolean);
  public
    constructor Create;
    destructor Destroy;

    procedure AddError(aErrorMessage: string);

    property IsCommandEmpty: Boolean read GetIsCommandEmpty
      write SetIsCommandEmpty;
    property Errors: Boolean read GetErrors write SetErrors;
    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;
  end;

  INovusCommandLineOption = interface
    ['{2195EF48-095A-4AF1-94F6-3B8B953584CF}']
    function GetOptionName: string;
    procedure SetOptionName(Value: string);
    procedure SetRequired(Value: Boolean);
    function GetRequired: Boolean;
    function GetHelp: string;
    procedure SetHelp(Value: string);
    function GetValue: string;
    procedure SetValue(Value: string);
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringlist);

    function Execute: Boolean;
    procedure AddError(aErrorMessage: string);

    property OptionName: string read GetOptionName write SetOptionName;
    property Required: Boolean read GetRequired write SetRequired;
    property Help: string read GetHelp write SetHelp;
    property Value: string read GetValue write SetValue;
    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;
  end;

  tNovusCommandLineOption = class(TInterfacedObject, INovusCommandLineOption)
  protected
    fsOptionName: string;
    fsHelp: string;
    fbRequired: Boolean;
    fsValue: string;
    fErrorMessages: TStringlist;
  private
    function GetOptionName: string;
    procedure SetOptionName(Value: string);
    procedure SetRequired(Value: Boolean);
    function GetRequired: Boolean;
    function GetHelp: string;
    procedure SetHelp(Value: string);
    function GetValue: string;
    procedure SetValue(Value: string);
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringlist);

  public
    constructor Create; virtual;
    destructor Destroy;

    function Execute: Boolean; virtual;
    procedure AddError(aErrorMessage: string);

    property OptionName: string read GetOptionName write SetOptionName;
    property Required: Boolean read GetRequired write SetRequired;
    property Help: string read GetHelp write SetHelp;
    property Value: string read GetValue write SetValue;
    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;
  end;

  INovusCommandLineCommand = interface
    ['{0CC48B45-F78D-42A5-9C49-C36C3BEE853E}']
    function GetCommandName: string;
    procedure SetCommandName(Value: string);
    function GetShortCommandName: string;
    procedure SetShortCommandName(Value: string);
    function GetHelp: string;
    procedure SetHelp(Value: string);
    procedure SetRequired(Value: Boolean);
    function GetRequired: Boolean;
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringlist);

    property CommandName: string read GetCommandName write SetCommandName;
    property ShortCommandName: string read GetShortCommandName
      write SetShortCommandName;
    property Help: string read GetHelp write SetHelp;

    property Required: Boolean read GetRequired write SetRequired;

    function IsOptionsExists: Boolean;
    procedure AddError(aErrorMessage: string);

    function Execute: Boolean;
    function Parse: Boolean;

    function RegisterOption(const aOptionName: string; const aHelp: String;
      const aRequired: Boolean; aCommandLineOption: tNovusCommandLineOption)
      : tNovusCommandLineOption;

    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;

  end;

  tNovusCommandLineCommand = class(TInterfacedObject, INovusCommandLineCommand)
  protected
    fErrorMessages: TStringlist;
    fbRequired: Boolean;
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
    procedure SetRequired(Value: Boolean);
    function GetRequired: Boolean;
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringlist);
  public
    constructor Create; virtual;
    destructor Destroy;

    property CommandName: string read GetCommandName write SetCommandName;
    property ShortCommandName: string read GetShortCommandName
      write SetShortCommandName;
    property Help: string read GetHelp write SetHelp;
    property Required: Boolean read GetRequired write SetRequired;

    function Execute: Boolean; virtual;
    function Parse: Boolean;

    function IsOptionsExists: Boolean;
    procedure AddError(aErrorMessage: string);

    function RegisterOption(const aOptionName: string; const aHelp: String;
      const aRequired: Boolean; aCommandLineOption: tNovusCommandLineOption)
      : tNovusCommandLineOption;

    property OptionList: tNovuslist read fCommandLineOptionList
      write fCommandLineOptionList;

    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;
  end;

  tNovusCommandLine = class
  protected
  class var
    FCommandList: tNovuslist;
    FParamStrList: TStringlist;
    fiParamIndex: Integer;
  private
    class function ParamStrToStringList: TStringlist;
    class function InternalParse: TNovusCommandLineResult;
    class function FindCommandName(aName: string): tNovusCommandLineCommand;
  public
    class constructor Create;
    class destructor Destroy;

    class function Parse: TNovusCommandLineResult;

    class function RegisterCommand(const aCommandName: string;
      const aShortCommandName: string; const aHelp: String;
      const aRequired: Boolean; aCommandLineCommand: tNovusCommandLineCommand)
      : tNovusCommandLineCommand;

  end;

implementation

class constructor tNovusCommandLine.Create;
begin
  FCommandList := tNovuslist.Create;
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

class function tNovusCommandLine.ParamStrToStringList: TStringlist;
Var
  i: Integer;
begin
  Result := TStringlist.Create;

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
  lCommand, lLastCommand: tNovusCommandLineCommand;
  liOptionIndex: Integer;
  liOption: tNovusCommandLineOption;
begin
  Result := TNovusCommandLineResult.Create;

  if FParamStrList.count = 0 then
  begin
    Result.IsCommandEmpty := true;

    Exit;
  end;

  fiParamIndex := 0;
  liOptionIndex := 0;
  lLastCommand := NIL;
  While (fiParamIndex <= FParamStrList.count - 1) do
  begin
    lsParamValue := Trim(FParamStrList.Strings[fiParamIndex]);

    if lsParamValue = '' then
    begin
      inc(fiParamIndex);
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

    lCommand := tNovusCommandLine.FindCommandName(lsParamValue);
    if Assigned(lCommand) then
    begin
      if (lLastCommand <> lCommand) then
      begin
        if Assigned(lLastCommand) then
        begin
          if not lLastCommand.Execute then
          begin
            Result.AddError(lLastCommand.ErrorMessages.Text);

            break;
          end;
        end;

        lLastCommand := lCommand;
        liOptionIndex := 0;
      end;

    end
    else if Assigned(lLastCommand) then
    begin
      if lLastCommand.IsOptionsExists then
      begin
        if (liOptionIndex <= lLastCommand.OptionList.count - 1) then
        begin
          liOption := tNovusCommandLineOption(lLastCommand.OptionList.items
            [liOptionIndex]);

          liOption.Value := lsParamValue;

          if not liOption.Execute then
          begin
            Result.AddError(liOption.ErrorMessages.Text);

            break;
          end;
        end
        else
        begin
          Result.AddError('Unknown command line option: ' +
            FParamStrList.Strings[fiParamIndex]);
          break;
        end;

        inc(liOptionIndex);
      end
    else
      begin
         Result.AddError('Unknown command line option: ' +
            FParamStrList.Strings[fiParamIndex]);
          break;
      end;
    end
    else
    begin
      Result.AddError('Unknown command line option : ' + FParamStrList.Strings
        [fiParamIndex]);
      break;
    end;

    inc(fiParamIndex);
  end;

  if Assigned(lLastCommand) then
  begin
     if not Result.Errors then
       begin
          if not lLastCommand.Execute then
          begin
            Result.AddError(lLastCommand.ErrorMessages.Text);
          end;
       end;
  end;

end;

// tNovusCommandLine

class function tNovusCommandLine.RegisterCommand(const aCommandName: string;
  const aShortCommandName: string; const aHelp: String;
  const aRequired: Boolean; aCommandLineCommand: tNovusCommandLineCommand)
  : tNovusCommandLineCommand;
begin

  if not Assigned(aCommandLineCommand) then
    aCommandLineCommand := tNovusCommandLineCommand.Create;

  aCommandLineCommand.CommandName := aCommandName;
  aCommandLineCommand.ShortCommandName := aShortCommandName;
  aCommandLineCommand.Help := aHelp;
  aCommandLineCommand.Required := aRequired;

  FCommandList.Add(aCommandLineCommand);
end;



// tNovusCommandLineCommand
procedure tNovusCommandLineCommand.AddError(aErrorMessage: string);
begin
  fErrorMessages.Add(aErrorMessage)
end;

function tNovusCommandLineCommand.GetErrorMessages: TStringlist;
begin
  Result := fErrorMessages;
end;

procedure tNovusCommandLineCommand.SetErrorMessages(Value: TStringlist);
begin
  fErrorMessages := Value;
end;

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
  Result := true;
end;

function tNovusCommandLineCommand.IsOptionsExists: Boolean;
begin
  Result := (fCommandLineOptionList.count <> 0);
end;

function tNovusCommandLineCommand.Parse: Boolean;
begin
  Result := Execute;
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
  fCommandLineOptionList := tNovuslist.Create;
  fErrorMessages := TStringlist.Create;
end;

destructor tNovusCommandLineCommand.Destroy;
begin
  fErrorMessages.Free;
  fCommandLineOptionList.Free;
end;

// tNovusCommandLineOption
function tNovusCommandLineCommand.RegisterOption(const aOptionName: string;
  const aHelp: string; const aRequired: Boolean;
  aCommandLineOption: tNovusCommandLineOption): tNovusCommandLineOption;
var
  fCommandLineOption: tNovusCommandLineOption;
begin

  if Assigned(aCommandLineOption) then
    fCommandLineOption := aCommandLineOption
  else
    fCommandLineOption := tNovusCommandLineOption.Create;

  fCommandLineOption := tNovusCommandLineOption.Create;
  fCommandLineOption.OptionName := aOptionName;
  fCommandLineOption.Help := aHelp;
  fCommandLineOption.Required := aRequired;

  fCommandLineOptionList.Add(fCommandLineOption);
end;

procedure tNovusCommandLineCommand.SetRequired(Value: Boolean);
begin
  fbRequired := Value;
end;

function tNovusCommandLineCommand.GetRequired: Boolean;
begin
  Result := fbRequired;
end;

// tNovusCommandlineResult
constructor TNovusCommandLineResult.Create;
begin
  fErrorMessages := TStringlist.Create;
end;

destructor TNovusCommandLineResult.Destroy;
begin
  fErrorMessages.Free;
end;

function TNovusCommandLineResult.GetIsCommandEmpty: Boolean;
begin
  Result := fbIsCommandEmpty;
end;

procedure TNovusCommandLineResult.SetIsCommandEmpty(Value: Boolean);
begin
  fbIsCommandEmpty := Value;
end;

function TNovusCommandLineResult.GetErrors: Boolean;
begin
  Result := fbErrors;
end;

procedure TNovusCommandLineResult.SetErrors(Value: Boolean);
begin
  fbErrors := Value;
end;

function TNovusCommandLineResult.GetErrorMessages: TStringlist;
begin
  Result := fErrorMessages;
end;

procedure TNovusCommandLineResult.SetErrorMessages(Value: TStringlist);
begin
  fErrorMessages := Value;
end;

procedure TNovusCommandLineResult.AddError(aErrorMessage: string);
begin
  fbErrors := true;
  fErrorMessages.Add(aErrorMessage)
end;

// tNovusCommandLineOption
constructor tNovusCommandLineOption.Create;
begin
  fErrorMessages := TStringlist.Create;
end;

destructor tNovusCommandLineOption.Destroy;
begin
  fErrorMessages.Free;
end;

function tNovusCommandLineOption.GetOptionName: string;
begin
  Result := fsOptionName;
end;

procedure tNovusCommandLineOption.SetOptionName(Value: string);
begin
  fsOptionName := Value;
end;

procedure tNovusCommandLineOption.SetRequired(Value: Boolean);
begin
  fbRequired := Value;
end;

function tNovusCommandLineOption.GetRequired: Boolean;
begin
  Result := fbRequired;
end;

function tNovusCommandLineOption.GetHelp: string;
begin
  Result := fsHelp;
end;

procedure tNovusCommandLineOption.SetHelp(Value: string);
begin
  fsHelp := Value;
end;

function tNovusCommandLineOption.Execute: Boolean;
begin
  Result := true;
end;

function tNovusCommandLineOption.GetValue: string;
begin
  Result := fsValue;
end;

procedure tNovusCommandLineOption.SetValue(Value: string);
begin
  fsValue := Value;
end;

function tNovusCommandLineOption.GetErrorMessages: TStringlist;
begin
  Result := fErrorMessages;
end;

procedure tNovusCommandLineOption.SetErrorMessages(Value: TStringlist);
begin
  fErrorMessages := Value;
end;

procedure tNovusCommandLineOption.AddError(aErrorMessage: string);
begin
  fErrorMessages.Add(aErrorMessage)
end;

end.
