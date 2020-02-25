unit NovusCommandLine;

interface

uses
  NovusConsole, SysUtils, Classes, NovusList, System.StrUtils, System.RegularExpressions,
  System.Generics.Defaults;

type
  INovusCommandLineCommand = interface;
  INovusCommandLine = interface;

  tNovusCommandOptions = record
    CommandEmptyExitCode: Integer;
    CommandLineOptionExitCode: Integer;
    CommandLineExitCode: Integer;
  end;

  INovusCommandLineResultOption = interface
    ['{6C2D2989-5016-4FE2-A7BF-85CA3F6754AA}']
    function GetOptionName: string;
    procedure SetOptionName(Value: string);
    function GetValue: string;
    procedure SetValue(Value: string);

    property OptionName: string read GetOptionName write SetOptionName;
    property Value: string read GetValue write SetValue;

  end;

   INovusCommandLineResultOptions =  interface
    ['{394CA461-271D-46A2-89DD-9736F0909417}']
    function GetItems: tNovusList;
    procedure SetItems(Value: tNovusList);

    procedure Add(aItem: INovusCommandLineResultOption);
    function FirstOption: INovusCommandLineResultOption;

    property Items: tNovuslist
      read GetItems
      write SetItems;
  end;


  INovusCommandLineResultCommand = interface
    ['{CC39CEF4-8289-4B74-9A9C-BCBCDAD0A730}']
    function GetCommandName: string;
    procedure SetCommandName(Value: string);

    function GetIsCommandOnly: boolean;
    procedure SetIsCommandOnly(value: boolean);
    function GetOptions: INovusCommandLineResultOptions;
    procedure SetOptions(Value: INovusCommandLineResultOptions);

    property CommandName: string read GetCommandName write SetCommandName;

    property IsCommandOnly: boolean read GetIsCommandOnly write SetIsCommandOnly;

    property Options: INovusCommandLineResultOptions read GetOptions write SetOptions;
  end;


  INovusCommandLineResultCommands = interface
    ['{AFE16358-BAC6-47E1-95F1-5A8C9DBAB002}']
    function GetItems: tNovusList;
    procedure SetItems(Value: tNovusList);

    procedure Add(aItem: INovusCommandLineResultCommand);

    function FirstCommand: INovusCommandLineResultCommand;
    function NextCommand: INovusCommandLineResultCommand;
    function GetIndex: Integer;
    procedure SetIndex(value: integer);

    function Exists(aCommandName: String): boolean;

    function GetCommands(aCommands: string): INovusCommandLineResultCommands;

    property Items: tNovuslist
      read GetItems
      write SetItems;

    property Index: Integer
      read GetIndex
      write SetIndex;
  end;

  INovusCommandLineResult = interface
    ['{4E2886DD-4120-4376-B778-0D803E621850}']
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);
    function GetExitCode: Integer;
    procedure SetExitCode(Value: Integer);
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringlist);
    function GetIsCommandEmpty: Boolean;
    procedure SetIsCommandEmpty(Value: Boolean);
    function GetHelp: String;
    function GetCommands: INovusCommandLineResultCommands;
    procedure SetCommands(Value: INovusCommandLineResultCommands);

    procedure AddError(aErrorMessage: string; aExitCode: Integer = 0); overload;
    procedure AddError(aCommand: INovusCommandLineCommand); overload;

    function FindFirstCommandwithOption(aCommandName: String): INovusCommandLineResultOption;
    function FindFirstCommand(aCommandName: String): INovusCommandLineResultCommand;

    property IsCommandEmpty: Boolean read GetIsCommandEmpty
      write SetIsCommandEmpty;
    property Help: String read GetHelp;

    property Errors: Boolean read GetErrors write SetErrors;
    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;

    property ExitCode: Integer read GetExitCode write SetExitCode;

    property Commands: INovusCommandLineResultCommands read GetCommands write SetCommands;
  end;

  TNovusCommandLineResultOption = class(TSingletonImplementation, INovusCommandLineResultOption)
  private
    fsOptionName: String;
    fsValue: String;
  protected
    function GetOptionName: string;
    procedure SetOptionName(Value: string);
    function GetValue: string;
    procedure SetValue(Value: string);
  public
    property OptionName: string read GetOptionName write SetOptionName;
    property Value: string read GetValue write SetValue;
  end;



  TNovusCommandLineResultCommand = class(TSingletonImplementation, INovusCommandLineResultCommand)
  protected
    fOptions: INovusCommandLineResultOptions;
    fsCommandName: string;
    fbIsCommandOnly: Boolean;
  private
    fbIsCommand: boolean;
    function GetCommandName: string;
    procedure SetCommandName(Value: string);

    function GetIsCommandOnly: boolean;
    procedure SetIsCommandOnly(value: boolean);

    function GetOptions: INovusCommandLineResultOptions;
    procedure SetOptions(Value: INovusCommandLineResultOptions);
  public
    constructor Create;
    destructor Destroy;

    property CommandName: string read GetCommandName write SetCommandName;


    property IsCommandOnly: Boolean read GetIsCommandOnly write SetIsCommandOnly;


    property Options: INovusCommandLineResultOptions read GetOptions write SetOptions;
  end;

  TNovusCommandLineResultOptions = class(TSingletonImplementation, INovusCommandLineResultOptions)
  protected
    FNovusCommandLineResultOptions: TNovusList;
  private
    function GetItems: tNovusList;
    procedure SetItems(Value: tNovusList);
  public
    constructor Create;
    destructor Destroy;

    procedure Add(aItem: INovusCommandLineResultOption);
    function FirstOption: INovusCommandLineResultOption;

    property Items: tNovuslist
      read GetItems
      write SetItems;
  end;

  TNovusCommandLineResultCommands = class(TSingletonImplementation, INovusCommandLineResultCommands)
  protected
    FNovusCommandLineResultCommands: TNovusList;
  private
    fiIndex: Integer;
    function GetItems: tNovusList;
    procedure SetItems(Value: tNovusList);
    function GetIndex: Integer;
    procedure SetIndex(value: integer);
  public
    constructor Create;
    destructor Destroy;

    function GetCommands(aCommandName: string): INovusCommandLineResultCommands;
    function FirstCommand: INovusCommandLineResultCommand;
    function NextCommand: INovusCommandLineResultCommand;

    procedure Add(aItem: INovusCommandLineResultCommand);

    function Exists(aCommandName: String): boolean;

    property Items: tNovuslist read GetItems write SetItems;

    property Index: Integer
      read GetIndex
      write SetIndex;
  end;

  TNovusCommandLineResult = class(TSingletonImplementation, INovusCommandLineResult)
  protected
    fCommands: INovusCommandLineResultCommands;
    fiExitCode: Integer;
    fbIsCommandEmpty: Boolean;
    fbErrors: Boolean;
    fsHelp: string;
    fErrorMessages: TStringlist;
  private
    function GetErrors: Boolean;
    procedure SetErrors(Value: Boolean);
    function GetHelp: String;
    function GetExitCode: Integer;
    procedure SetExitCode(Value: Integer);
    function GetErrorMessages: TStringlist;
    procedure SetErrorMessages(Value: TStringlist);
    function GetIsCommandEmpty: Boolean;
    procedure SetIsCommandEmpty(Value: Boolean);
    function GetCommands: INovusCommandLineResultCommands;
    procedure SetCommands(Value: INovusCommandLineResultCommands);
  public
    constructor Create;
    destructor Destroy;

    procedure AddError(aErrorMessage: string; aExitCode: Integer = 0); overload;
    procedure AddError(aCommand: INovusCommandLineCommand); overload;

    function FindFirstCommand(aCommandName: String): INovusCommandLineResultCommand;
    function FindFirstCommandwithOption(aCommandName: String): INovusCommandLineResultOption;

    property IsCommandEmpty: Boolean read GetIsCommandEmpty
      write SetIsCommandEmpty;

    property Help: String read GetHelp;
    property Errors: Boolean read GetErrors write SetErrors;
    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;

    property Commands: INovusCommandLineResultCommands read GetCommands write SetCommands;

    property ExitCode: Integer
       read fiExitCode
       write fiExitCode;
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

    function GetExitCode: Integer;
    procedure SetExitCode(Value: Integer);

    function Execute: Boolean;
    procedure AddError(aErrorMessage: string);

    property OptionName: string read GetOptionName write SetOptionName;
    property Required: Boolean read GetRequired write SetRequired;
    property Help: string read GetHelp write SetHelp;
    property Value: string read GetValue write SetValue;
    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;
    property ExitCode: Integer
       read GetExitCode write SetExitCode;
  end;

  tNovusCommandLineOption = class(TSingletonImplementation, INovusCommandLineOption)
  protected
    fiExitCode: Integer;
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

    function GetExitCode: Integer;
    procedure SetExitCode(Value: Integer);

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
    property ExitCode : integer
       read GetExitCode write SetExitCode;
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
    function GetOptionList: tNovuslist;
    procedure SetOptionList(Value: tNovuslist);
    function GetExitCode: Integer;
    procedure SetExitCode(Value: Integer);
    function GetCommandList: tNovuslist;
    procedure SetCommandList(Value: tNovuslist);

    function IsOptionsRequried: Boolean;

    property CommandName: string read GetCommandName write SetCommandName;
    property ShortCommandName: string read GetShortCommandName
      write SetShortCommandName;
    property Help: string read GetHelp write SetHelp;

    property Required: Boolean read GetRequired write SetRequired;

    procedure RegisterObject(aObject: TObject);
    function FindRegisterObject(aClassName: String): TObject;

    function FindCommandName(aCommandName: string): INovusCommandLineCommand;

    function FindOptionByName(aOptionName: string): INovusCommandLineOption;
    function IsOptionsExists: Boolean;
    procedure AddError(aErrorMessage: string; aExitCode: Integer = 0);

    function Execute: Boolean;
    function Parse: Boolean; overload;


    function RegisterOption(const aOptionName: string; const aHelp: String;
      const aRequired: Boolean; aCommandLineOption: tNovusCommandLineOption)
      : tNovusCommandLineOption;

    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;

    property ExitCode: Integer read GetExitCode write SetExitCode;

    property OptionList: tNovuslist read GetOptionList write SetOptionList;

    property CommandList: tNovuslist read GetCommandList write SetCommandList;

  end;

  tNovusCommandLineCommand = class(TSingletonImplementation, INovusCommandLineCommand)
  protected
    fiExitCode: Integer;
    fErrorMessages: TStringlist;
    fbRequired: Boolean;
    fsHelp: string;
    fsCommandName: String;
    fsShortCommandName: String;
    fOptionList: tNovuslist;
    fCommandList: tNovuslist;
    fObjectList: tNovuslist;
  private
    function GetOptionList: tNovuslist;
    procedure SetOptionList(Value: tNovuslist);
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
    function GetExitCode: Integer;
    procedure SetExitCode(Value: Integer);
    function GetCommandList: tNovuslist;
    procedure SetCommandList(Value: tNovuslist);
  public
    constructor Create; virtual;
    destructor Destroy;

    function FindCommandName(aCommandName: string): INovusCommandLineCommand;

    property CommandName: string read GetCommandName write SetCommandName;
    property ShortCommandName: string read GetShortCommandName
      write SetShortCommandName;
    property Help: string read GetHelp write SetHelp;
    property Required: Boolean read GetRequired write SetRequired;

    function Execute: Boolean; virtual;
    function Parse: Boolean;
  
    function FindOptionByName(aOptionName: string): INovusCommandLineOption;
    function IsOptionsExists: Boolean;

    function IsOptionsRequried: Boolean;

    procedure AddError(aErrorMessage: string; aExitCode: Integer = 0);

    procedure RegisterObject(aObject: TObject);
    function FindRegisterObject(aClassName: String): TObject;

    function RegisterOption(const aOptionName: string; const aHelp: String;
      const aRequired: Boolean; aCommandLineOption: tNovusCommandLineOption)
      : tNovusCommandLineOption;

    property OptionList: tNovuslist read GetOptionList write SetOptionList;

    property ErrorMessages: TStringlist read GetErrorMessages
      write SetErrorMessages;

    property ExitCode: Integer read GetExitCode write SetExitCode;

    property CommandList: tNovuslist read GetCommandList write SetCommandList;

  end;

  INovusCommandLine = interface
    ['{64BD4D75-DFF2-4311-A414-9F92CE774949}']

  end;

  tNovusCommandLine = class(TSingletonImplementation, INovusCommandLine)
  protected
  class var
    fCommandList: tNovuslist;
    FParamStrList: TStringlist;
    fiParamIndex: Integer;
  private
    class function ParamStrToStringList: TStringlist;
    class function InternalParse(aOptions: tNovusCommandOptions): TNovusCommandLineResult;
    class function FindCommandName(aCommandName: string)
      : INovusCommandLineCommand;
  public
    class constructor Create;
    class destructor Destroy;

    class function Execute: TNovusCommandLineResult; overload;
    class function Execute(aOptions: tNovusCommandOptions): TNovusCommandLineResult; overload;

    class function RegisterCommand(const aCommandName: string;
      const aShortCommandName: string; const aHelp: String;
      const aRequired: Boolean; aCommandLineCommand: tNovusCommandLineCommand)
      : tNovusCommandLineCommand;


  end;

implementation

class constructor tNovusCommandLine.Create;
begin
  fCommandList := tNovuslist.Create;
end;

class destructor tNovusCommandLine.Destroy;
begin
  if Assigned(FParamStrList) then
    FParamStrList.Free;

  fCommandList.Free;
end;

class function tNovusCommandLine.Execute: TNovusCommandLineResult;
Var
  FOptions: tNovusCommandOptions;
begin
  Result := NIL;

  FOptions.CommandEmptyExitCode := -1;
  FOptions.CommandLineExitCode := -2;
  FOptions.CommandLineOptionExitCode := -3;


  FParamStrList := tNovusCommandLine.ParamStrToStringList;

  Result := tNovusCommandLine.InternalParse(FOptions);
end;

class function tNovusCommandLine.Execute(aOptions: tNovusCommandOptions): TNovusCommandLineResult;
begin
  Result := NIL;

  FParamStrList := tNovusCommandLine.ParamStrToStringList;

  Result := tNovusCommandLine.InternalParse(aOptions);
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

class function tNovusCommandLine.FindCommandName(aCommandName: string)
  : INovusCommandLineCommand;
Var
  fCommand: tNovusCommandLineCommand;
  i: Integer;


  function IsAplhaNumbicString(aString: String): boolean;
  begin
    result := false;

    Result := Not System.RegularExpressions.TRegEx.IsMatch(aString, '[^A-Za-z0-9\.]+');
  end;



begin
  Result := NIL;

  if not  IsAplhaNumbicString(aCommandName) then Exit;

  For i := 0 to fCommandList.count - 1 do
  begin
    if fCommandList.items[i] Is tNovusCommandLineCommand then
      begin
        fCommand := fCommandList.items[i] as tNovusCommandLineCommand;

        if (Uppercase(fCommand.CommandName) = Uppercase(aCommandName)) or
          (Uppercase(fCommand.ShortCommandName) = Uppercase(aCommandName)) then
        begin
          Result := fCommand;

          break;
        end;
     end;

  end;
end;




class function tNovusCommandLine.InternalParse(aOptions: tNovusCommandOptions): TNovusCommandLineResult;
Var
  lsParamValue: string;
  lCommand, lLastCommand: INovusCommandLineCommand;
  liOptionIndex: Integer;
  lOption: tNovusCommandLineOption;
  fbOptionsRequried: Boolean;
  lbIsCommand: Boolean;
  lsLastParamValue: String;
  fResultCommand: INovusCommandLineResultCommand;
  fResultOption: INovusCommandLineResultOption;
  I: integer;
  fCommand: tNovusCommandLineCommand;
begin
  Result := TNovusCommandLineResult.Create;

  if FParamStrList.count = 0 then
  begin
    Result.IsCommandEmpty := true;

    Result.Errors := true;
  
    Result.ExitCode := aOptions.CommandEmptyExitCode;

    Exit;
  end;

  fiParamIndex := 0;
  liOptionIndex := 0;
  fbOptionsRequried := false;
  lLastCommand := NIL;
  While (fiParamIndex <= FParamStrList.count - 1) do
  begin
    lsParamValue := Trim(FParamStrList.Strings[fiParamIndex]);

    if lsParamValue = '' then
    begin
      inc(fiParamIndex);
      continue;
    end;

    lbIsCommand := false;
    lsLastParamValue := lsParamValue;
    if StartsStr('--', lsParamValue) then
      Delete(lsParamValue, 1, 2)
    else if StartsStr('@', lsParamValue) then
      Delete(lsParamValue, 1, 1)
    else if StartsStr('/', lsParamValue) then
      Delete(lsParamValue, 1, 1)
    else if StartsStr('-', lsParamValue) then
      Delete(lsParamValue, 1, 1);

    if Trim(lsLastParamValue) <> Trim(lsParamValue) then
      lbIsCommand := true;


    lCommand := tNovusCommandLine.FindCommandName(lsParamValue);
    if Assigned(lCommand) and (lLastCommand <> lCommand) then
    begin
      lLastCommand := lCommand;
      liOptionIndex := 0;
      fbOptionsRequried := lLastCommand.IsOptionsRequried;
    end
    else
    if Assigned(lLastCommand) then
      begin
        // fbOptionsRequried := false;
        lOption := nil;
        if lLastCommand.IsOptionsExists then
        begin
          if (liOptionIndex <= lLastCommand.OptionList.count - 1) then
          begin
            lOption := tNovusCommandLineOption
              (lLastCommand.OptionList.items[liOptionIndex]);

            lOption.Value := lsParamValue;

            if (lOption.Value = '') and (lOption.Required = true) then
            begin
              Result.AddError('Required Option [' + lOption.OptionName +
                '] was not specified');

              result.ExitCode := aOptions.CommandLineOptionExitCode;

              break;
            end
            else if not lOption.Execute then
            begin
              Result.AddError(lOption.ErrorMessages.Text);

              result.ExitCode := lOption.ExitCode;
              if result.ExitCode = 0 then
                result.ExitCode := aOptions.CommandLineOptionExitCode;

              break;
            end;
          end
          else
          begin
            Result.AddError('Unknown command line option: ' +
              FParamStrList.Strings[fiParamIndex]);

            result.ExitCode := aOptions.CommandLineOptionExitCode;

            break;
          end;

          inc(liOptionIndex);
        end
        else
        begin
          Result.AddError('Unknown command line option: ' +
            FParamStrList.Strings[fiParamIndex]);

          result.ExitCode := aOptions.CommandLineOptionExitCode;

          break;
        end;


      if not Result.Errors then
        begin
          if not lLastCommand.Execute then
          begin
            Result.AddError(lLastCommand);

            Result.ExitCode := lLastCommand.ExitCode;
            if Result.ExitCode = 0 then
               Result.ExitCode := aOptions.CommandLineExitCode;

            break;
          end
        else
          begin
            fResultCommand := TNovusCommandLineResultCommand.Create;

            fResultCommand.CommandName := lLastCommand.CommandName;

            if Assigned(lOption) then
              begin
                fResultOption:= TNovusCommandLineResultOption.Create;

                fResultOption.OptionName := lOption.OptionName;
                fResultOption.Value := lOption.Value;

                fResultCommand.Options.Add(fResultOption);
              end
            else fResultCommand.IsCommandOnly := true;

            Result.Commands.Add(fResultCommand);

            lLastCommand := NIL;
          end;

      end
      else
      begin
        Result.AddError('Unknown command line option: ' + FParamStrList.Strings
          [fiParamIndex]);

       Result.ExitCode := aOPtions.CommandLineOptionExitCode;

        break;
      end;
    end
    else
      if lbIsCommand then
        begin
          Result.AddError('Unknown command line: ' + lsLastParamValue);

          Result.ExitCode := aOPtions.CommandLineExitCode;

          break;
        end;

    if not fbOptionsRequried and lbIsCommand then
      begin
        fResultCommand := TNovusCommandLineResultCommand.Create;

        fResultCommand.CommandName := lLastCommand.CommandName;

        fResultCommand.IsCommandOnly := true;

        Result.Commands.Add(fResultCommand);

        lLastCommand := NIL;
      end;


    inc(fiParamIndex);
  end;


  // Check Required Command
  For i := 0 to fCommandList.count - 1 do
  begin
    fCommand := fCommandList.items[i] as tNovusCommandLineCommand;

    if fCommand.Required then
      begin
        if not Result.Commands.Exists(fCommand.CommandName) then
          begin
            Result.AddError('Required Command [' + fCommand.CommandName +'] was not specified');
            Result.ExitCode := aOPtions.CommandLineExitCode;
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

  aCommandLineCommand.CommandList := fCommandList;

  fCommandList.Add(aCommandLineCommand);
end;

// tNovusCommandLineCommand
procedure tNovusCommandLineCommand.AddError(aErrorMessage: string;
  aExitCode: Integer);
begin
  fiExitCode := aExitCode;

  fErrorMessages.Add(Trim(aErrorMessage));
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

function tNovusCommandLineCommand.GetOptionList: tNovuslist;
begin
  Result := fOptionList;
end;

procedure tNovusCommandLineCommand.SetOptionList(Value: tNovuslist);
begin
  fOptionList := Value;
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
  Result := (fOptionList.count <> 0);
end;




function tNovusCommandLineCommand.IsOptionsRequried: Boolean;
var
  lOption: tNovusCommandLineOption;
  i: Integer;
begin
  Result := false;

  for i := 0 to OptionList.count - 1 do
  begin
    lOption := tNovusCommandLineOption(OptionList.items[i]);
    if lOption.Required = true then
    begin
      Result := true;
      break;
    end;

  end;
end;

function tNovusCommandLineCommand.FindOptionByName(aOptionName: string)
  : INovusCommandLineOption;
Var
  fNovusCommandLineOption: TObject;
begin
  Result := NIL;

  fNovusCommandLineOption := fOptionList.FindItem(aOptionName);

  if Assigned(fNovusCommandLineOption) then
    Result := tNovusCommandLineOption(fNovusCommandLineOption);
end;

function tNovusCommandLineCommand.FindCommandName(aCommandName: string)
  : INovusCommandLineCommand;
Var
  fCommand: INovusCommandLineCommand;
  i: Integer;
begin
  Result := NIL;

  For i := 0 to fCommandList.count - 1 do
  begin
    fCommand := fCommandList.items[i] as tNovusCommandLineCommand;

    if (Uppercase(fCommand.CommandName) = Uppercase(aCommandName)) or
      (Uppercase(fCommand.ShortCommandName) = Uppercase(aCommandName)) then
    begin
      Result := fCommand;

      break;
    end;
  end;
end;

procedure tNovusCommandLineCommand.RegisterObject(aObject: TObject);
begin
  fObjectList.Add(aObject.ClassName, aObject);
end;

function tNovusCommandLineCommand.FindRegisterObject
  (aClassName: String): TObject;
begin
  Result := fObjectList.FindItem(aClassName);
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
  fOptionList := tNovuslist.Create;
  fErrorMessages := TStringlist.Create;
  fObjectList := tNovuslist.Create;
end;

destructor tNovusCommandLineCommand.Destroy;
begin
  fObjectList.Free;
  fErrorMessages.Free;
  fOptionList.Free;
  fCommandList.Free;
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

  fOptionList.Add(aOptionName, fCommandLineOption);
end;

procedure tNovusCommandLineCommand.SetRequired(Value: Boolean);
begin
  fbRequired := Value;
end;

function tNovusCommandLineCommand.GetRequired: Boolean;
begin
  Result := fbRequired;
end;

procedure tNovusCommandLineCommand.SetExitCode(Value: Integer);
begin
  fiExitCode := Value;
end;

function tNovusCommandLineCommand.GetExitCode: Integer;
begin
  Result := fiExitCode;
end;

function tNovusCommandLineCommand.GetCommandList: tNovuslist;
begin
  Result := fCommandList;
end;

procedure tNovusCommandLineCommand.SetCommandList(Value: tNovuslist);
begin
  fCommandList := Value;
end;

// tNovusCommandlineResult
constructor TNovusCommandLineResult.Create;
begin
  inherited;

  fiExitCode := 0;

  fCommands := TNovusCommandLineResultCommands.Create;

  fErrorMessages := TStringlist.Create;
  fErrorMessages.TrailingLineBreak := false;
end;

destructor TNovusCommandLineResult.Destroy;
begin
  inherited;

  (fCommands as TNovusCommandLineResultCommands).Free;
  fErrorMessages.Free;
end;

function TNovusCommandLineResult.GetCommands: INovusCommandLineResultCommands;
begin
  Result := fCommands;
end;

procedure TNovusCommandLineResult.SetCommands(Value: INovusCommandLineResultCommands);
begin
  fCommands := Value;
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

function TNovusCommandLineResult.GetExitCode: Integer;
begin
  Result := fiExitCode;
end;

procedure TNovusCommandLineResult.SetExitCode(Value: Integer);
begin
  fiExitCode := Value;
end;

function TNovusCommandLineResult.GetHelp: string;
begin
  Result := fsHelp;
end;

procedure TNovusCommandLineResult.AddError(aErrorMessage: string;
  aExitCode: Integer);
begin
  fbErrors := true;
  fiExitCode := aExitCode;

  fErrorMessages.Add(Trim(aErrorMessage));
end;

procedure TNovusCommandLineResult.AddError(aCommand: INovusCommandLineCommand);
begin
  if Not Assigned(aCommand) then
    Exit;
  fbErrors := true;
  fsHelp := aCommand.Help;
  if Trim(aCommand.ErrorMessages.Text) <> '' then
    fErrorMessages.Add(Trim(aCommand.ErrorMessages.Text));

  fiExitCode := aCommand.ExitCode;
end;


function TNovusCommandLineResult.FindFirstCommand(aCommandName: String): INovusCommandLineResultCommand;
Var
  fNovusCommandLineResultCommands: INovusCommandLineResultCommands;
  fNovusCommandLineResultCommand: INovusCommandLineResultCommand;
begin
  Result := NIL;

  fNovusCommandLineResultCommands := Commands.GetCommands
    ( aCommandName);
  if Assigned(fNovusCommandLineResultCommands) then
      Result := fNovusCommandLineResultCommands.FirstCommand;
end;

function TNovusCommandLineResult.FindFirstCommandwithOption(aCommandName: String): INovusCommandLineResultOption;
Var
  fNovusCommandLineResultCommands: INovusCommandLineResultCommands;
  fNovusCommandLineResultCommand: INovusCommandLineResultCommand;
begin
  Result := NIL;

  fNovusCommandLineResultCommand := FindFirstCommand(aCommandName);
  if Assigned(fNovusCommandLineResultCommand) then
    begin
      if not fNovusCommandLineResultCommand.IsCommandOnly then
         Result := fNovusCommandLineResultCommand.Options.FirstOption;
    end;
end;

// tNovusCommandLineOption
constructor tNovusCommandLineOption.Create;
begin
  fErrorMessages := TStringlist.Create;
  fiExitCode := 0;
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

function tNovusCommandLineOption.GetExitCode: Integer;
begin
  Result := fiExitCode;
end;

procedure tNovusCommandLineOption.SetExitCode(Value: Integer);
begin
  fiExitCode := Value;
end;

// TNovusCommandLineResultCommand
constructor TNovusCommandLineResultCommand.Create;
begin
  fsCommandName := '';

  fbIsCommand := False;

  fOptions:= TNovusCommandLineResultOptions.Create;
end;

destructor TNovusCommandLineResultCommand.Destroy;
begin
  (fOptions as TNovusCommandLineResultOptions).Free;
end;

function TNovusCommandLineResultCommand.GetCommandName: string;
begin
  Result := fsCommandName;
end;


function TNovusCommandLineResultCommand.GetOptions: INovusCommandLineResultOptions;
begin
  Result := fOptions;
end;

procedure TNovusCommandLineResultCommand.SetOptions(Value: INovusCommandLineResultOptions);
begin
  fOptions := Value;
end;

procedure TNovusCommandLineResultCommand.SetCommandName(Value: string);
begin
  fsCommandName := Value;
end;

function TNovusCommandLineResultCommand.GetIsCommandOnly: Boolean;
begin
  Result := fbIsCommandOnly;
end;

procedure TNovusCommandLineResultCommand.SetIsCommandOnly(Value: Boolean);
begin
  fbIsCommandOnly := Value;
end;

// TNovusCommandLineResultCommands
constructor TNovusCommandLineResultCommands.Create;
begin
  fiIndex := 0;

  FNovusCommandLineResultCommands:= TNovusList.Create(TNovusCommandLineResultCommand);
end;

destructor TNovusCommandLineResultCommands.Destroy;
begin
  FNovusCommandLineResultCommands.Free;
end;

function TNovusCommandLineResultCommands.GetItems: tNovusList;
begin
  Result := FNovusCommandLineResultCommands;
end;

procedure TNovusCommandLineResultCommands.SetItems(Value: tNovusList);
 begin
   FNovusCommandLineResultCommands := Value;
 end;

function TNovusCommandLineResultCommands.GetIndex: Integer;
begin
  Result := fiIndex;
end;

procedure TNovusCommandLineResultCommands.SetIndex(value: integer);
begin
  fiIndex := value;
end;

procedure TNovusCommandLineResultCommands.Add(aItem: INovusCommandLineResultCommand);
begin
  FNovusCommandLineResultCommands.Add(aItem as TNovusCommandLineResultCommand);
end;

function TNovusCommandLineResultCommands.GetCommands(aCommandName: string): INovusCommandLineResultCommands;
Var
  fNovusCommandLineResultCommand: TNovusCommandLineResultCommand;
  I: Integer;
begin
  Result := NIL;

  if Not Exists(aCommandName) then Exit;

  Result := tNovusCommandLineResultCommands.Create;

  for I := 0 to Items.Count -1 do
    begin
      fNovusCommandLineResultCommand := (Items[i] as TNovusCommandLineResultCommand);

      if Uppercase( Trim(fNovusCommandLineResultCommand.CommandName)) = Uppercase(Trim(aCommandName)) then
          Result.Add(fNovusCommandLineResultCommand);

    end;

end;


function TNovusCommandLineResultCommands.Exists(aCommandName: String): boolean;
Var
  I: integer;
  LItem: TNovusCommandLineResultCommand;
begin
  Result := False;

  for I := 0 to FNovusCommandLineResultCommands.Count -1 do
    begin
      LItem  := TNovusCommandLineResultCommand(Self.Items[i]);

      if Uppercase(Trim(LItem.CommandName)) = Uppercase(Trim(aCommandName)) then
        begin
          Result := True;

          Break;
        end;
    end;
end;

function TNovusCommandLineResultCommands.FirstCommand: INovusCommandLineResultCommand;
begin
  Result := NIL;

  fiIndex := 0;

  if Items.Count > 0 then
    Result := TNovusCommandLineResultCommand(Items[fiIndex ]);
end;

function TNovusCommandLineResultCommands.NextCommand: INovusCommandLineResultCommand;
begin
  Result := NIL;

  Inc(fiIndex);
  if fiIndex > (Items.Count -1) then
    Exit;

  Result := TNovusCommandLineResultCommand(Items[fiIndex]);
end;

// TNovusCommandLineResultOption

function TNovusCommandLineResultOption.GetOptionName: string;
begin
  Result := fsOptionName;
end;

procedure TNovusCommandLineResultOption.SetOptionName(Value: string);
begin
  fsOptionName := Value;
end;

function TNovusCommandLineResultOption.GetValue: string;
begin
  Result := fsValue;
end;

procedure TNovusCommandLineResultOption.SetValue(Value: string);
begin
  fsValue := Value;
end;


// TNovusCommandLineResultOptions
constructor TNovusCommandLineResultOptions.Create;
begin
  FNovusCommandLineResultOptions := tNovusList.Create(tNovusCommandLineResultOption);
end;

destructor TNovusCommandLineResultOptions.Destroy;
begin
  FNovusCommandLineResultOptions.Free;
end;

function TNovusCommandLineResultOptions.GetItems: tNovusList;
begin
  Result := FNovusCommandLineResultOptions;
end;

procedure TNovusCommandLineResultOptions.SetItems(Value: tNovusList);
begin
  FNovusCommandLineResultOptions := Value;
end;

procedure TNovusCommandLineResultOptions.Add(aItem: INovusCommandLineResultOption);
begin
  FNovusCommandLineResultOptions.Add(aItem as tNovusCommandLineResultOption);
end;

function TNovusCommandLineResultOptions.FirstOption: INovusCommandLineResultOption;
begin
   Result := NIL;

  if Items.Count > 0 then
    Result := TNovusCommandLineResultOption(Items[0]);
end;

end.
