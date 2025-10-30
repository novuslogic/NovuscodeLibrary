unit NovusLogger.Provider;

interface

Uses NovusObject, System.SysUtils, NovusUtilities, System.Threading,
     System.Generics.Collections;

type
  TSeverityType = (stNone, stInformation, stSuccess, stWarning, stError, stCritical, stException, stDebug, stSystem);

  tNovusLogger_Provider = class(TObject)
  protected
  private
    fbActive: Boolean;
    fiRetryCount: Integer;
    fLogger: TNovusObject;
    fsDateTimeMask: String;
    function FormatDateTimeMask(aDateTime: tDateTime): String; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function OpenLog: Boolean; virtual;
    function CloseLog: Boolean; virtual;

    procedure FlushLog; virtual;

    procedure SendLogMessage(aLogMessage: String;aLogDateTime: tDateTime; aSeverityType: TSeverityType); virtual;

    function SeverityTypeToString(aSeverityType: tSeverityType): String;
    function FormatLogOutput(aLogMessage: string; aDateTime: tDateTime; aSeverityType: TSeverityType): string; virtual;

    function AddLog(aLogMessage: string; aLogDateTime: tDateTime;  aSeverityType: TSeverityType = stNone): string; virtual;
    procedure AddLogSuccess(aLogMessage: string); virtual;
    procedure AddLogInformation(aLogMessage : string); virtual;
    procedure AddLogError(aLogMessage: string); overload; virtual;
    procedure AddLogError(aException: Exception); overload; virtual;
    procedure AddLogWarning(aLogMessage: string); virtual;
    procedure AddLogDebug(aLogMessage: string); virtual;
    function AddLogException: String; overload; virtual;
    function AddLogException(aLogMessage: String): string; overload; virtual;
    procedure AddLogException(aException: Exception); overload; virtual;
    procedure AddLogSystem(aLogMessage: String); overload; virtual;

    property DateTimeMask: String read fsDateTimeMask write fsDateTimeMask;

    property Logger: tNovusObject read fLogger write fLogger;

    property RetryCount: Integer read fiRetryCount write fiRetryCount;

    property Active: Boolean read fbActive write fbActive;

 end;

implementation

Uses NovusLogger;

constructor tNovusLogger_Provider.Create;
begin
  inherited Create;

  fbActive := True;

  RetryCount := 5;

  DateTimeMask := FormatSettings.LongDateFormat + ' hh:mm:ss';
end;

destructor tNovusLogger_Provider.Destroy;
begin
  FLogger := Nil;

  inherited Destroy;
end;

procedure tNovusLogger_Provider.AddLogSuccess(aLogMessage: string);
begin
end;

procedure tNovusLogger_Provider.AddLogInformation(aLogMessage : string);
begin
end;

procedure tNovusLogger_Provider.AddLogError(aLogMessage: string);
begin
end;

procedure tNovusLogger_Provider.AddLogError(aException: Exception);
begin
end;

procedure tNovusLogger_Provider.AddLogWarning(aLogMessage: string);
begin
end;

procedure tNovusLogger_Provider.AddLogDebug(aLogMessage: string);
begin
end;


function tNovusLogger_Provider.FormatLogOutput(aLogMessage: string; aDateTime: tDateTime; aSeverityType: TSeverityType): string;
begin
  // Example - Feb 3 12:34:56 myserver myprogram: [error] An error occurred: file not found

  Result := Format('%s : [%s] %s', [FormatDateTime(DateTimeMask, aDateTime),
     SeverityTypeToString(aSeverityType), aLogMessage]);
end;


function tNovusLogger_Provider.FormatDateTimeMask(aDateTime: tDateTime): String;
begin
  result := Format(DateTimeMask, [aDateTime]);
end;

function tNovusLogger_Provider.AddLogException: String;
begin
  Result := TNovusUtilities.GetExceptMess;
  AddLogException(Result);
end;


function tNovusLogger_Provider.AddLogException(aLogMessage: String): string;
begin
  Result := '';
end;

procedure tNovusLogger_Provider.AddLogSystem(aLogMessage: String);
begin
end;

procedure tNovusLogger_Provider.AddLogException(aException: Exception);
begin
  AddLogException(aException.Message);
end;

function tNovusLogger_Provider.AddLog(aLogMessage: string; aLogDateTime: tDateTime; aSeverityType: TSeverityType): string;
begin
  result := '';
end;

function tNovusLogger_Provider.OpenLog: Boolean;
begin
  Result := true;
end;

function tNovusLogger_Provider.CloseLog: Boolean;
begin
  result := true;
end;

procedure tNovusLogger_Provider.FlushLog;
begin
end;

function tNovusLogger_Provider.SeverityTypeToString(aSeverityType: tSeverityType): String;
begin
  Result := '';

  case aSeverityType of
      stInformation: result := 'Information';
      stSuccess: result := 'Success';
      stWarning: result := 'Warning';
      stError: result := 'Error';
      stCritical: result := 'Critical';
      stException: result := 'Exception';
      stDebug: result := 'Debug';
      stSystem: result := 'System';
  end;
end;

procedure tNovusLogger_Provider.SendLogMessage(aLogMessage: String; aLogDateTime: tDateTime; aSeverityType: TSeverityType);
begin
end;

end.
