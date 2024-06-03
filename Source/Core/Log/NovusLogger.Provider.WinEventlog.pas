unit NovusLogger.Provider.WinEventLog;

interface

Uses NovusLogger.Provider, NovusConsole, System.SysUtils, Winapi.Windows,
  System.Threading, System.Classes, NovusObject, NovusStringUtils,
  NovusFileUtils, System.IOUtils, NovusWinEventLog, System.Generics.Collections;

type
  TNovusLogger_Provider_WinEventLog = class(tNovusLogger_Provider)
  private
  protected
    fsAplicationName: string;
  Public
    constructor Create(aAplicationName: string); overload;
    destructor Destroy; override;

    function OpenLog: Boolean; override;
    function CloseLog: Boolean; override;

    procedure SendLogMessage(aLogMessage: String; aLogDateTime: tDateTime;
      aSeverityType: TSeverityType); override;

    procedure AddLog(aLogMessage: string; aLogDateTime: tDateTime;
      aSeverityType: TSeverityType); override;

    procedure AddLogSuccess(aLogMessage: string); override;
    procedure AddLogInformation(aLogMessage: string); override;
    procedure AddLogError(aLogMessage: string); overload; override;
    procedure AddLogError(aException: Exception); overload; override;
    procedure AddLogWarning(aLogMessage: string); override;
    procedure AddLogDebug(aLogMessage: string); override;
    procedure AddLogException(aLogMessage: string); overload; override;
    procedure AddLogSystem(aLogMessage: string); override;
  end;

implementation

Uses NovusLogger;

constructor TNovusLogger_Provider_WinEventLog.Create(aAplicationName: String);
begin
  fsAplicationName :=  aAplicationName;

  inherited Create;
end;

destructor TNovusLogger_Provider_WinEventLog.Destroy;
begin
  inherited Destroy;
end;


function TNovusLogger_Provider_WinEventLog.OpenLog: Boolean;
begin
  Result := True;
end;

function TNovusLogger_Provider_WinEventLog.CloseLog: Boolean;
begin
  Result := True;
end;

procedure TNovusLogger_Provider_WinEventLog.AddLog(aLogMessage: string;
  aLogDateTime: tDateTime; aSeverityType: TSeverityType);
begin
  (Logger as TNovusLogger).PushLogMessage(FormatLogOutput(aLogMessage,
    aLogDateTime, aSeverityType), aLogDateTime, aSeverityType, Self);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogSuccess(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stSuccess);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogInformation
  (aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stInformation);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogError(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stError);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogError(aException: Exception);
begin
  if assigned(aException) then
    AddLogError(aException.Message);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogWarning(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stWarning);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogDebug(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stDebug);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogSystem(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stSystem);
end;

procedure TNovusLogger_Provider_WinEventLog.AddLogException
  (aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stException);
end;

procedure TNovusLogger_Provider_WinEventLog.SendLogMessage(aLogMessage: String;
  aLogDateTime: tDateTime; aSeverityType: TSeverityType);
var
  loNovusWinEventLog: TNovusWinEventLog;
begin
      Try
        loNovusWinEventLog := TNovusWinEventLog.Create;

        loNovusWinEventLog.ApplicationName := fsAplicationName;

        case aSeverityType of
          stInformation:
            loNovusWinEventLog.EventType := etInformation;
          stSuccess:
            loNovusWinEventLog.EventType := etAuditSuccess;
          stNone:
            loNovusWinEventLog.EventType := etInformation;
          stWarning:
            loNovusWinEventLog.EventType := etError;
          stError:
            loNovusWinEventLog.EventType := etError;
          stCritical:
            loNovusWinEventLog.EventType := etAuditFailure;
          stException:
            loNovusWinEventLog.EventType := etAuditFailure;
          stDebug:
            loNovusWinEventLog.EventType := etInformation;
          stSystem:
            loNovusWinEventLog.EventType := etInformation;
        end;

        loNovusWinEventLog.LogEvent(aLogMessage);

      Finally
        loNovusWinEventLog.Free;
      End;
end;

end.
