unit NovusLogger.Provider.Syslog;

interface

Uses NovusLogger.Provider, NovusConsole, System.SysUtils, Winapi.Windows,
  System.Threading, System.Classes, NovusObject, NovusStringUtils,
  NovusFileUtils, System.IOUtils, IdBaseComponent, IdComponent, IdSysLog,
  IdSysLogMessage;

type
  TNovusLogger_Provider_SysLog = class(tNovusLogger_Provider)
  private
  protected
    fsHost: string;
    fiPort: Integer;
    fSysLog: TIdSysLog;
    fsHostname: string;
    fsApplicationName: string;

    procedure WriteSyslog(aLogMessage: String; aLogDateTime: tDateTime;
      aSeverityType: TSeverityType);
  Public
    constructor Create(aHost: string; aPort: Integer; aHostname: string = '';
      aApplicationName: String = ''); overload;
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

constructor TNovusLogger_Provider_SysLog.Create(aHost: string; aPort: Integer;
  aHostname: string = ''; aApplicationName: String = '');
begin
  fsHost := aHost;
  fiPort := aPort;

  fSysLog := TIdSysLog.Create(nil);
  fSysLog.Host := fsHost;
  fSysLog.Port := aPort;

  fsHostname := aHostname;
  fsApplicationName := aApplicationName;

  inherited Create;
end;

destructor TNovusLogger_Provider_SysLog.Destroy;
begin
  fSysLog.Free;

  inherited Destroy;
end;

function TNovusLogger_Provider_SysLog.OpenLog: Boolean;
begin
  Result := False;

  if Assigned(fSysLog) then
  begin
    Try
      fSysLog.Connect;

      Result := fSysLog.Connected;
    except
      on E: Exception do
        raise Exception.CreateFmt('TNovusLogger_Provider_SysLog: %s',
          [E.message]);
    end;
  end;
end;

function TNovusLogger_Provider_SysLog.CloseLog: Boolean;
begin
  Result := False;

  if Assigned(fSysLog) then
  begin
    if fSysLog.Connected then
      fSysLog.Disconnect;
  end;
end;

procedure TNovusLogger_Provider_SysLog.AddLog(aLogMessage: string;
  aLogDateTime: tDateTime; aSeverityType: TSeverityType);
begin
  (Logger as TNovusLogger).PushLogMessage(aLogMessage, aLogDateTime,
    aSeverityType, Self);
end;

procedure TNovusLogger_Provider_SysLog.AddLogSuccess(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stSuccess);
end;

procedure TNovusLogger_Provider_SysLog.AddLogInformation(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stInformation);
end;

procedure TNovusLogger_Provider_SysLog.AddLogError(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stError);
end;

procedure TNovusLogger_Provider_SysLog.AddLogError(aException: Exception);
begin
  if Assigned(aException) then
    AddLogError(aException.message);
end;

procedure TNovusLogger_Provider_SysLog.AddLogWarning(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stWarning);
end;

procedure TNovusLogger_Provider_SysLog.AddLogDebug(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stDebug);
end;

procedure TNovusLogger_Provider_SysLog.AddLogSystem(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stSystem);
end;

procedure TNovusLogger_Provider_SysLog.AddLogException(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stException);
end;

procedure TNovusLogger_Provider_SysLog.SendLogMessage(aLogMessage: String;
  aLogDateTime: tDateTime; aSeverityType: TSeverityType);
begin
  WriteSyslog(aLogMessage, aLogDateTime, aSeverityType);
end;

procedure TNovusLogger_Provider_SysLog.WriteSyslog(aLogMessage: String;
  aLogDateTime: tDateTime; aSeverityType: TSeverityType);
Var
  fSysLogMessage: TIdSysLogMessage;
begin
  if Assigned(fSysLog) then
  begin

    if not fSysLog.Connected then
      Exit;

    Try
      Try
        fSysLogMessage := TIdSysLogMessage.Create(nil);

        fSysLogMessage.TimeStamp := aLogDateTime;
        fSysLogMessage.Hostname := fsHostname;
        fSysLogMessage.Facility := TIdSyslogFacility.sfUserLevel;

        fSysLogMessage.Msg.Content := aLogMessage;

        case aSeverityType of
          stNone:
            ;
          stInformation:
            fSysLogMessage.Severity := TIdSyslogSeverity.slInformational;
          stSuccess:
            fSysLogMessage.Severity := TIdSyslogSeverity.slNotice;
          stWarning:
            fSysLogMessage.Severity := TIdSyslogSeverity.slWarning;
          stError:
            fSysLogMessage.Severity := TIdSyslogSeverity.slError;
          stCritical:
            fSysLogMessage.Severity := TIdSyslogSeverity.slCritical;
          stException:
            fSysLogMessage.Severity := TIdSyslogSeverity.slAlert;
          stDebug:
            fSysLogMessage.Severity := TIdSyslogSeverity.slDebug;
          stSystem:
            fSysLogMessage.Severity := TIdSyslogSeverity.slNotice;
        end;

        fSysLogMessage.Msg.Process := fsApplicationName;

        fSysLog.SendLogMessage(fSysLogMessage, False);
      Except
        on E: Exception do
          raise Exception.CreateFmt('TNovusLogger_Provider_SysLog: %s',
            [E.message]);
      End;
    Finally
      fSysLogMessage.Free;
    End;
  end;
end;

end.
