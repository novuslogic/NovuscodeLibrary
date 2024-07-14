{$I ..\..\core\NovusCodeLibrary.inc}

Unit NovusSvcMgr;


interface

uses
  NovusUtilities,
  Windows,
  NovusLogger,
  WinSvc;

type
  TNovusSvcMgr = class(TNovusUtilities)
  protected
    procedure ServiceMain; virtual;
  private
    FLog: tNovusLogger;
    FServiceName: PChar;
    FDisplayName: PChar;
    FStatus: TServiceStatus;
    FStatusHandle: SERVICE_STATUS_HANDLE;
    FStopped: Boolean;
    FPaused: Boolean;

    procedure ServiceCtrlHandler(Control: DWORD); stdcall;
    procedure ServiceCtrlDispatcher(dwArgc: DWORD; var lpszArgv: PChar); stdcall;
  public
    constructor Create(AServiceName, ADisplayName: PChar; aLog: tNovusLogger = nil); virtual;

    destructor Destroy; override;

    procedure InstallService(FileName: string);
    procedure UninstallService;
    function IsServiceInstalled: Boolean;
    procedure Run; virtual;

    property Status: TServiceStatus
      read fStatus;

    property Stopped: Boolean
      read fStopped;

    property Paused: Boolean
      read FPaused;
  end;

implementation


constructor TNovusSvcMgr.Create(aServiceName, aDisplayName: PChar; aLog: tNovusLogger);
begin
  FServiceName := AServiceName;
  FDisplayName := ADisplayName;
  FLog := aLog
end;




destructor TNovusSvcMgr.Destroy;
begin
  inherited;
end;

procedure TNovusSvcMgr.ServiceMain;
begin
end;

procedure TNovusSvcMgr.ServiceCtrlHandler(Control: DWORD); stdcall;
begin
  if Assigned(FLog) then FLog.AddLogInformation('ServiceCtrlHandler');


  case Control of
    SERVICE_CONTROL_STOP:
      begin
        FStopped := True;
        FStatus.dwCurrentState := SERVICE_STOP_PENDING;
        SetServiceStatus(FStatusHandle, FStatus);
      end;
    SERVICE_CONTROL_PAUSE:
      begin
        FPaused := True;
        FStatus.dwCurrentState := SERVICE_PAUSED;
        SetServiceStatus(FStatusHandle, FStatus);
      end;
    SERVICE_CONTROL_CONTINUE:
      begin
        FPaused := False;
        FStatus.dwCurrentState := SERVICE_RUNNING;
        SetServiceStatus(FStatusHandle, FStatus);
      end;
    SERVICE_CONTROL_INTERROGATE: SetServiceStatus(FStatusHandle, FStatus);
    SERVICE_CONTROL_SHUTDOWN: FStopped := True;
  end;
end;

procedure TNovusSvcMgr.InstallService(FileName: string);
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  Args: pchar;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;
  try
    Service := CreateService(SCManager, FServiceName, FDisplayName, SERVICE_ALL_ACCESS,
      SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS, SERVICE_AUTO_START,
      SERVICE_ERROR_IGNORE, PChar(FileName), nil, nil, nil, nil, nil);

    Args := NIL;

    StartService(Service, 0, Args);
    CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure TNovusSvcMgr.UninstallService;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;
  try
    Service := OpenService(SCManager, FServiceName, SERVICE_ALL_ACCESS);
    ControlService(Service, SERVICE_CONTROL_STOP, FStatus);
    DeleteService(Service);
    CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure TNovusSvcMgr.Run;
var
  ServiceTable: array [0..1] of TServiceTableEntry;
begin
  if Assigned(FLog) then FLog.AddLogInformation('Start Run');

  ServiceTable[0].lpServiceName := FServiceName;
  ServiceTable[0].lpServiceProc := @TNovusSvcMgr.ServiceCtrlDispatcher;
  ServiceTable[1].lpServiceName := nil;
  ServiceTable[1].lpServiceProc := nil;


  StartServiceCtrlDispatcher(ServiceTable[0]);

  if Assigned(FLog) then FLog.AddLogInformation('End Run');
end;

procedure TNovusSvcMgr.ServiceCtrlDispatcher(dwArgc: DWORD; var lpszArgv: PChar); stdcall;
begin
  FStatusHandle := RegisterServiceCtrlHandler(FServiceName, @TNovusSvcMgr.ServiceCtrlHandler);
  if FStatusHandle <> 0 then
  begin
    ZeroMemory(@FStatus, SizeOf(FStatus));
    FStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
    FStatus.dwCurrentState := SERVICE_START_PENDING;
    FStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP or SERVICE_ACCEPT_PAUSE_CONTINUE;
    FStatus.dwWaitHint := 1000;
    SetServiceStatus(FStatusHandle, FStatus);
    FStopped := False;
    FPaused := False;
    FStatus.dwCurrentState := SERVICE_RUNNING;
    SetServiceStatus(FStatusHandle, FStatus);
    ServiceMain;
    FStatus.dwCurrentState := SERVICE_STOPPED;
    SetServiceStatus(FStatusHandle, FStatus);
  end;
end;

function TNovusSvcMgr.IsServiceInstalled: Boolean;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager = 0 then Exit;
  try
    Service := OpenService(SCManager, FServiceName, SERVICE_QUERY_STATUS);
    if Service <> 0 then
    begin
      Result := True;
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

end.

