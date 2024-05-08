unit NovusSvcMgr;

interface

uses
  Winapi.Windows, Winapi.WinSvc, System.SysUtils, Vcl.SvcMgr, NovusFileUtils,
  NovusUtilities;

type
(*
  TServiceStatus = record
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
  end;
*)


  TNovusSvcConfig = record
    IsLocalSystem: Boolean;
    Account: String;
  end;

  TNovusServiceStatus = (tSSInstall, tSSUninstall, tSSExists, tSSNone, tSSError);


  TNovusSvcMgr = class(TNovusUtilities)
  private
    fdwLastError: DWord;
    fsSysErrorMessage: string;
    FsServiceName: string;
    function GetSCManagerHandle: SC_HANDLE;
    function GetServiceHandle: SC_HANDLE;
  public
    constructor Create(const aServiceName: string);
    function IsExists: Boolean; overload;
    function Install(const aDisplayName, aBinaryPath: string;
      aStartType: TStartType = stAuto): Boolean;
    function Uninstall: Boolean;
    function Start: Boolean;
    function Stop: Boolean;
    function Query(var aSvcConfig: TNovusSvcConfig): Boolean;
    function Change(const aSvcConfig: TNovusSvcConfig): Boolean;

    class function SvcMgrApplication: TServiceApplication;

    class function SvcMgrComboService(aServiceName, aDisplayName, aBinaryPath: string;
      aStartType: TStartType = stAuto): TNovusServiceStatus;
  end;

implementation

constructor TNovusSvcMgr.Create(const aServiceName: string);
begin
  inherited Create;
  FsServiceName := StringReplace(aServiceName, ' ', '_', [rfReplaceAll]);
end;

function TNovusSvcMgr.GetSCManagerHandle: SC_HANDLE;
begin
  Result := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
end;

function TNovusSvcMgr.GetServiceHandle: SC_HANDLE;
var
  SCM: SC_HANDLE;
begin
  Result := 0;
  SCM := GetSCManagerHandle;
  if SCM > 0 then
    try
      Result := OpenService(SCM, PChar(FsServiceName), SERVICE_ALL_ACCESS);
    finally
      CloseServiceHandle(SCM);
    end;
end;

function TNovusSvcMgr.Install(const aDisplayName, aBinaryPath: string;
  aStartType: TStartType = stAuto): Boolean;
var
  SCM, Service: SC_HANDLE;
  fSvcQueryConfig: TNovusSvcConfig;
begin
  SCM := GetSCManagerHandle;
  if SCM = 0 then
    begin
      fdwLastError := GetLastError;
      fsSysErrorMessage := SysErrorMessage(fdwLastError);

      Exit(False);
    end;
  try
    Service := Winapi.WinSvc.CreateService(SCM, PChar(FsServiceName),
      PChar(aDisplayName), SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS,
      DWord(aStartType), SERVICE_ERROR_NORMAL, PChar(aBinaryPath), nil, nil,
      nil, nil, nil);
    if Service > 0 then
    begin
      CloseServiceHandle(Service);

      Result := True;
    end
    else
      begin
        Result := False;

        fdwLastError := GetLastError;
        fsSysErrorMessage := SysErrorMessage(fdwLastError);
      end;


  finally
    CloseServiceHandle(SCM);
  end;
end;

function TNovusSvcMgr.Uninstall: Boolean;
var
  Service: SC_HANDLE;

begin
  Result := False;

  Service := GetServiceHandle;
  if Service = 0 then
    Exit(False);
  try
    Result := DeleteService(Service);
    if Not result then
      begin
        fdwLastError := GetLastError;
        fsSysErrorMessage := SysErrorMessage(fdwLastError);
      end;


  finally
    CloseServiceHandle(Service);
  end;
end;

function TNovusSvcMgr.Start: Boolean;
var
  Service: SC_HANDLE;
  pArgs: PWideChar;
begin
  Service := GetServiceHandle;
  if Service > 0 then
    try
      Result := Winapi.WinSvc.StartService(Service, 0, pArgs);
    finally
      CloseServiceHandle(Service);
    end
  else
    begin
      Result := False;

      fdwLastError := GetLastError;
      fsSysErrorMessage := SysErrorMessage(fdwLastError);
    end;
end;

function TNovusSvcMgr.Stop: Boolean;
var
  Service: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  Service := GetServiceHandle;
  if Service > 0 then
    try
      Result := ControlService(Service, SERVICE_CONTROL_STOP, ServiceStatus);
    finally
      CloseServiceHandle(Service);
    end
  else
    begin
      Result := False;

      fdwLastError := GetLastError;
      fsSysErrorMessage := SysErrorMessage(fdwLastError);
    end;
end;

function TNovusSvcMgr.IsExists: Boolean;
var
  ServiceHandle: SC_HANDLE;
begin
  ServiceHandle := GetServiceHandle;
  if ServiceHandle > 0 then
  begin
    CloseServiceHandle(ServiceHandle);
    Result := True;
  end
  else
    Result := False;
end;

function TNovusSvcMgr.Query(var aSvcConfig: TNovusSvcConfig): Boolean;
var
  SCManager, SvcHandle: SC_HANDLE;
  ServiceConfig: PQueryServiceConfig;
  BytesNeeded: DWORD;
begin
  Result := False;

  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCManager = 0 then
    begin
      fdwLastError := GetLastError;
      fsSysErrorMessage := SysErrorMessage(fdwLastError);

      Exit;
    end;

  try
    SvcHandle := OpenService(SCManager,PChar(FsServiceName), SERVICE_QUERY_CONFIG);
    if SvcHandle = 0 then
      begin
        fdwLastError := GetLastError;
        fsSysErrorMessage := SysErrorMessage(fdwLastError);

        Exit;
      end;

    try
      if not QueryServiceConfig(SvcHandle, nil, 0, BytesNeeded) and
         (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        GetMem(ServiceConfig, BytesNeeded);
        try
          if not QueryServiceConfig(SvcHandle, ServiceConfig, BytesNeeded, BytesNeeded) then
            begin
              fdwLastError := GetLastError;
              fsSysErrorMessage := SysErrorMessage(fdwLastError);

              Exit;
            end;


          // Now you can access ServiceConfig^ fields
          // For example, to check if the service runs under LocalSystem account:
          if ServiceConfig^.lpServiceStartName = 'LocalSystem' then
            begin
              aSvcConfig.IsLocalSystem := True;
              aSvcConfig.Account := 'LocalSystem';


            end
          else
            begin
              aSvcConfig.Account := ServiceConfig^.lpServiceStartName;
            end;
        finally
          FreeMem(ServiceConfig);
        end;
      end;
    finally
      CloseServiceHandle(SvcHandle);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;


class function TNovusSvcMgr.SvcMgrApplication: TServiceApplication;
begin
  Result := Vcl.SvcMgr.Application;
end;

class function TNovusSvcMgr.SvcMgrComboService(aServiceName, aDisplayName,
  aBinaryPath: string; aStartType: TStartType = stAuto): TNovusServiceStatus;
var
  FSvcMgr: TNovusSvcMgr;
  fbIsService, fbinstallService, fbuninstallService, fbexistsService: Boolean;
begin
  Result := tSSNone;

  FSvcMgr := NIl;

  Try
    FSvcMgr := TNovusSvcMgr.Create(aServiceName);

    if aDisplayName = '' then
      aDisplayName := FSvcMgr.FsServiceName;

    if aBinaryPath = '' then
      aBinaryPath := TNovusFileUtils.TrailingBackSlash(TNovusFileUtils.AppRootDirectory) + TNovusFileUtils.AppFilename;

    fbexistsService := FSvcMgr.IsExists;
    fbinstallService := FindCmdLineSwitch('install', ['-', '\', '/'], True);
    fbuninstallService := FindCmdLineSwitch('uninstall', ['-', '\', '/'], True);

    fbIsService := (fbexistsService or fbinstallService or fbuninstallService);

    if fbIsService then
    begin
      if fbinstallService then
        begin
           if FSvcMgr.Install(aDisplayName, aBinaryPath) then Result := TNovusServiceStatus.tSSInstall
           else
              Result := TNovusServiceStatus.tSSError;
        end
      else
      if fbuninstallService then
        begin
          If FSvcMgr.unInstall then Result := TNovusServiceStatus.tSSUninstall
          else Result := TNovusServiceStatus.tSSError;
        end
      else Result := TNovusServiceStatus.tSSExists;
    end;
  Finally
    if Assigned(FSvcMgr) then FSvcMgr.Free;
  End;
end;

function TNovusSvcMgr.Change(const aSvcConfig: TNovusSvcConfig): Boolean;
var
  SCManager, SvcHandle: SC_HANDLE;
begin
  Result := False;

  // Open the Service Control Manager
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    begin
      fdwLastError := GetLastError;
      fsSysErrorMessage := SysErrorMessage(fdwLastError);


      Exit;
    end;

  try
    // Open the specific service
    SvcHandle := OpenService(SCManager, PChar(FsServiceName), SERVICE_CHANGE_CONFIG);
    if SvcHandle = 0 then
      begin
        fdwLastError := GetLastError;
        fsSysErrorMessage := SysErrorMessage(fdwLastError);

        Exit;
      end;
    try
      // Change the service configuration
      if not ChangeServiceConfig(
        SvcHandle,                // service handle
        SERVICE_NO_CHANGE,        // service type: no change
        SERVICE_NO_CHANGE,        // start type: no change
        SERVICE_NO_CHANGE,        // error control: no change
        nil,                      // binary path: no change
        nil,                      // load order group: no change
        nil,                      // tag ID: no change
        nil,                      // dependencies: no change
        'NT AUTHORITY\NetworkService', // account name
        nil,                      // password: no change
        nil                       // display name: no change
      ) then
        begin
          fdwLastError := GetLastError;
          fsSysErrorMessage := SysErrorMessage(fdwLastError);

          Exit;
        end;

      Result := True;
    finally
      CloseServiceHandle(SvcHandle);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

end.
