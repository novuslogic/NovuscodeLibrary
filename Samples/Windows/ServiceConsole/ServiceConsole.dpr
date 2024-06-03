program ServiceConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  Winapi.WinSvc,
  System.Threading,
  NovusSvcMgr in '..\..\..\Source\Core\Utilities\NovusSvcMgr.pas',
  NovusWindows in '..\..\..\Source\Core\Utilities\NovusWindows.pas',
  NovusLogger in '..\..\..\Source\Core\Log\NovusLogger.pas',
  NovusLogger.Provider in '..\..\..\Source\Core\Log\NovusLogger.Provider.pas',
  NovusObject in '..\..\..\Source\Core\Object\NovusObject.pas',
  NovusConsole in '..\..\..\Source\Core\Console\NovusConsole.pas',
  NovusUtilities in '..\..\..\Source\Core\Utilities\NovusUtilities.pas',
  NovusLogger.Provider.Files in '..\..\..\Source\Core\Log\NovusLogger.Provider.Files.pas',
  NovusLogger.Provider.Console in '..\..\..\Source\Core\Log\NovusLogger.Provider.Console.pas',
  NovusFileUtils in '..\..\..\Source\Core\Utilities\NovusFileUtils.pas',
  NovusStringUtils in '..\..\..\Source\Core\Utilities\NovusStringUtils.pas',
  NovusVariants in '..\..\..\Source\Core\Utilities\NovusVariants.pas';

var
  ServiceStatus: TServiceStatus;
  ServiceStatusHandle: THandle;
  StopEvent: THandle;


procedure ServiceControlHandler(Control: DWord); stdcall;
begin
  case Control of
    SERVICE_CONTROL_STOP: begin
      ServiceStatus.dwCurrentState := SERVICE_STOP_PENDING;
      SetServiceStatus(ServiceStatusHandle, ServiceStatus);

      SetEvent(StopEvent); // signal the service to stop
    end;
  end;
end;

procedure ServiceMain(Argc: DWord; Argv: PWideChar); stdcall;
var
  LogFile: TextFile;
begin
  StopEvent := CreateEvent(nil, True, False, nil);

  ServiceStatusHandle := RegisterServiceCtrlHandler('MyService', @ServiceControlHandler);

  ServiceStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS;
  ServiceStatus.dwCurrentState := SERVICE_RUNNING;
  ServiceStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP;
  SetServiceStatus(ServiceStatusHandle, ServiceStatus);

  AssignFile(LogFile, 'D:\Delphi Components\NovuscodeLibrary\Samples\Windows\ServiceConsole\Win32\Debug\LogFile.txt');
  Rewrite(LogFile);

  while WaitForSingleObject(StopEvent, 1000) = WAIT_TIMEOUT do
  begin
    Writeln(LogFile, Format('Service is running: %s', [DateTimeToStr(Now)]));
    Flush(LogFile); // Ensure the message is written immediately
  end;

  CloseFile(LogFile);
  CloseHandle(StopEvent);

  ServiceStatus.dwCurrentState := SERVICE_STOPPED;
  SetServiceStatus(ServiceStatusHandle, ServiceStatus);
end;

var
  Log: tNovusLogger;
  ServiceTable: array[0..1] of TServiceTableEntry;

begin
  Try
    Log := tNovusLogger.Create([TNovusLogger_Provider_Console.Create,
                               TNovusLogger_Provider_Files.Create('Service.log')]);

    Log.OpenLog;

    Log.AddLogInformation('Infomation');
    Log.AddLogSuccess('Success');
    Log.AddLogError('Error');
    Log.AddLogWarning('Warning');
    Log.AddLogDebug('Debug');
    Log.AddLogException('Exception');
    Log.AddLogSystem('System');

    Log.CloseLog;
  Finally
    Log.Free;
  End;








  (*


  ServiceTable[0].lpServiceName := 'MyService';
  ServiceTable[0].lpServiceProc := @ServiceMain;
  ServiceTable[1].lpServiceName := nil;
  ServiceTable[1].lpServiceProc := nil;

  if not StartServiceCtrlDispatcher(ServiceTable[0]) then
    RaiseLastOSError;
  *)
end.
