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
  NovusVariants in '..\..\..\Source\Core\Utilities\NovusVariants.pas',
  ServiceMain in 'ServiceMain.pas';


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


  CloseHandle(StopEvent);

  ServiceStatus.dwCurrentState := SERVICE_STOPPED;
  SetServiceStatus(ServiceStatusHandle, ServiceStatus);
end;

var
  MyService: TMyService;
  Log: TNovusLogger;
begin
  Try
    Log := tNovusLogger.Create([TNovusLogger_Provider_Files.Create('Service.log')]);
    Log.AddLogInformation('Start Log');
    MyService := TMyService.Create('MyService', 'Demo MyService', Log);
    MyService.Run;
  Finally
    MyService.Free;

    Log.AddLogInformation('End Log');

    Log.Free;
  end;
end.

end.
