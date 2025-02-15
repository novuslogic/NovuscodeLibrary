program Logger4;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  NovusLogger in '..\..\..\..\Source\Core\Log\NovusLogger.pas',
  NovusLogger.Provider.Console in '..\..\..\..\Source\Core\Log\NovusLogger.Provider.Console.pas',
  NovusLogger.Provider in '..\..\..\..\Source\Core\Log\NovusLogger.Provider.pas',
  NovusUtilities in '..\..\..\..\Source\Core\Utilities\NovusUtilities.pas',
  NovusObject in '..\..\..\..\Source\Core\Object\NovusObject.pas',
  NovusConsole in '..\..\..\..\Source\Core\Console\NovusConsole.pas',
  NovusLogger.Provider.SysLog in '..\..\..\..\Source\Core\Log\NovusLogger.Provider.SysLog.pas',
  NovusStringUtils in '..\..\..\..\Source\Core\Utilities\NovusStringUtils.pas',
  NovusVariants in '..\..\..\..\Source\Core\Utilities\NovusVariants.pas',
  NovusFileUtils in '..\..\..\..\Source\Core\Utilities\NovusFileUtils.pas',
  NovusWindows in '..\..\..\..\Source\Core\Utilities\NovusWindows.pas';

Var Log: tNovusLogger;

begin
  ReportMemoryLeaksOnShutdown := True;

  Try
    Log := tNovusLogger.Create([TNovusLogger_Provider_Console.Create,
              TNovusLogger_Provider_SysLog.Create('127.0.0.1', 514)]);

    if Log.OpenLog then
     begin
        Log.AddLogInformation('Infomation');
        Log.AddLogSuccess('Success');
        Log.AddLogError('Error');
        Log.AddLogWarning('Warning');
        Log.AddLogDebug('Debug');
        Log.AddLogException('Exception');
        Log.AddLogSystem('System');

        Log.CloseLog;
     end;
  Finally
    Log.Free;
  End;
end.
