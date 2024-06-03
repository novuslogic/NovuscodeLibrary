program Logger1;

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
  NovusList in '..\..\..\..\Source\Core\Object\NovusList.pas';

Var Log: tNovusLogger;

begin
  Try
    Log := tNovusLogger.Create([TNovusLogger_Provider_Console.Create]);

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
end.
