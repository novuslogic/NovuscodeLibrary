unit NovusLogger.Provider.Console;

interface

Uses NovusLogger.Provider, NovusConsole, System.SysUtils, Winapi.Windows,
     System.Threading, System.Classes, NovusObject;

type
  TNovusLogger_Provider_Console = class(tNovusLogger_Provider)
  private
  protected
     fCurrTextColour,
     fCurrBgColour: Word;

     procedure GetCurrentConsoleColour;
     procedure RestoreCurrentConsoleColour;

     procedure SetConsoleColour(aTextColour: word);
  Public
    constructor Create; override;

    procedure AddLog(aLogMessage: string; aDateTime: tDateTime; aSeverityType: TSeverityType); override;

    procedure AddLogSuccess(aLogMessage: string); override;
    procedure AddLogInformation(aLogMessage : string); override;
    procedure AddLogError(aLogMessage: string); overload; override;
    procedure AddLogError(aException: Exception); overload; override;
    procedure AddLogWarning(aLogMessage: string); override;
    procedure AddLogDebug(aLogMessage: string); override;
    procedure AddLogException(aLogMessage: string); overload; override;
    procedure AddLogSystem(aLogMessage: string); override;
  end;

implementation

constructor TNovusLogger_Provider_Console.Create;
begin
  inherited Create;

  fCurrTextColour := 0;
  fCurrBgColour := 0;

end;

procedure TNovusLogger_Provider_Console.GetCurrentConsoleColour;
var
  lCurrTextColour, lCurrBgColour: word;
begin
  lCurrTextColour :=0;
  lCurrBgColour := 0;

  if (fCurrTextColour = 0) and (fCurrBgColour = 0) then
    begin
      if TNovusConsole.GetCurrentConsoleColour(lCurrTextColour, lCurrBgColour) then
        begin
          fCurrTextColour :=  lCurrTextColour;
          fCurrBgColour := lCurrBgColour;
        end;
    end;
end;

procedure TNovusLogger_Provider_Console.RestoreCurrentConsoleColour;
begin
   TNovusConsole.SetConsoleColour(  fCurrTextColour,
     fCurrBgColour);
end;

procedure TNovusLogger_Provider_Console.AddLog(aLogMessage: string; aDateTime: tDateTime; aSeverityType: TSeverityType);
begin
  writeln(FormatLogOutput(aLogMessage, aDateTime, aSeverityType));

  RestoreCurrentConsoleColour;
end;

procedure TNovusLogger_Provider_Console.AddLogSuccess(aLogMessage: string);
begin
  GetCurrentConsoleColour;

  SetConsoleColour(FOREGROUND_GREEN);

  AddLog(aLogMessage, Now, TSeverityType.stSuccess);
end;

procedure TNovusLogger_Provider_Console.AddLogInformation(aLogMessage : string);
begin
  GetCurrentConsoleColour;

  SetConsoleColour(FOREGROUND_BLUE);

  AddLog(aLogMessage, Now, TSeverityType.stInformation);
end;

procedure TNovusLogger_Provider_Console.SetConsoleColour(aTextColour: word);
begin
  TNovusConsole.SetConsoleColour(  aTextColour,
     fCurrBgColour);
end;

procedure TNovusLogger_Provider_Console.AddLogError(aLogMessage: string);
Var
  Task: ITask;
begin
  GetCurrentConsoleColour;

  SetConsoleColour(FOREGROUND_RED);

  AddLog(aLogMessage, Now, TSeverityType.stError);
end;

procedure TNovusLogger_Provider_Console.AddLogError(aException: Exception);
begin
  if assigned(aException) then
    AddLogError(aException.Message);
end;

procedure TNovusLogger_Provider_Console.AddLogWarning(aLogMessage: string);
begin
  GetCurrentConsoleColour;

  SetConsoleColour(FOREGROUND_YELLOW);

  AddLog(aLogMessage, Now, TSeverityType.stWarning);
end;

procedure TNovusLogger_Provider_Console.AddLogDebug(aLogMessage: string);
begin
  GetCurrentConsoleColour;

  SetConsoleColour(FOREGROUND_WHITE);

  AddLog(aLogMessage, Now, TSeverityType.stDebug);
end;

procedure TNovusLogger_Provider_Console.AddLogSystem(aLogMessage: string);
begin
  GetCurrentConsoleColour;

  SetConsoleColour(FOREGROUND_WHITE);

  AddLog(aLogMessage, Now, TSeverityType.stSystem);
end;

procedure TNovusLogger_Provider_Console.AddLogException(aLogMessage: string);
begin
  GetCurrentConsoleColour;

  SetConsoleColour(FOREGROUND_RED);

  AddLog(aLogMessage, Now, TSeverityType.stException);
end;


end.
