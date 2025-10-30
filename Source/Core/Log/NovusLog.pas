unit NovusLog;

interface

Uses Classes, NovusStringUtils, NovusList, NovusUtilities, SysUtils, NovusLogger,
  NovusBO, NovusObject, NovusFileUtils, System.IOUtils, NovusLogger.Provider.Console,
  NovusLogger.Provider.Files, NovusLogger.Provider;

Type
  TNovusLog = Class(TNovusObject)
  private
  protected
    fLogger: tNovusLogger;
  public
    constructor Create;
    destructor Destroy; override;

    function OpenLog: Boolean;
    function CloseLog: boolean;

    procedure WriteLog(aMsg: string); virtual;
    function WriteExceptLog(aMsg: string = ''): string; virtual;
 //   function FormatedNow(const aDate: tDateTime = 0): String; virtual;

    property Logger: tNovusLogger
      read fLogger write fLogger;
  end;

  TNovusLogFile = Class(TNovusLog)
  private
  protected
    fboutputConsole: boolean;
    procedure SetFilename(value: String);
    function GetFilename: String;
  public
    constructor Create(aFilename: String); virtual;
    destructor Destroy; override;

    procedure WriteLog(aMsg: string); override;
    function WriteExceptLog(aMsg: string = ''): string; override;

    property OutputConsole: Boolean read fboutputConsole write fboutputConsole;
    property Filename: string read GetFilename write SetFilename;
  end;

implementation

// TNovusLog
constructor TNovusLog.Create;
begin
  inherited Create;
end;

destructor TNovusLog.Destroy;
begin
  inherited;
end;

procedure TNovusLog.WriteLog(aMsg: string);
begin
  //
end;


function TNovusLog.WriteExceptLog(aMsg: string = ''): string;
begin
  result := '';
end;

(*
function TNovusLog.FormatedNow(const aDate: tDateTime = 0): String;
begin
  Result := '';
end;
*)

function TNovusLog.OpenLog: Boolean;
begin
  result := false;
  If Assigned(flogger) then Result := flogger.OpenLog;
end;

function TNovusLog.CloseLog: boolean;
begin
  result := false;
  If Assigned(flogger) then Result := flogger.CloseLog;
end;


// TNovusLogFile
constructor TNovusLogFile.Create(aFilename: String);
begin
  fLogger := tNovusLogger.Create([TNovusLogger_Provider_Console.Create,
              TNovusLogger_Provider_Files.Create(aFilename)]);


  fboutputConsole := true;

  Filename := aFilename;
end;

destructor TNovusLogFile.Destroy;
begin
  inherited;
end;

(*
function TNovusLogFile.FormatedNow(const aDate: tDateTime = 0): String;
var
  tmpDate: tDateTime;
begin
  tmpDate := aDate;
  If aDate = 0 then tmpDate := Now;

  If OutputConsole then Result := Logger.FormatLogOutput('', tmpDate, TSeverityType.stInformation, TNovusLogger_Provider_Console);
  Result :=Logger.FormatLogOutput('', aDate, TSeverityType.stInformation);
end;
*)


procedure TNovusLogFile.WriteLog(aMsg: string);
begin
  If OutputConsole then Logger.AddLogInformation(aMsg, TNovusLogger_Provider_Console);
  Logger.AddLogInformation(aMsg, TNovusLogger_Provider_Files)
end;


function TNovusLogFile.WriteExceptLog(aMsg: string = ''): string;
begin
  If OutputConsole then Result := Logger.AddLogException(aMsg, TNovusLogger_Provider_Console);
  Logger.AddLogInformation(aMsg, TNovusLogger_Provider_Files);
end;

procedure TNovusLogFile.SetFilename(value: String);
begin
  Var Provider := Logger.FindProvider(TNovusLogger_Provider_Files);
  If Assigned(Provider) then TNovusLogger_Provider_Files(Provider).FileName := Value;
end;

function TNovusLogFile.GetFilename: String;
begin
  Result := '';
  Var Provider := Logger.FindProvider(TNovusLogger_Provider_Files);
  If Assigned(Provider) then Result := TNovusLogger_Provider_Files(Provider).FileName;
end;

end.
