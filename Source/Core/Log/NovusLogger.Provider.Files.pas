unit NovusLogger.Provider.Files;

interface

Uses NovusLogger.Provider, NovusConsole, System.SysUtils, Winapi.Windows,
     System.Threading, System.Classes, NovusObject, NovusStringUtils,
     NovusFileUtils, System.IOUtils;

type
  TNovusLogger_Provider_Files = class(tNovusLogger_Provider)
  private
  protected
    fbIsFileOpen: Boolean;
    fiMaxRotateFiles: Integer;
    fbDailyRotate: Boolean;
    fsFilename: String;
    fiLimitLogSize : Int64;
    procedure WriteLine(aLogMessage: String);
  Public
    constructor Create(aFilename: String;
                       aDailyRotate: Boolean = false;
                       aMaxRotateFiles: Integer = 10;
                       aLimitLogSize: Int64 = 0); overload;

    function OpenLog: Boolean; override;
    function CloseLog: Boolean;  override;

    procedure SendLogMessage(aLogMessage: String); override;

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

Uses NovusLogger;

constructor TNovusLogger_Provider_Files.Create(aFilename: String;
                                aDailyRotate: Boolean;
                                aMaxRotateFiles: Integer;
                                aLimitLogSize: Int64);
begin
  fsfilename := aFilename;
  fiLimitLogSize := aLimitLogSize;
  fbDailyRotate := aDailyRotate;
  fiMaxRotateFiles := aMaxRotateFiles;

  inherited Create;
end;

function  TNovusLogger_Provider_Files.OpenLog: Boolean;
begin
  Result := false;

  If fbIsFileOpen then
    Exit;

  var lsCurrentFolder := TNovusStringUtils.JustPathname(fsFilename);

  if Trim(lsCurrentFolder) <> '' then
    begin
      if Not DirectoryExists(lsCurrentFolder) then
       Exit;
    end;

  if FileExists(fsFilename) then
  begin
    if not TNovusFileUtils.IsFileInUse(fsFilename) then
           TFile.Delete(fsFilename);
  end;

  fbIsFileOpen := true;

  Result := fbIsFileOpen;
end;

function  TNovusLogger_Provider_Files.CloseLog: Boolean;
begin
  result := True;

  fbIsFileOpen := false;

  FlushLog;
end;



procedure TNovusLogger_Provider_Files.AddLog(aLogMessage: string; aDateTime: tDateTime; aSeverityType: TSeverityType);
begin
  (Logger as TNovusLogger).PushLogMessage(FormatLogOutput(aLogMessage,aDateTime,aSeverityType), Self);
end;

procedure TNovusLogger_Provider_Files.AddLogSuccess(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stSuccess);
end;

procedure TNovusLogger_Provider_Files.AddLogInformation(aLogMessage : string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stInformation);
end;

procedure TNovusLogger_Provider_Files.AddLogError(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stError);
end;
procedure TNovusLogger_Provider_Files.AddLogError(aException: Exception);
begin
  if assigned(aException) then
    AddLogError(aException.Message);
end;

procedure TNovusLogger_Provider_Files.AddLogWarning(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stWarning);
end;

procedure TNovusLogger_Provider_Files.AddLogDebug(aLogMessage: string);
begin
   AddLog(aLogMessage, Now, TSeverityType.stDebug);
end;

procedure TNovusLogger_Provider_Files.AddLogSystem(aLogMessage: string);
begin
   AddLog(aLogMessage, Now, TSeverityType.stSystem);
end;

procedure TNovusLogger_Provider_Files.AddLogException(aLogMessage: string);
begin
  AddLog(aLogMessage, Now, TSeverityType.stException);
end;

procedure TNovusLogger_Provider_Files.WriteLine(aLogMessage: String);
Var
  FStreamWriter: tStreamWriter;
  liRetryCount: Integer;
begin
  liRetryCount := 0;

  while liRetryCount < RetryCount do
  begin
    try
      FStreamWriter := TStreamWriter.Create(fsfilename,true);
      FStreamWriter.AutoFlush := true;

      FStreamWriter.WriteLine(aLogMessage);

      FStreamWriter.Flush;

      Freeandnil(FStreamWriter);

      Exit;
     except
      on E: EFOpenError do
      begin
        // If the error is "The process cannot access the file because it is being used by another process",
        // wait for a moment and then retry the file write
        if Pos('The process cannot access the file because it is being used by another process', E.Message) > 0 then
        begin
          Inc(liRetryCount);
          Sleep(1000); // Wait for 1 second before retrying
          Continue; // Continue to the next iteration of the loop
        end;
      end;
      on E: Exception do
      begin
        raise Exception.Create('An error occurred while writing to the file: ' + E.Message);

        Exit;
      end;
    end;
  end;
end;


procedure TNovusLogger_Provider_Files.SendLogMessage(aLogMessage: String);
begin
  WriteLine(aLogMessage);
end;


end.
