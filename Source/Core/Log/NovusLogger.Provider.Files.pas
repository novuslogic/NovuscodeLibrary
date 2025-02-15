unit NovusLogger.Provider.Files;

interface

Uses NovusLogger.Provider,  System.SysUtils, Winapi.Windows,
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
    procedure WriteFile(aLogMessage: String);
  Public
    constructor Create(aFilename: String;
                       aDailyRotate: Boolean = false;
                       aMaxRotateFiles: Integer = 10;
                       aLimitLogSize: Int64 = 0); overload;

    function OpenLog: Boolean; override;
    function CloseLog: Boolean;  override;

    procedure SendLogMessage(aLogMessage: String;aLogDateTime: tDateTime; aSeverityType: TSeverityType); override;

    function AddLog(aLogMessage: string; aLogDateTime: tDateTime; aSeverityType: TSeverityType): string; override;

    procedure AddLogSuccess(aLogMessage: string); override;
    procedure AddLogInformation(aLogMessage : string); override;
    procedure AddLogError(aLogMessage: string); overload; override;
    procedure AddLogError(aException: Exception); overload; override;
    procedure AddLogWarning(aLogMessage: string); override;
    procedure AddLogDebug(aLogMessage: string); override;
    function AddLogException(aLogMessage: string): string; overload; override;
    procedure AddLogSystem(aLogMessage: string); override;

    property Filename: String  read fsFilename write fsFilename;

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



function TNovusLogger_Provider_Files.AddLog(aLogMessage: string; aLogDateTime: tDateTime; aSeverityType: TSeverityType): string;
begin
  Result := FormatLogOutput(aLogMessage, aLogDateTime,aSeverityType);

  (Logger as TNovusLogger).PushLogMessage(FormatLogOutput(aLogMessage, aLogDateTime,aSeverityType),  aLogDateTime,aSeverityType,  Self);
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

function TNovusLogger_Provider_Files.AddLogException(aLogMessage: string): string;
begin
  result := AddLog(aLogMessage, Now, TSeverityType.stException);
end;

procedure TNovusLogger_Provider_Files.WriteFile(aLogMessage: String);
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

      FStreamWriter.Write(aLogMessage+  sLineBreak);

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


procedure TNovusLogger_Provider_Files.SendLogMessage(aLogMessage: String; aLogDateTime: tDateTime; aSeverityType: TSeverityType);
begin
  WriteFile(aLogMessage);
end;


end.
