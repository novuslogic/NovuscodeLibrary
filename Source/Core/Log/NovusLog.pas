unit NovusLog;

interface

Uses Classes, NovusStringUtils, NovusList, NovusUtilities, SysUtils,
  NovusBO, NovusWinEventLog, NovusFileUtils, System.IOUtils, System.Threading;

Type
  TLogType = (ltFile, ltWinEventType);

  TNovusLogDetails = class(TNovusBO)
  private
    fiLogID: Integer;
    fdtLogDateTime: tDateTime;
    fsLogDesc: String;
    fsDateTimeMask: String;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    Property LogID: Integer read fiLogID write fiLogID;

    property LogDateTime: tDateTime read fdtLogDateTime write fdtLogDateTime;

    property LogDesc: String read fsLogDesc write fsLogDesc;

    property DateTimeMask: String read fsDateTimeMask write fsDateTimeMask;

  end;

  TNovusLog = Class(TNovusBO)
  private
    fiLastLogID,
    fiRetryCount: Integer;
    foLogDetailsList: TNovusList;
    foEventLog: tNovusWinEventLog;
    fsDateTimeMask: String;
  protected
  public
    constructor Create(ALogType: TLogType);
    destructor Destroy; override;

    function FormatedNow(const aDate: tDateTime = 0): String;

    procedure AddLogDetails(ALogDesc: string; ALogDateTime: tDateTime);

    function WriteLog(AMsg: string; AEventType: TEventType = etNone)
      : string; virtual;
    function WriteExceptLog: String; virtual;

    Property oLogDetailsList: TNovusList read foLogDetailsList
      write foLogDetailsList;

    property DateTimeMask: String read fsDateTimeMask write fsDateTimeMask;

    property oEventLog: tNovusWinEventLog read foEventLog write foEventLog;

    property LastLogID: Integer read fiLastLogID write fiLastLogID;

    property RetryCount: Integer
      read fiRetryCount write fiRetryCount;
  end;

  TNovusLogFile = Class(TNovusLog)
  private
  protected
    fboutputConsole: Boolean;
    fcSeparator: char;
    fbIsFileOpen: Boolean;
    fsFilename: string;
    FFilePonter: text;
    fsPathName: String;
  public
    constructor Create(aFilename: String); virtual;
    destructor Destroy; override;

    function WriteLine(ATimeStr, ALogDesc: string;
      ALogDateTime: tDateTime): String;

    function OpenLog(aOveride: Boolean = false): Boolean; virtual;
    procedure CloseLog; virtual;

    function WriteLog(AMsg: string; AEventType: TEventType = etNone)
      : string; override;
    function WriteExceptLog: String; override;

    property Filename: String read fsFilename write fsFilename;

    property Separator: char read fcSeparator write fcSeparator;

    property OutputConsole: Boolean read fboutputConsole write fboutputConsole;
  end;

implementation

// TNovusLog
constructor TNovusLog.Create(ALogType: TLogType);
begin
  inherited Create;

  DateTimeMask := FormatSettings.ShortDateFormat + ' hh:mm';

  foLogDetailsList := TNovusList.Create(TNovusLogDetails);

  fiRetryCount := 3;

  foEventLog := tNovusWinEventLog.Create;
end;

destructor TNovusLog.Destroy;
begin
  foEventLog.Free;

  foLogDetailsList.Free;

  inherited;
end;

constructor TNovusLogDetails.Create;
begin
  inherited Create;
end;

destructor TNovusLogDetails.Destroy;
begin
  inherited;
end;

function TNovusLog.FormatedNow(const aDate: tDateTime = 0): String;
var
  fDate: tDateTime;
begin
  fDate := aDate;
  if aDate = 0 then
    fDate := Now;

  Result := FormatDateTime(DateTimeMask, fDate)
end;

procedure TNovusLog.AddLogDetails;
Var
  loLogDetails: TNovusLogDetails;
begin
  Inc(fiLastLogID);

  loLogDetails := TNovusLogDetails.Create;

  loLogDetails.LogID := fiLastLogID;

  loLogDetails.LogDateTime := ALogDateTime;
  loLogDetails.LogDesc := ALogDesc;

  loLogDetails.DateTimeMask := DateTimeMask;

  foLogDetailsList.Add(loLogDetails);
end;

function TNovusLog.WriteLog(AMsg: string;
  AEventType: TEventType = etNone): String;
Var
  ldDataTime: tDateTime;
begin
  ldDataTime := Now;

  AddLogDetails(AMsg, ldDataTime);

  If AEventType = etNone then
  begin
    Self.oEventLog.EventType := AEventType;

    Self.oEventLog.LogEvent(AMsg);
  end;
end;

function TNovusLog.WriteExceptLog: String;
begin
  Result := WriteLog(TNovusUtilities.GetExceptMess);
end;

// TNovusLogFile

constructor TNovusLogFile.Create;
begin
  inherited Create(ltFile);

  fboutputConsole := true;

  fsFilename := AFilename;
  fbIsFileOpen := false;

  fsPathName := '';

  fcSeparator := '-';
end;

destructor TNovusLogFile.Destroy;
begin
  inherited;
end;

function TNovusLogFile.OpenLog(aOveride: Boolean = false): Boolean;
begin
  inherited;

  Result := false;

  If fbIsFileOpen then
    Exit;

  if Not DirectoryExists(TNovusStringUtils.JustPathname(Filename)) then
    Exit;

  if FileExists(Filename) then
  begin
    if Not aOveride then
      begin
        if not TNovusFileUtils.IsFileInUse(Filename) then
           TFile.Delete(Filename);
      end;
  end;

  fbIsFileOpen := true;

  Result := fbIsFileOpen;

  //WriteLog('Logging started');
end;

function TNovusLogFile.WriteLine(ATimeStr, ALogDesc: string;
  ALogDateTime: tDateTime): String;
Var
  lsLine: String;
  FStreamWriter: tStreamWriter;
  FTask: ITask;
  liRetryCount: Integer;
begin
  liRetryCount := 0;

  If Separator = #0 then
    lsLine := ATimeStr + ALogDesc
  else
    lsLine := ATimeStr + Separator + ALogDesc;

  while liRetryCount < RetryCount do
  begin
    try
      FStreamWriter := TStreamWriter.Create(Filename,true);
      FStreamWriter.AutoFlush := true;

      FStreamWriter.WriteLine(lsLine);

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

  Result := lsLine;
end;

procedure TNovusLogFile.CloseLog;
begin
  fbIsFileOpen := false;
end;

function TNovusLogFile.WriteExceptLog: String;
begin
  Result := inherited WriteExceptLog;

  WriteLine(FormatDateTime(fsDateTimeMask, Now), Result, Now);
end;

function TNovusLogFile.WriteLog(AMsg: string;
  AEventType: TEventType = etNone): string;
begin
  if AMsg = '' then
    Exit;

  if OutputConsole then
    Writeln(AMsg);

  inherited WriteLog(AMsg, AEventType);

  Result := WriteLine(FormatedNow, AMsg, Now);
end;


end.
