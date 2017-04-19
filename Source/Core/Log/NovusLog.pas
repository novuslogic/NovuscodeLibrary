unit NovusLog;

interface

Uses Classes, NovusStringUtils, NovusList, NovusUtilities, SysUtils,
  NovusBO, NovusWinEventLog;

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
    fiLastLogID: Integer;
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
  end;

  TNovusLogFile = Class(TNovusLog)
  private
  protected
    fboutputConsole: Boolean;
    fcSeparator: char;
    fiFileSize: Integer;
    fbIsFileOpen: Boolean;
    fsFilename: string;
    FFilePonter: text;
    fsPathName: String;
  public
    constructor Create(AFilename: String); virtual;
    destructor Destroy; override;

    function WriteLine(ATimeStr, ALogDesc: string;
      ALogDateTime: tDateTime): String;
    function ReadLine: String;

    function OpenLog(AOveride: Boolean = false): Boolean; virtual;
    procedure CloseLog; virtual;

    procedure ReadAll;

    function WriteLog(AMsg: string; AEventType: TEventType = etNone)
      : string; override;
    function WriteExceptLog: String; override;

    property Filename: String read fsFilename write fsFilename;

    property Separator: char read fcSeparator write fcSeparator;

    property IsFileOpen: Boolean read fbIsFileOpen write fbIsFileOpen;

    property FileSize: Integer read fiFileSize write fiFileSize;

    property FilePonter: text read FFilePonter write FFilePonter;

    property OutputConsole: Boolean read fboutputConsole write fboutputConsole;
  end;

implementation

constructor TNovusLog.Create(ALogType: TLogType);
begin
  inherited Create;

  DateTimeMask := FormatSettings.ShortDateFormat + ' hh:mm';

  foLogDetailsList := TNovusList.Create(TNovusLogDetails);

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

constructor TNovusLogFile.Create;
begin
  inherited Create(ltFile);

  fboutputConsole := true;

  fsFilename := AFilename;
  fbIsFileOpen := false;

  fsPathName := '';

  fiFileSize := 0;

  fcSeparator := '-';
end;

destructor TNovusLogFile.Destroy;
begin
  inherited;
end;

function TNovusLogFile.OpenLog(AOveride: Boolean = false): Boolean;
begin
  inherited;

  Result := false;

  If fbIsFileOpen then
    Exit;

  if Not DirectoryExists(TNovusUtilities.JustPathname(Filename)) then
    Exit;

  if FileExists(Filename) then
  begin
    If (FileSize > 0) and (TNovusUtilities.FindFileSize(Filename) > FileSize)
    then
      RenameFile(Filename, TNovusUtilities.JustPathname(Filename) +
        TNovusUtilities.JustFilename(Filename) + '.bak');
  end;

  AssignFile(FFilePonter, Filename);
  if AOveride = false then
  begin
    if FileExists(Filename) then
      Append(FFilePonter)
    else
      Rewrite(FFilePonter);
  end
  else
  begin
    Rewrite(FFilePonter);

  end;

  fbIsFileOpen := true;

  Result := fbIsFileOpen;

  WriteLog('Logging started');
end;

function TNovusLogFile.WriteLine(ATimeStr, ALogDesc: string;
  ALogDateTime: tDateTime): String;
Var
  lsLine: String;
begin
  If Separator = #0 then
    lsLine := ATimeStr + ALogDesc
  else
    lsLine := ATimeStr + Separator + ALogDesc;

  Writeln(FFilePonter, lsLine);

  Result := lsLine;
end;

function TNovusLogFile.ReadLine: string;
begin
  Readln(FFilePonter, Result)
end;

procedure TNovusLogFile.CloseLog;
begin
  Flush(FFilePonter);

  fbIsFileOpen := false;

  WriteLog('Logging finished');

  CloseFile(FFilePonter);
end;

function TNovusLogFile.WriteExceptLog: String;
begin
  Result := inherited WriteExceptLog;

  WriteLine(FormatDateTime(fsDateTimeMask, Now), Result, Now);
  // Flush(FFilePonter);
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
  // Flush(FFilePonter);
end;

procedure TNovusLogFile.ReadAll;
Var
  lsLine: String;
  liPos: Integer;
  ldLogDateTime: tDateTime;
  lsLogDesc: String;
begin
  If Not FileExists(Filename) then
    Exit;

  AssignFile(FFilePonter, Filename);

  Reset(FFilePonter);

  while not System.Eof(FilePonter) do
  begin
    lsLine := ReadLine;

    If Trim(lsLine) <> '' then
    begin
      liPos := 1;
      ldLogDateTime := TNovusStringUtils.Str2DateTime
        (TNovusStringUtils.GetStrTokenA(lsLine, '-', liPos));
      lsLogDesc := TNovusStringUtils.GetStrTokenA(lsLine, '-', liPos);

      AddLogDetails(lsLogDesc, ldLogDateTime);
    end;

  end;

  CloseFile(FFilePonter)
end;

end.
