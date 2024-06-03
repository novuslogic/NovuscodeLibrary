unit NovusWinEventLog;

interface

Uses Windows, Registry, SysUtils, NovusObject, VCL.Forms;

type
  TEventType = (etError, etWarning, etInformation, etAuditSuccess,
    etAuditFailure, etNone);

  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-even/1ed850f9-a1fe-4567-a371-02683c6ed3cb

  TNovusWinEventLog = class(tNovusObject)
  private
  protected
    FsApplicationName: String;
    FbIncludeUserName: Boolean;
    FEventType: TEventType;
    FwEventID: DWord;
    FwEventCategory: Word;
    FsUserName: String;

    procedure SetApplicationName(Value: String);
    procedure SetIncludeUserName(Value: Boolean);
    procedure SetEventID(Value: DWord);
    procedure SetEventCategory(Value: Word);

  public
    constructor Create;
    destructor Destroy; override;

    procedure LogEvent(const aLogMessage: String);

    property ApplicationName: String read FsApplicationName
      write SetApplicationName;

    property IncludeUserName: Boolean read FbIncludeUserName
      write SetIncludeUserName;

    property EventType: TEventType read FEventType write FEventType
      default etInformation;

    property EventID: DWord read FwEventID write SetEventID;

    property EventCategory: Word read FwEventCategory write SetEventCategory;
  end;

implementation

const
  RegPath = '\SYSTEM\CurrentControlSet\Services\EventLog\Application';

procedure TNovusWinEventLog.SetApplicationName(Value: String);
begin
  FsApplicationName := Value;
end;

procedure TNovusWinEventLog.SetIncludeUserName(Value: Boolean);
begin
  FbIncludeUserName := Value;
end;

procedure TNovusWinEventLog.SetEventID(Value: DWord);
begin
  FwEventID := Abs(Value);
end;

procedure TNovusWinEventLog.SetEventCategory(Value: Word);
begin
  FwEventCategory := Abs(Value);
end;

procedure TNovusWinEventLog.LogEvent(const aLogMessage: String);
var
  LogHandle: THandle;
  OK: Boolean;
  eType: Word;
  eMsg, aName: PChar;
  Reg: TRegistry;
  nSize: DWord;
  VersionInfo: TOSVersionInfo;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if Windows.GetVersionEx(VersionInfo) then
    if VersionInfo.dwPlatformId < VER_PLATFORM_WIN32_NT then
      Exit;

   //LogHandle := OpenEventLog(NIL, PChar(ApplicationName));

  LogHandle := RegisterEventSource(NIL, PChar(ApplicationName));


  if LogHandle <> 0 then
  begin
    eType := 0;
    case EventType of
      etError:
        eType := 1;
      etWarning:
        eType := 2;
      etInformation:
        eType := 4;
      etAuditSuccess:
        eType := 8;
      etAuditFailure:
        eType := 16;
    end;

    FsUserName := #13#10;
    If IncludeUserName then
    begin
      nSize := 20; // Max UserName
      aName := stralloc(nSize + 1);
      OK := GetUserName(aName, nSize);
      if not OK then
        strcopy(aName, 'N/A');
      FsUserName := FsUserName + 'User: ' + aName + #13#10;
      strDispose(aName);
    end;

    If IncludeUserName then
      eMsg := PChar(FsUserName + aLogMessage)
    else
      eMsg := PChar(aLogMessage);

    ReportEvent(LogHandle, eType, EventCategory, EventID, NIL, 1, 0,
      @eMsg, NIL);

    DeregisterEventSource(LogHandle);
   // CloseEventLog(LogHandle);
  end;
end;

constructor TNovusWinEventLog.Create;
begin
  inherited Create;

  ApplicationName := 'TNovusWinEventLog';
  If Assigned(Application) then
    ApplicationName := Application.Title;

  EventCategory := 0;
  EventID := 0;
  EventType := etInformation;
  IncludeUserName := True;
end;

destructor TNovusWinEventLog.Destroy;
begin
  inherited Destroy;
end;

end.
