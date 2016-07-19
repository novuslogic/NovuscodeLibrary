unit NovusWinEventLog;

interface

Uses Windows, Registry, SysUtils, NovusInfrastructre, VCL.Forms;

type
  TEventType = (etError,etWarning,etInformation,etAuditSuccess,etAuditFailure, etNone);

  TNovusWinEventLog = class(tNovusInfrastructre)
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

    procedure LogEvent(const Line: String);

    property ApplicationName: String
      read FsApplicationName
      write SetApplicationName;

    property IncludeUserName: Boolean
      read FbIncludeUserName
      write SetIncludeUserName;

    property EventType: TEventType
      read FEventType
      write FEventType default etInformation;

    property EventID: DWord
      read FwEventID
      write SetEventID;

    property EventCategory: Word
      read FwEventCategory
      write SetEventCategory;
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

procedure TNovusWinEventLog.SetEventId(Value: DWord);
begin
  FwEventID := Abs(Value);
end;

procedure TNovusWinEventLog.SetEventCategory(Value: Word);
begin
  FwEventCategory := Abs(Value);
end;

procedure TNovusWinEventLog.LogEvent(const Line: String);
var
  LogHandle:	THandle;
  OK: Boolean;
  eType: Word;
  eMsg, aName: PChar;
  Reg: TRegistry;
  nSize: Dword;
  VersionInfo : TOSVersionInfo;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf( TOSVersionInfo );
  if Windows.GetVersionEx( VersionInfo ) then
  if VersionInfo.dwPlatformId < VER_PLATFORM_WIN32_NT then Exit;

  LogHandle:= OpenEventLog( NIL, PChar(ApplicationName) );
  if Loghandle<>0 then
    begin
       eType := 0;
       case EventType of
         etError:        eType := 1;
         etWarning:      eType := 2;
         etInformation:  eType := 4;
         etAuditSuccess: eType := 8;
         etAuditFailure: eType := 16;
       end;

       FsUsername := #13#10;
       If IncludeUserName then
         begin
           nSize := 20 ; // Max UserName
           aName := stralloc ( nSize+1 );
           OK := GetUserName( aName, nSize );
 	   if not OK then strcopy( aName, 'N/A' );
	   FsUserName := FsUserName + 'User: ' + aName + #13#10;
 	   strDispose( aName );
	 end;

    If IncludeUserName then
       eMsg := Pchar(FsUserName + Line)
    else
       eMsg := Pchar(Line);

       ReportEvent(LogHandle, eType, EventCategory, EventID, NIL, 1, 0, @eMsg, NIL);
       CloseEventLog(LogHandle);
    end;
end;

constructor TNovusWinEventLog.create;
begin
  inherited create;

  ApplicationName := 'TNovusWinEventLog';
  If Assigned(Application) then
    ApplicationName := Application.Title;
  
  EventCategory       := 0;
  EventID             := 0;
  EventType           := etInformation;
  IncludeUserName     := True;
end;

destructor TNovusWinEventLog.destroy;
begin
  inherited destroy;
end;


end.
