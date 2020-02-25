{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusShell;

interface

Uses NovusUtilities, ShellAPI, Windows, Forms, AnsiStrings, Classes, comobj,
  variants,
  SysUtils, NovusWindows;

const
  SECURITY_WORLD_SID_AUTHORITY: TSidIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 1));
  HEAP_ZERO_MEMORY = $00000008;
  ACL_REVISION = 2;
  SECURITY_WORLD_RID = $00000000;
  FILE_READ_DATA = $0001;
  FILE_LIST_DIRECTORY = $0001;
  FILE_WRITE_DATA = $0002;
  FILE_ADD_FILE = $0002;
  FILE_APPEND_DATA = $0004;
  FILE_ADD_SUBDIRECTORY = $0004;
  FILE_CREATE_PIPE_INSTANCE = $0004;
  FILE_READ_EA = $0008;
  FILE_WRITE_EA = $0010;
  FILE_EXECUTE = $0020;
  FILE_TRAVERSE = $0020;
  FILE_DELETE_CHILD = $0040;
  FILE_READ_ATTRIBUTES = $0080;
  FILE_WRITE_ATTRIBUTES = $0100;
  FILE_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or Windows.SYNCHRONIZE or $1FF);
  FILE_GENERIC_READ = (STANDARD_RIGHTS_READ or FILE_READ_DATA or
    FILE_READ_ATTRIBUTES or FILE_READ_EA or Windows.SYNCHRONIZE);
  FILE_GENERIC_WRITE = (STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or
    FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA or
    Windows.SYNCHRONIZE);
  FILE_GENERIC_EXECUTE = (STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or
    FILE_EXECUTE or Windows.SYNCHRONIZE);

type
  TEventRunCaptureCommandOutput = procedure(var aOutput: string) of Object;


  TAceHeader = packed record
    AceType: Byte;
    AceFlags: Byte;
    AceSize: Word;
  end;

  TAccessAllowedAce = packed record
    Header: TAceHeader;
    Mask: ACCESS_MASK;
    SidStart: DWORD;
  end;

  tNovusShell = class(tNovusUtilities)
  protected
    function WindowsCaptureExecute(aCommandline: String;
      var aOutput: String): Integer;

    function WindowsShellExecute(const aOperation: String;
      const aCommandline: string; const aDirectory: string;
      const aParameters: String; const aShow: Integer): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    /// <summary>
    /// Run an console application and capture output
    /// </summary>
    function RunCommandCapture(const aCommandline: string;
      var aOutput: string): Integer;

    function RunCommandSilent(const aCommandline: string;
      const aDirectory: string; const aParameters: String): Integer;

    function RunCommand(const aCommandline: string; const aDirectory: string;
      const aParameters: String): Integer;

  end;

function WTSQueryUserToken(SessionId: DWORD; phToken: THandle): bool; stdcall;
  external 'wtsapi32.dll';

implementation

constructor tNovusShell.Create;
begin
  inherited Create;
end;

destructor tNovusShell.Destroy;
begin
  inherited;
end;

function tNovusShell.RunCommandCapture(const aCommandline: string;
  var aOutput: String): Integer;
begin
  result := WindowsCaptureExecute(aCommandline, aOutput);

end;

function tNovusShell.RunCommandSilent(const aCommandline: string;
  const aDirectory: string; const aParameters: String): Integer;
begin
  result := WindowsShellExecute('open', aCommandline, aDirectory,
    aParameters, SW_HIDE);

end;

function tNovusShell.RunCommand(const aCommandline: String;
  const aDirectory: string; const aParameters: String): Integer;
begin
  result := WindowsShellExecute('open', aCommandline, aDirectory, aParameters,
    SW_SHOWNORMAL);

end;

function tNovusShell.WindowsCaptureExecute(aCommandline: String;
  var aOutput: String): Integer;

const
  CReadBuffer = 124000;
var
  FSecurityAttributes: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  FStartupInfo: TStartupInfo;
  FProcessInformation: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
  liExitCode: Integer;
  pSD: PSecurityDescriptor;
  psidEveryone: PSID;
  sidAuth: TSidIdentifierAuthority;
  lSDSize, lACLSize: Cardinal;
  lpACL: PACL;
  ConnSessID: Cardinal;
  Token: THandle;
  hProcess: THandle;
 // pEnv: Pointer;
begin
  liExitCode := -1;

  FSecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  FSecurityAttributes.bInheritHandle := true;
  FSecurityAttributes.lpSecurityDescriptor := nil;

  sidAuth := SECURITY_WORLD_SID_AUTHORITY;
  if not AllocateAndInitializeSid(sidAuth, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0,
    0, 0, psidEveryone) then
    Exit;

  lSDSize := SizeOf(TSecurityDescriptor);
  lACLSize := GetLengthSID(psidEveryone) + SizeOf(TAccessAllowedAce) +
    SizeOf(TACL);
  pSD := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, lSDSize + lACLSize);
  if pSD = nil then
    Exit;

  if not InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;

  lpACL := PACL(PChar(pSD) + lSDSize);
  if not InitializeAcl(lpACL^, lACLSize, ACL_REVISION) then
    Exit;

  if not AddAccessAllowedAce(lpACL^, ACL_REVISION, FILE_ALL_ACCESS, psidEveryone)
  then
    Exit;

  if not SetSecurityDescriptorDacl(pSD, true, lpACL, False) then
    Exit;

  FSecurityAttributes.lpSecurityDescriptor := pSD;

  if CreatePipe(hRead, hWrite, @FSecurityAttributes, 0) then
    try
      FillChar(FStartupInfo, SizeOf(TStartupInfo), #0);
      FStartupInfo.cb := SizeOf(TStartupInfo);
      FStartupInfo.hStdInput := hRead;
      FStartupInfo.hStdOutput := hWrite;
      FStartupInfo.hStdError := hWrite;
      FStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      FStartupInfo.wShowWindow := SW_HIDE;
      FStartupInfo.lpDesktop := NIL;

      if CreateProcess(nil, PChar(aCommandline), nil, nil, true,
        CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS, nil, nil,
        FStartupInfo, FProcessInformation) then
        try
          repeat
            dRunning := WaitForSingleObject(FProcessInformation.hProcess,
              1000 (* INFINITE *) );

            PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
            if (dAvailable > 0) then
              repeat
                dRead := 0;
                ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                pBuffer[dRead] := #0;
                OemToCharA(pBuffer, dBuffer);

                aOutput := aOutput + dBuffer;

              until (dRead < CReadBuffer);

            Application.ProcessMessages;
          until (dRunning <> WAIT_TIMEOUT);

          if not GetExitCodeProcess(FProcessInformation.hProcess,
            DWORD(liExitCode)) then
            liExitCode := -1;
        finally
          CloseHandle(FProcessInformation.hProcess);
          CloseHandle(FProcessInformation.hThread);
        end;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;

  result := liExitCode;
end;

function tNovusShell.WindowsShellExecute(const aOperation: String;
  const aCommandline: string; const aDirectory: string;
  const aParameters: String; const aShow: Integer): Integer;
var
  ShellInfo: TShellExecuteInfo;
  liExitCode: Integer;
  fbOK: LongBool;
  Msg: tagMSG;
  fok: Boolean;
begin
  //result := 0;
  liExitCode := 0;

  FillChar(ShellInfo, SizeOf(ShellInfo), Chr(0));

  ShellInfo.cbSize := SizeOf(ShellInfo);
  ShellInfo.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI or
    SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
  ShellInfo.lpFile := PChar(aCommandline);
  ShellInfo.lpParameters := Pointer(aParameters);
  ShellInfo.lpVerb := Pointer(aOperation);
  ShellInfo.nShow := aShow;

  fok := ShellExecuteEx(@ShellInfo);
  if fok then
  begin
    WaitForInputIdle(ShellInfo.hProcess, INFINITE);
    while WaitForSingleObject(ShellInfo.hProcess, 10) = WAIT_TIMEOUT do
      repeat
        Msg.hwnd := 0;
        fbOK := PeekMessage(Msg, ShellInfo.Wnd, 0, 0, PM_REMOVE);
        if fbOK then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      until not fbOK;

    If NOt GetExitCodeProcess(ShellInfo.hProcess, DWORD(liExitCode)) then
      liExitCode := 0;

    CloseHandle(ShellInfo.hProcess);
  end
  else
    liExitCode := -1;

  result := liExitCode;
end;

end.
