unit NovusShell;

interface

Uses NovusUtilities, ShellAPI, Windows, Forms, AnsiStrings, Classes  ;

type
  TReadPipeThread = class(TThread)
  protected
    FPipe: THandle;
    FContent: TStringStream;
    function GetContent: String;
    procedure Execute; override;
  public
    constructor Create(const aPipe: THandle);
    destructor Destroy; override;
    property aContent: String read GetContent;
  end;

  TWritePipeThread = class(TThread)
  protected
    FPipe: THandle;
    FContent: TStringStream;
    procedure Execute; override;
  public
    constructor Create(const aPipe: THandle; const aContent: String);
    destructor Destroy; override;
  end;

  tNovusShell = class(tNovusUtilities)
  protected
    function WindowsRedirectedExecute(aCommandline: String;
        const aInput: String;
        var aOutput, aError: String;
        const Wait: DWORD = 3600000): boolean;

    function WindowsShellExecute(const aOperation: String;
                        const aCommandLine: string;
                        const aDirectory: string;
                        const aParameters: String;
                        const aShow: Integer): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function RunRedirectCommand(const aCommandLine: string;
                                const aInput: string;
                                var aOutput,
                                aError: string): boolean;

    function RunCommandSilent(const aCommandLine: string;
                        const aDirectory: string;
                        const aParameters: String ): Boolean;

    function RunCommand(const aCommandLine: string;
                        const aDirectory: string;
                        const aParameters: String): Boolean;


  end;


implementation

constructor tNovusShell.Create;
begin
  inherited Create;
end;

destructor tNovusShell.Destroy;
begin
  inherited;
end;

function tNovusShell.RunRedirectCommand(const aCommandLine: string;
                                const aInput: string;
                                var aOutput,
                                aError: string): boolean;
begin
  Result :=  WindowsRedirectedExecute(aCommandLine,  aInput,
                                aOutput, aError);

end;


function tNovusShell.RunCommandSilent(const aCommandLine: string;
                        const aDirectory: string;
                        const aParameters: String): Boolean;
begin
  Result := WindowsShellExecute('open',
                       aCommandLine,
                       aDirectory,
                       aParameters,
                       SW_HIDE);


end;

function tNovusShell.RunCommand(const aCommandLine: String;
                        const aDirectory: string;
                        const aParameters: String): Boolean;
begin
  Result := WindowsShellExecute('open',
                       aCommandLine,
                       aDirectory,
                       aParameters,
                       SW_SHOWNORMAL);


end;


function tNovusShell.WindowsRedirectedExecute(aCommandline: String;
        const aInput: String;
        var aOutput, aError: String;
        const Wait: DWORD = 3600000): Boolean;

var
  FSecurityAttributes: SECURITY_ATTRIBUTES;
  FStartupInfo: STARTUPINFO;
  FProcessInfo: PROCESS_INFORMATION;
  hPipeInputRead, hPipeInputWrite: THandle;
  hPipeOutputRead, hPipeOutputWrite: THandle;
  hPipeErrorRead, hPipeErrorWrite: THandle;
  FWriteInputThread: TWritePipeThread;
  FReadOutputThread: TReadPipeThread;
  FReadErrorThread: TReadPipeThread;
  Fok: Integer;
  liExitcode: Integer;
begin
  FillChar(FSecurityAttributes, SizeOf(SECURITY_ATTRIBUTES), Chr(0));

  FSecurityAttributes.nLength := SizeOf(SECURITY_ATTRIBUTES);
  FSecurityAttributes.bInheritHandle := TRUE;

  hPipeInputRead := 0;
  hPipeInputWrite := 0;
  if (aInput <> '') then
    CreatePipe(hPipeInputRead, hPipeInputWrite, @FSecurityAttributes, 0);

  CreatePipe(hPipeOutputRead, hPipeOutputWrite, @FSecurityAttributes, 0);
  CreatePipe(hPipeErrorRead, hPipeErrorWrite, @FSecurityAttributes, 0);

  FillChar(FStartupInfo, SizeOf(STARTUPINFO), Chr(0));

  FStartupInfo.cb := Sizeof(STARTUPINFO);

  FStartupInfo.dwFlags := STARTF_USESHOWWINDOW;

  FStartupInfo.wShowWindow := SW_HIDE;

  FStartupInfo.dwFlags := FStartupInfo.dwFlags or STARTF_USESTDHANDLES;
  FStartupInfo.hStdInput := hPipeInputRead;
  FStartupInfo.hStdOutput := hPipeOutputWrite;
  FStartupInfo.hStdError := hPipeErrorWrite;

  UniqueString(aCommandline);

  Result := CreateProcess(nil, PChar(aCommandline), nil, nil, True,
    CREATE_NEW_CONSOLE, nil, nil, FStartupInfo, FProcessInfo);

  CloseHandle(hPipeInputRead);
  CloseHandle(hPipeOutputWrite);
  CloseHandle(hPipeErrorWrite);

  if Result then
  begin
    FWriteInputThread := nil;
    if (hPipeInputWrite <> 0) then
      FWriteInputThread := TWritePipeThread.Create(hPipeInputWrite, aInput);
    FReadOutputThread := TReadPipeThread.Create(hPipeOutputRead);
    FReadErrorThread := TReadPipeThread.Create(hPipeErrorRead);
    try
    Fok := WaitForSingleObject(FProcessInfo.hProcess, Wait);

    if (Fok = WAIT_TIMEOUT) then
    begin
      Result := False;
      TerminateProcess(FProcessInfo.hProcess, UINT(ERROR_CANCELLED));
    end;

    liExitcode :=0;

    result := GetExitCodeProcess(FProcessInfo.hProcess, DWORD(liExitcode));


    FReadOutputThread.WaitFor;
    aOutput := FReadOutputThread.aContent;
    FReadErrorThread.WaitFor;
    aError := FReadErrorThread.aContent;
    finally
      FWriteInputThread.Free;
      FReadOutputThread.Free;
      FReadErrorThread.Free;
      CloseHandle(FProcessInfo.hThread);
      CloseHandle(FProcessInfo.hProcess);
    end;
  end;
  // close our ends of the pipes
  CloseHandle(hPipeOutputRead);
  CloseHandle(hPipeErrorRead);

end;


function tNovusShell.WindowsShellExecute(const aOperation: String;
                        const aCommandLine: string;
                        const aDirectory: string;
                        const aParameters: String;
                        const aShow: Integer): Boolean;
var
  ShellInfo: TShellExecuteInfo;
  liExitcode: Integer;
  fbOK: LongBool;
  Msg: tagMSG;
begin
  FillChar(ShellInfo, SizeOf(ShellInfo), Chr(0));

  ShellInfo.cbSize := SizeOf(ShellInfo);
  ShellInfo.fMask := SEE_MASK_DOENVSUBST  or SEE_MASK_FLAG_NO_UI  or SEE_MASK_NOCLOSEPROCESS or
    SEE_MASK_FLAG_DDEWAIT;
  ShellInfo.lpFile := PChar(aCommandLine);
  ShellInfo.lpParameters := Pointer(aParameters);
  ShellInfo.lpVerb := Pointer(aOperation);
  ShellInfo.nShow := aShow;

  Result := ShellExecuteEx(@ShellInfo);
  if Result then
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

      liExitcode :=0;
        
      result := GetExitCodeProcess(ShellInfo.hProcess, DWORD(liExitcode));
        
      CloseHandle(ShellInfo.hProcess);
    end;

end;

{ TReadPipeThread }

constructor TReadPipeThread.Create(const aPipe: THandle);
begin
  FPipe := aPipe;
  FContent := TStringStream.Create('');
  inherited Create(False);
end;

destructor TReadPipeThread.Destroy;
begin
  FContent.Free;
  inherited Destroy;
end;

procedure TReadPipeThread.Execute;
const
  BLOCK_SIZE = 4096;
var
  iBytesRead: DWORD;
  myBuffer: array[0..BLOCK_SIZE-1] of Byte;
begin
  repeat
    if ReadFile(FPipe, myBuffer, BLOCK_SIZE, iBytesRead, nil) then
      FContent.Write(myBuffer, iBytesRead);
  until (iBytesRead = 0);
end;

function TReadPipeThread.GetContent: String;
begin
  Result := FContent.DataString;
end;

{ TWritePipeThread }

constructor TWritePipeThread.Create(const aPipe: THandle;
  const aContent: String);
begin
  FPipe := aPipe;
  FContent := TStringStream.Create(aContent);
  inherited Create(False); // start running
end;

destructor TWritePipeThread.Destroy;
begin
  FContent.Free;
  if (FPipe <> 0) then
    CloseHandle(FPipe);
  inherited Destroy;
end;

procedure TWritePipeThread.Execute;
const
  BLOCK_SIZE = 4096;
var
  myBuffer: array[0..BLOCK_SIZE-1] of Byte;
  iBytesToWrite: DWORD;
  iBytesWritten: DWORD;
begin
  iBytesToWrite := FContent.Read(myBuffer, BLOCK_SIZE);
  while (iBytesToWrite > 0) do
  begin
    WriteFile(FPipe, myBuffer, iBytesToWrite, iBytesWritten, nil);
    iBytesToWrite := FContent.Read(myBuffer, BLOCK_SIZE);
  end;
  // close our handle to let the other process know, that
  // there won't be any more data.
  CloseHandle(FPipe);
  FPipe := 0;
end;


end.
