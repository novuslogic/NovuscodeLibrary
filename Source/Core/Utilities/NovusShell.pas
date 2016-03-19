unit NovusShell;

interface

Uses NovusUtilities, ShellAPI, Windows, Forms;

type
  tNovusShell = class(tNovusUtilities)
  protected
    function Execute(const aOperation: String;
                        const aFilename: String;
                        const aDirectory: string;
                        const aParameters: String;
                        const aShow: Integer): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function RunCommandSilent(const aFilename: String;
                        const aDirectory: string = '';
                        const aParameters: String = ''): Boolean;

    function RunCommand(const aFilename: String;
                        const aDirectory: string = '';
                        const aParameters: String = ''): Boolean;


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



function tNovusShell.RunCommandSilent(const aFilename: String;
                        const aDirectory: string;
                        const aParameters: String): Boolean;
begin
  Result := WinExecute('open',
                       aFilename,
                       aDirectory,
                       aParameters,
                       SW_HIDE);


end;

function tNovusShell.RunCommand(const aFilename: String;
                        const aDirectory: string;
                        const aParameters: String): Boolean;
begin
  Result := Execute('open',
                       aFilename,
                       aDirectory,
                       aParameters,
                       SW_SHOWNORMAL);


end;

function tNovusShell.Execute(const aOperation: String;
                        const aFilename: String;
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
  ShellInfo.lpFile := PChar(aFileName);
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

      result := GetExitCodeProcess(ShellInfo.hProcess, DWORD(liExitcode));
        
      CloseHandle(ShellInfo.hProcess);
    end;

end;



end.
