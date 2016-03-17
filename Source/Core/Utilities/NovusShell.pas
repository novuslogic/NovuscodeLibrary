unit NovusShell;

interface

Uses NovusUtilities, ShellAPI, Windows;

type
  tNovusShell = class(tNovusUtilities)
  protected
    function WinExecute(const aOperation: String;
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
  Result := WinExecute('open',
                       aFilename,
                       aDirectory,
                       aParameters,
                       SW_SHOWNORMAL);


end;

function tNovusShell.WinExecute(const aOperation: String;
                        const aFilename: String;
                        const aDirectory: string;
                        const aParameters: String;
                        const aShow: Integer): Boolean;
var
  ShellInfo: TShellExecuteInfo;
begin
  FillChar(ShellInfo, SizeOf(ShellInfo), Chr(0));

  ShellInfo.cbSize := SizeOf(ShellInfo);
  ShellInfo.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI;
  ShellInfo.lpFile := PChar(aFileName);
  ShellInfo.lpParameters := Pointer(aParameters);
  ShellInfo.lpVerb := Pointer(aOperation);
  ShellInfo.nShow := aShow;

  Result := ShellExecuteEx(@ShellInfo);

  if Result then GetExitCodeProcess(ShellInfo.hProcess, DWORD(Exitcode));


end;



end.
