unit ServiceMain;

interface

uses NovusSvcMgr, System.SysUtils, NovusLogger, NovusLogger.Provider.Files,
    NovusLogger.Provider.Console;

type
  TMyService = class(TNovusSvcMgr)
  protected
    Log: tNovusLogger;
    procedure ServiceMain; override;
  private
  public
    procedure Run; override;
  end;

implementation

procedure TMyService.Run;
begin
  var Param := ParamStr(1);

  // Custom code for handling installation parameter
  if Param = '/install' then
  begin
    InstallService(ParamStr(0));
    Exit;
  end
  // Custom code for handling uninstallation parameter
  else if Param = '/uninstall' then
  begin
    UninstallService;
    Exit;
  end;
  // Custom code for checking if service is installed
  inherited;
end;


procedure TMyService.ServiceMain;
begin
  if Assigned(Log) then Log.AddLogInformation('ServiceMain');

  try
    repeat
      if not Paused then
      begin
        Sleep(5000); // Sleep for 5 seconds
      end;
    until Stopped;
  finally
  end;
end;




end.
