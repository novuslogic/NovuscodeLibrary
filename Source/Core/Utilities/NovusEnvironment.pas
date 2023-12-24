{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusEnvironment;

interface

Uses NovusUtilities, NovusParser, NovusTemplate2, SysUtils, NovusParser.Common,
  Winapi.Windows, System.Win.Registry, Winapi.Messages;

type
  tEnvironmentTokenType = (ETTToken1, ETTToken2);

  tNovusEnvironment = class(tNovusUtilities)
  protected
  private
  public
    /// <summary>
    /// Set it as an environment variable at the user level
    /// </summary>
    class function SetUserEnvVar(const aVariableName, aValue: string): boolean;
    /// <summary>
    /// Set it as a system-wide environment variable (requires admin privileges)
    /// </summary>
    class function SetSystemWideEnvVar(const aVariableName,
      aValue: string): boolean;
    /// <summary>
    /// Set it as an environment variable for the current process
    /// </summary>
    class function SetCurrentProcessEnvVar(const aVariableName,
      aValue: string): boolean;
    /// <summary>
    /// Is the current user running as Administrator
    /// </summary>
    class function IsAdministrator: boolean;

    /// <summary>
    /// Parse enciroment variables in string
    /// </summary>
    /// <param name="aInput">
    /// input string
    /// </param>
    /// <param name="aEnvironmentTokenType">
    /// ETTToken1, ETTToken2
    /// </param>
    /// <remarks>
    /// Defaults to ETTToken2
    /// </remarks>
    class function ParseGetEnvironmentVar(aInput: String;
      aEnvironmentTokenType: tEnvironmentTokenType = ETTToken2): String;
  end;

implementation

class function tNovusEnvironment.ParseGetEnvironmentVar(aInput: String;
  aEnvironmentTokenType: tEnvironmentTokenType): String;
Var
  loTemplate: tNovusTemplate2;
  I: Integer;
  FTemplateTag: TNovusTemplateTag;
begin
  result := aInput;

  if aInput = '' then
    Exit;

  Try
    loTemplate := tNovusTemplate2.Create;

    case aEnvironmentTokenType of
      ETTToken1:
        begin
          loTemplate.StartToken := '%';
          loTemplate.EndToken := '%';
          loTemplate.SecondToken := #0;
        end;
      ETTToken2:
        begin
          loTemplate.StartToken := '{';
          loTemplate.EndToken := '}';
          loTemplate.SecondToken := '%';
        end;
    end;

    // loTemplate.TemplateDoc.Text := Trim(aInput);
    loTemplate.LoadFromString(Trim(aInput));

    loTemplate.ParseTemplate;

    For I := 0 to loTemplate.TemplateTags.Count - 1 do
    begin
      FTemplateTag := TNovusTemplateTag(loTemplate.TemplateTags.items[I]);

      FTemplateTag.TagValue := GetEnvironmentVariable(FTemplateTag.TagName);
    end;

    loTemplate.InsertAllTagValues;

    result := Trim(loTemplate.OutputDoc.Text);

  Finally
    loTemplate.Free;
  End;
end;

class function tNovusEnvironment.IsAdministrator: boolean;
var
  TokenHandle: THandle;
  TokenInformation: TTokenElevation;
  ReturnLength: DWORD;
begin
  result := False;
  TokenHandle := 0;
  try
    // Open the access token associated with the current process
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    begin
      ReturnLength := 0;
      // Retrieve token elevation information
      if GetTokenInformation(TokenHandle, TokenElevation, @TokenInformation,
        SizeOf(TokenInformation), ReturnLength) then
      begin
        result := TokenInformation.TokenIsElevated <> 0;
      end;
    end;
  finally
    // Close the token handle
    if TokenHandle <> 0 then
      CloseHandle(TokenHandle);
  end;
end;

class function tNovusEnvironment.SetSystemWideEnvVar(const aVariableName,
  aValue: string): boolean;
var
  Reg: TRegistry;
begin
  result := False;

  Try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      // Use HKEY_CURRENT_USER\Environment for current user environment variables
      if Reg.OpenKey
        ('\System\CurrentControlSet\Control\Session Manager\Environment', True)
      then
      begin
        Reg.WriteString(aVariableName, aValue);
        Reg.CloseKey;

        result := True;
      end;
    finally
      Reg.Free;
    end;

    // Notify the system of the change
    SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
      LPARAM(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, 0);
  Except
    result := False;

    RaiseLastOSError;
  End;
end;

class function tNovusEnvironment.SetUserEnvVar(const aVariableName,
  aValue: string): boolean;
var
  Reg: TRegistry;
begin
  result := False;

  try
    Reg := TRegistry.Create(KEY_WRITE);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Environment', True) then
      begin
        Reg.WriteString(aVariableName, aValue);
        Reg.CloseKey;
        result := True;
      end
      else
        raise Exception.Create('Failed to open registry key');
    finally
      Reg.Free;
    end;

    // Notify the system of the environment variable change
    SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
      LPARAM(PChar('Environment')), SMTO_ABORTIFHUNG, 5000, 0);
  Except
    result := False;

    RaiseLastOSError;
  End;
end;

class function tNovusEnvironment.SetCurrentProcessEnvVar(const aVariableName,
  aValue: string): boolean;
begin
  // Set the environment variable for the current process
  if not SetEnvironmentVariable(PChar(aVariableName), PChar(aValue)) then
    RaiseLastOSError;
end;

end.
