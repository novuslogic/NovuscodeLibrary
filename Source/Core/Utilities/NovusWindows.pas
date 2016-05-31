unit NovusWindows;

interface


uses Windows, sysutils, Classes, NovusUtilities, Registry, Messages;

Type
  TNovusWindows = class(TNovusUtilities)
  protected
  public
    class function IsWin64: Boolean;
    class function CommonFilesDir: string;
    class function WindowsSystemDir: String;
    class function WindowsDir: string;
    class function WindowsTempPath: String;
    class function WindowsExceptMess: String;
    class function GetLocalComputerName: String;
    class function SetEnvironmentVariableEx(const aVariableName: String; const aValue: string; aIsSystemVariable: Boolean): Integer;
    class function SetSysEnvironmentVariable(const aVariableName: String; aValue: string): boolean;
  end;

implementation





class function TNovusWindows.WindowsDir: string;
begin
  SetLength( result, 255 );
  Windows.GetWindowsDirectory( pChar(result), 255 );
  SetLength(result, StrLen(pChar(result)));
end;

class function TNovusWindows.WindowsSystemDir: String;
begin
  SetLength( result, 255 );
  Windows.GetSystemDirectory( pChar(result), 255 );
  SetLength( result, StrLen(pChar(result)) );
end;

class function TNovusWindows.CommonFilesDir: string;
begin
  with TRegistry.Create do try
    RootKey:= HKey_Local_Machine;
    if OpenKey( 'Software\Microsoft\Windows\CurrentVersion', True ) then begin
      if ValueExists( 'CommonFilesDir' ) then begin
        result:= ReadString( 'CommonFilesDir' );
      end else begin
        result:= Copy(WindowsDir{},1,2) + '\Program Files\Common Files';
        WriteString( 'CommonFilesDir', result );
      end;
    end;
  finally Free end;
end;

class function TNovusWindows.WindowsTempPath: String;
begin
  SetLength(Result,Max_path);
  SetLength(result,GetTempPath(Max_Path,Pchar(Result)));
end;

class function TNovusWindows.WindowsExceptMess;
Var
  ValSize: Integer;
  P: Pointer;
  S: String;
begin
  Result := '';

  If ExceptObject = NIL then Exit;

  ValSize := 255;

  P := AllocMem(ValSize);

  ExceptionErrorMessage(ExceptObject, ExceptAddr, P,  ValSize);

{$IFDEF VER180}
 S := StrPas(P);
{$ELSE}
 S := StrPas(PWideChar(P));
 {$ENDIF}

  FreeMem(P);

  S := Copy(S, (Pos('.', S) + 1), Length(S) - Pos('.', S));
  Result := Copy(S, (Pos('.', S) + 1), Length(S) - Pos('.', S));
end;


class function TNovusWindows.GetLocalComputerName;
var
 P: Pointer;
 Size : DWORD;
begin
  Result := '';

  Size := MAX_COMPUTERNAME_LENGTH + 1;

  P := AllocMem(Size);

  if GetComputerName(P, Size) then
    {$IFDEF VER180}
    Result := StrPas(P);
    {$ELSE}
    Result := StrPas(PWideChar(P));
    {$ENDIF}

  FreeMem(P);
end;

class function TNovusWindows.SetSysEnvironmentVariable(const aVariableName: String; aValue: string): Boolean;
var
  fok: Boolean;
  reg: TRegistry;
resourcestring
  key = 'SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment';

begin
  Try
    Result := False;


    reg := TRegistry.Create;

    reg.Access := KEY_ALL_ACCESS or KEY_WOW64_64KEY;

    reg.RootKey := HKEY_LOCAL_MACHINE;

    fok := reg.KeyExists(key);
    if fok then
      begin
        if reg.OpenKey(key, true) then
          begin
            Result := True;

            reg.WriteString(aVariableName, aValue);

            //SetEnvironmentVariable(PChar(aVariableName), PChar(aValue));

            SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
          end;
      end;

  Finally

    reg.free;
  End;

  (*
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      fok  := OpenKey('SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment', true);

      if fok then
      begin
        WriteString(aVariableName, aValue);

        SetEnvironmentVariable(PChar(aVariableName), PChar(aValue));

        SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
      end
    else
      Result := GetLastError;

    finally
      Free;
    end;
    *)
end;

class function TNovusWindows.SetEnvironmentVariableEx(const aVariableName: String; const aValue: string; aIsSystemVariable: Boolean): Integer;
var
  rv: DWORD;
begin
  if aIsSystemVariable = false then
    begin
      if Windows.SetEnvironmentVariable(PChar(aVariableName),
        PChar(aValue)) then
        Result := 0
      else
        begin
          Result := GetLastError;

          if Result = 0 then Result := -1;
        end;

    end
  else
    begin
       Result := 0;

       if Not SetSysEnvironmentVariable(aVariableName, aValue) then
         begin
           Result := GetLastError;

           if Result = 0 then Result := -1;
         end;

    end;
end;

class function TNovusWindows.IsWin64: Boolean;
var
  IsWow64Process : function(hProcess : THandle; var Wow64Process : BOOL): BOOL; stdcall;
  Wow64Process : BOOL;
begin
  Result := False;
  IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');
  if Assigned(IsWow64Process) then begin
    if IsWow64Process(GetCurrentProcess, Wow64Process) then begin
      Result := Wow64Process;
    end;
  end;
end;


end.
