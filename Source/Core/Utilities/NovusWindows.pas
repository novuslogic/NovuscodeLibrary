{********************************************************************}
{                                                                    }
{           DelphiVersions.inc                                       }
{                                                                    }
{           Apache License                                           }
{           Version 2.0, January 2004                                }
{           License at http://www.apache.org/licenses/               }
{                                                                    }
{                                                                    }
{           Copyright (c) 2017 Novuslogic Software                   }
{           http://www.novuslogic.com                                }
{                                                                    }
{********************************************************************}

{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusWindows;

interface


uses Windows, sysutils, Classes, NovusUtilities, Registry, Messages;

Type
  TNovusWindows = class(TNovusUtilities)
  protected
  public
    /// <summary>
    ///   Is Win64 running
    /// </summary>
    class function IsWin64: Boolean;
    /// <summary>
    ///   Returns Windows path \Program Files\Common Files
    /// </summary>
    class function CommonFilesDir: string;
    /// <summary>
    ///   Returns Windows System path directory
    /// </summary>
    class function WindowsSystemDir: String;
    /// <summary>
    ///   Returns Windows installed path directory
    /// </summary>
    class function WindowsDir: string;
    /// <summary>
    ///   Returns the path of the directory for windows temporary files
    /// </summary>
    class function WindowsTempPath: String;
    /// <summary>
    ///   Return current windows exception
    /// </summary>
    class function WindowsExceptMess: String;
    /// <summary>
    ///   Returns NetBIOS name of the local computer
    /// </summary>
    /// <remarks>
    ///   Using RPC will only return local comupter name not the RPC Windows computer name.
    ///
    ///   https://msdn.microsoft.com/en-us/library/windows/desktop/ms724295(v=vs.85).aspx
    /// </remarks>
    class function GetLocalComputerName: String;
    /// <summary>
    ///   Set local Windows Environment Variable
    /// </summary>
    class function SetEnvironmentVariableEx(const aVariableName: String; const aValue: string; aIsSystemVariable: Boolean): Integer;
    /// <summary>
    ///   Set system Windows Environment Variable
    /// </summary>
    /// <param name="aVariableName">
    ///   Variable name
    /// </param>
    /// <param name="aValue">
    ///   Value of variable
    /// </param>
    class function SetSysEnvironmentVariable(const aVariableName: String; aValue: string): boolean;
  end;

  function CreateEnvironmentBlock(var lpEnvironment: Pointer; hToken: THandle; bInherit: BOOL): BOOL; stdcall; external 'userenv';
  function DestroyEnvironmentBlock(pEnvironment: Pointer): BOOL; stdcall; external 'userenv';

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

{$IFDEF DELPHI2009_UP}
   S := StrPas(PWideChar(P));
{$ELSE}
   S := StrPas(P);
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
    {$IFDEF DELPHI2009_UP}
    Result := StrPas(PWideChar(P));
    {$ELSE}
    Result := StrPas(P);
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
end;

class function TNovusWindows.SetEnvironmentVariableEx(const aVariableName: String; const aValue: string; aIsSystemVariable: Boolean): Integer;
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


