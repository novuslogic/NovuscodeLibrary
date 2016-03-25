unit NovusWindows;

interface


uses Windows, sysutils, Classes, NovusUtilities, Registry;

Type
  TNovusWindows = class(TNovusUtilities)
  protected
  public
    class function CommonFilesDir: string;
    class function WindowsSystemDir: String;
    class function WindowsDir: string;
    class function WindowsTempPath: String;
    class function WindowsExceptMess: String;
    class function GetLocalComputerName: String;
//    class function ExecuteApp(FileName:String; Visibility : integer): Integer;
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

(*

Moved to TNovusShell rename to TNovusWindows.WindowsRedirectedExecute

class function TNovusWindows.ExecuteApp(FileName:String; Visibility : integer): Integer;
var
  zAppName:array[0..512] of char;
  zCurDir:array[0..255] of char;
  WorkDir:String;
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
  MyResult : Cardinal;
begin
  Result := -1;

  StrPCopy(zAppName,FileName);
  GetDir(0,WorkDir);
  StrPCopy(zCurDir,WorkDir);
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
    zAppName,                      { pointer to command line string }
    nil,                           { pointer to process security attributes }
    nil,                           { pointer to thread security attributes }
    false,                         { handle inheritance flag }
    CREATE_NEW_CONSOLE or          { creation flags }
    NORMAL_PRIORITY_CLASS,
    nil,                           { pointer to new environment block }
    nil,                           { pointer to current directory name }
    StartupInfo,                   { pointer to STARTUPINFO }
    ProcessInfo) then Result := -1  { pointer to PROCESS_INF }

  else begin
    WaitforSingleObject(ProcessInfo.hProcess,INFINITE);

    GetExitCodeProcess(ProcessInfo.hProcess, MyResult);

    Result := MyResult;
  end;
end;
*)


end.
