{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusWindows;

interface

uses Windows, sysutils, Classes, NovusUtilities, Registry, Messages, TlHelp32;

Const
  IS_TEXT_UNICODE_ASCII16 = $1;
  IS_TEXT_UNICODE_REVERSE_ASCII16 = $10;
  IS_TEXT_UNICODE_STATISTICS = $2;
  IS_TEXT_UNICODE_REVERSE_STATISTICS = $20;
  IS_TEXT_UNICODE_CONTROLS = $4;
  IS_TEXT_UNICODE_REVERSE_CONTROLS = $40;
  IS_TEXT_UNICODE_SIGNATURE = $8;
  IS_TEXT_UNICODE_REVERSE_SIGNATURE = $80;
  IS_TEXT_UNICODE_ILLEGAL_CHARS = $100;
  IS_TEXT_UNICODE_ODD_LENGTH = $200;
  IS_TEXT_UNICODE_DBCS_LEADBYTE = $400;
  IS_TEXT_UNICODE_NULL_BYTES = $1000;
  IS_TEXT_UNICODE_UNICODE_MASK = $F;
  IS_TEXT_UNICODE_REVERSE_MASK = $F0;
  IS_TEXT_UNICODE_NOT_UNICODE_MASK = $F00;
  IS_TEXT_UNICODE_NOT_ASCII_MASK = $F000;

  WM_CLOSEWINDOW = WM_USER + 1;
  WM_UPDATE = WM_USER + 1;


Type
  TNovusWindows = class(TNovusUtilities)
  protected
  public
    /// <summary>
    /// Win32 API FormatMessage function
    /// http://msdn.microsoft.com/en-us/library/windows/desktop/ms679351%28v=vs.85%29.aspx
    /// </summary>
    class function FormatMessStr(aString: String): String;
    /// <summary>
    /// Get String from a ResFile
    /// </summary>
    class function GetStrRes(const Index: Integer): String;
    /// <summary>
    /// Is Win64 running
    /// </summary>
    class function IsWin64: Boolean;
    /// <summary>
    /// Returns Windows path \Program Files\Common Files
    /// </summary>
    class function CommonFilesDir: string;
    /// <summary>
    /// Returns Windows System path directory
    /// </summary>
    class function WindowsSystemDir: String;
    /// <summary>
    /// Returns Windows installed path directory
    /// </summary>
    class function WindowsDir: string;
    /// <summary>
    /// Returns the path of the directory for windows temporary files
    /// </summary>
    class function WindowsTempPath: String;
    /// <summary>
    /// Returns NetBIOS name of the local computer
    /// </summary>
    /// <remarks>
    /// Using RPC will only return local comupter name not the RPC Windows computer name.
    ///
    /// https://msdn.microsoft.com/en-us/library/windows/desktop/ms724295(v=vs.85).aspx
    /// </remarks>
    class function GetLocalComputerName: String;
    /// <summary>
    /// Set local Windows Environment Variable
    /// </summary>
    class function SetEnvironmentVariableEx(const aVariableName: String;
      const aValue: string; aIsSystemVariable: Boolean): Integer;
    /// <summary>
    /// Set system Windows Environment Variable
    /// </summary>
    /// <param name="aVariableName">
    /// Variable name
    /// </param>
    /// <param name="aValue">
    /// Value of variable
    /// </param>
    class function SetSysEnvironmentVariable(const aVariableName: String;
      aValue: string): Boolean;
    /// <summary>
    ///   Check if a Process is running
    /// </summary>
    class function IsProcess32Exists(aFileName: string): Boolean;
    /// <summary>
    ///   Check if Unicode exists in string
    /// </summary>
    class function IsStringUniCode(aString: String): boolean;
    /// <summary>
    ///   Get Current Module Filename
    /// </summary>
    class function GetModuleFileName: String;

  end;

function CreateEnvironmentBlock(var lpEnvironment: Pointer; hToken: THandle;
  bInherit: BOOL): BOOL; stdcall; external 'userenv';
function DestroyEnvironmentBlock(pEnvironment: Pointer): BOOL; stdcall;
  external 'userenv';

implementation

class function TNovusWindows.WindowsDir: string;
begin
  SetLength(result, 255);
  Windows.GetWindowsDirectory(pChar(result), 255);
  SetLength(result, StrLen(pChar(result)));
end;

class function TNovusWindows.WindowsSystemDir: String;
begin
  SetLength(result, 255);
  Windows.GetSystemDirectory(pChar(result), 255);
  SetLength(result, StrLen(pChar(result)));
end;

class function TNovusWindows.CommonFilesDir: string;
begin
  with TRegistry.Create do
    try
      RootKey := HKey_Local_Machine;
      if OpenKey('Software\Microsoft\Windows\CurrentVersion', True) then
      begin
        if ValueExists('CommonFilesDir') then
        begin
          result := ReadString('CommonFilesDir');
        end
        else
        begin
          result := Copy(WindowsDir { } , 1, 2) + '\Program Files\Common Files';
          WriteString('CommonFilesDir', result);
        end;
      end;
    finally
      Free
    end;
end;

class function TNovusWindows.WindowsTempPath: String;
begin
  SetLength(result, Max_path);
  SetLength(result, GetTempPath(Max_path, pChar(result)));
end;

class function TNovusWindows.GetLocalComputerName;
var
  P: Pointer;
  Size: DWORD;
begin
  result := '';

  Size := MAX_COMPUTERNAME_LENGTH + 1;

  P := AllocMem(Size);

  if GetComputerName(P, Size) then
{$IFDEF DELPHI2009_UP}
    result := StrPas(PWideChar(P));
{$ELSE}
    result := StrPas(P);
{$ENDIF}
  FreeMem(P);
end;

class function TNovusWindows.SetSysEnvironmentVariable(const aVariableName
  : String; aValue: string): Boolean;
var
  fok: Boolean;
  reg: TRegistry;
resourcestring
  key = 'SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment';

begin
  reg := NIL;

  Try
    result := False;

    reg := TRegistry.Create;

    reg.Access := KEY_ALL_ACCESS or KEY_WOW64_64KEY;

    reg.RootKey := HKey_Local_Machine;

    fok := reg.KeyExists(key);
    if fok then
    begin
      if reg.OpenKey(key, True) then
      begin
        result := True;

        reg.WriteString(aVariableName, aValue);

        // SetEnvironmentVariable(PChar(aVariableName), PChar(aValue));

        SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
          Integer(pChar('Environment')));
      end;
    end;

  Finally
    if Assigned(reg) then
      reg.Free;
  End;
end;

class function TNovusWindows.SetEnvironmentVariableEx(const aVariableName
  : String; const aValue: string; aIsSystemVariable: Boolean): Integer;
begin
  if aIsSystemVariable = False then
  begin
    if Windows.SetEnvironmentVariable(pChar(aVariableName), pChar(aValue)) then
      result := 0
    else
    begin
      result := GetLastError;

      if result = 0 then
        result := -1;
    end;

  end
  else
  begin
    result := 0;

    if Not SetSysEnvironmentVariable(aVariableName, aValue) then
    begin
      result := GetLastError;

      if result = 0 then
        result := -1;
    end;

  end;
end;

class function TNovusWindows.IsWin64: Boolean;
var
  IsWow64Process: function(hProcess: THandle; var Wow64Process: BOOL)
    : BOOL; stdcall;
  Wow64Process: BOOL;
begin
  result := False;

  if sizeof(IntPtr) = 8 then   //  is 64Bit App
    result := true
  else
    begin
      IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');
      if Assigned(IsWow64Process) then
      begin
        if IsWow64Process(GetCurrentProcess, Wow64Process) then
        begin
          result := Wow64Process;
        end;
      end;
    end;
end;

class function TNovusWindows.IsProcess32Exists(aFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  result := False;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(aFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(aFileName))) then
      result := True;

    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

class function TNovusWindows.IsStringUniCode(aString: String): boolean;
var
  fOpt : Integer;
begin
  Result := False;

  fOpt := IS_TEXT_UNICODE_UNICODE_MASK;
  if IsTextUnicode(PChar(aString),Length(aString),@fOpt) then Result := True;
end;

class function TNovusWindows.GetModuleFileName: String;
begin
  SetLength(result, 255);
  Windows.GetModuleFileName(hInstance, pChar(result), 255);
  SetLength(result, StrLen(pChar(result)));
end;

class function TNovusWindows.GetStrRes;
var
  buffer: array [0 .. 255] of Char;
  ls: Integer;
begin
  result := '';
  ls := LoadString(hInstance, Index, buffer, SizeOf(buffer));
  if ls <> 0 then
    result := buffer;

end;

class function TNovusWindows.FormatMessStr(aString: String): String;
const
  TextBufLength = 256;
var
  sText: array [0 .. TextBufLength] of Char;
begin
  ZeroMemory(@sText, TextBufLength);

  FormatMessage(FORMAT_MESSAGE_FROM_STRING, PWideChar(aString), 0, 0, sText,
    TextBufLength, nil);
  result := String(sText);

end;


end.
