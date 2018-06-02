unit NovusConsole;

interface

Uses Winapi.Windows, SysUtils;

type
  TKeyEvent = record
    KeyCode: Word;
    ScanCode: Word;
  end;

  TNovusConsole = class
  private
  protected
  public
    /// <summary>
    /// Check if the console has a key press
    /// </summary>
    /// <param name="aHandle">
    /// Windows Console Handle
    /// </param>
    class function IsAvailableKey(aHandle: THandle): Boolean;
    /// <summary>
    /// Check if the console has a key press and return key
    /// </summary>
    /// <param name="aHandle">
    /// Windows Console Handle
    /// </param>
    class function IsAvailableKeyEx(aHandle: THandle): TKeyEvent;
    /// <summary>
    /// Get available char from console.
    /// </summary>
    class function GetAvailableChar(aHandle: THandle): char;
    /// <summary>
    /// Get Standard Input Handle
    /// </summary>
    class function GetStdInputHandle: THandle;
    /// <summary>
    /// Get Standard Output Handle
    /// </summary>
    class function GetStdOutputHandle: THandle;
  end;



implementation

class function TNovusConsole.GetStdInputHandle: THandle;
begin
  Result := GetStdHandle(STD_INPUT_HANDLE)
end;

class function TNovusConsole.GetStdOutputHandle: THandle;
begin
  Result := GetStdHandle(STD_OUTPUT_HANDLE)
end;

class function TNovusConsole.GetAvailableChar(aHandle: THandle): char;
var
  FInputRecord: TInputRecord;
  FNumRead: Cardinal;
begin
  repeat
    ReadConsoleInput(aHandle, FInputRecord, 1, FNumRead);

    if FInputRecord.Event.KeyEvent.bKeyDown and
      (FInputRecord.EventType = KEY_EVENT) then
    begin
      if FInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE then
      begin
        Result := Chr(FInputRecord.Event.KeyEvent.wVirtualKeyCode);

        break;
      end;

      if FInputRecord.Event.KeyEvent.AsciiChar <> #0 then
      begin
        Result := Chr(Ord(FInputRecord.Event.KeyEvent.AsciiChar));

        break;
      end;

      break;
    end;

  until false;
End;

class function TNovusConsole.IsAvailableKeyEx(aHandle: THandle): TKeyEvent;
Var
  i, numEvents: Cardinal;
  events: Array of TInputRecord;
Begin
  Result.KeyCode := 0;
  Result.ScanCode :=0 ;
  Win32Check(GetNumberOfConsoleInputEvents(aHandle, numEvents));
  If numEvents > 0 Then
  Begin
    SetLength(events, numEvents);
    Win32Check(PeekConsoleInput(aHandle, events[0], numEvents, numEvents));
    For i := 0 to numEvents - 1 Do
      If (events[i].EventType = KEY_EVENT) and
        (events[i].Event.KeyEvent.bKeyDown) Then
      Begin
        Result.KeyCode := events[i].Event.KeyEvent.wVirtualKeyCode;
        Result.ScanCode := events[i].Event.KeyEvent.wVirtualScanCode;

        break;
      End;
  End;
End;


class function TNovusConsole.IsAvailableKey(aHandle: THandle): Boolean;
Var
  loKeyEvent: TKeyEvent;
begin
  loKeyEvent := TNovusConsole.IsAvailableKeyEx(aHandle);

  Result := ((loKeyEvent.KeyCode <>0) or (loKeyEvent.ScanCode <> 0));
end;

end.
