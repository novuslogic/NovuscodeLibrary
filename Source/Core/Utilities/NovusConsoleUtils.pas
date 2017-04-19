unit NovusConsoleUtils;

interface

Uses NovusUtilities, Winapi.Windows, SysUtils;

type
  TNovusConsoleUtils = class(TNovusUtilities)
  private
  protected
  public
    /// <summary>
    /// Check if the console has a key press.
    /// </summary>
    /// <param name="aHandle">
    /// Windows Console Handle
    /// </param>
    class function IsAvailableKey(aHandle: THandle): Boolean;
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

class function TNovusConsoleUtils.GetStdInputHandle: THandle;
begin
  Result := GetStdHandle(STD_INPUT_HANDLE)
end;

class function TNovusConsoleUtils.GetStdOutputHandle: THandle;
begin
  Result := GetStdHandle(STD_OUTPUT_HANDLE)
end;

class function TNovusConsoleUtils.GetAvailableChar(aHandle: THandle): char;
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

  // Win32Check(
  // ReadConsole( ahandle,
  // @result, 1,
  // charsread, nil ));
End;

class function TNovusConsoleUtils.IsAvailableKey(aHandle: THandle): Boolean;
Var
  i, numEvents: Cardinal;
  events: Array of TInputRecord;
Begin
  Result := false;
  Win32Check(GetNumberOfConsoleInputEvents(aHandle, numEvents));
  If numEvents > 0 Then
  Begin
    SetLength(events, numEvents);
    Win32Check(PeekConsoleInput(aHandle, events[0], numEvents, numEvents));
    For i := 0 to numEvents - 1 Do
      If (events[i].EventType = KEY_EVENT) and
        (events[i].Event.KeyEvent.bKeyDown) Then
      Begin
        Result := true;
        break;
      End;
  End;
End;

end.
