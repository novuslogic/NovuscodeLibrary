unit NovusStreamUtils;

interface


uses Windows, sysutils, Classes, NovusUtilities, Clipbrd;

Type
  TNovusStreamUtils = class(TNovusUtilities)
  protected
  public
    class procedure CopyMemoryStreamToClipboard(St: TmemoryStream);
    class procedure CopyMemoryStreamFromClipboard(St: TmemoryStream);
    class procedure MemoryStreamToStringList(St: TmemoryStream;Var AStringList:tStringlist);
  end;

implementation

class procedure TNovusStreamUtils.CopyMemoryStreamToClipboard(St: TmemoryStream);
var
  hbuf    : THandle;
  bufptr  : Pointer;
  CF_MYFORMAT: Word;
begin
  CF_MYFORMAT := RegisterClipboardFormat('My Format Description');

  {-- Write your data to the stream. --}
  hbuf := GlobalAlloc(GMEM_MOVEABLE, St.size);
  try
    bufptr := GlobalLock(hbuf);
    try
      Move(St.Memory^, bufptr^, St.size);
      Clipboard.SetAsHandle(CF_MYFORMAT, hbuf);
    finally
      GlobalUnlock(hbuf);
    end;
  except
    GlobalFree(hbuf);
    raise;
  end;
end;


class procedure TNovusStreamUtils.CopyMemoryStreamFromClipboard(St: TMemoryStream);
var
  hbuf    : THandle;
  bufptr  : Pointer;
  CF_MYFORMAT: Word;
begin
  CF_MYFORMAT := RegisterClipboardFormat('My Format Description');

  hbuf := Clipboard.GetAsHandle(CF_MYFORMAT);
  if hbuf <> 0 then begin
    bufptr := GlobalLock(hbuf);
    if bufptr <> nil then begin
      try
        St.WriteBuffer(bufptr^, GlobalSize(hbuf));
        St.Position := 0;
      finally
        GlobalUnlock(hbuf);
      end;
    end;
  end;
end; { CopyStreamFromClipboard }

class procedure TNovusStreamUtils.MemoryStreamToStringList(St: TmemoryStream;Var AStringList:tStringlist);
begin
  St.Position := 0;
  AStringList.Clear;
  AStringList.LoadFromStream(St);
end;


end.
