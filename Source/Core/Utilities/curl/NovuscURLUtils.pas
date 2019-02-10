unit NovuscURLUtils;

interface

Uses
   NovusUtilities, Curl.Easy, Curl.Lib, Curl.Interfaces, Curl.HttpCodes, System.SysUtils, System.Variants, System.Classes,
   NovusFileUtils, NovusWebUtils;

type
  tNovuscURLUtils = class(TNovusUtilities)
  private
    fsErrorMessage: String;
    fsHeader: UnicodeString;
  protected
  public
    constructor Create;
    destructor  Destroy;

    function DownloadFile(aURL: UnicodeString; aDownloadPath: String): boolean;

    class function HeaderFunc(
          var Buffer;
          Size, NItems : NativeUInt;
          OutStream : pointer) : NativeUInt;  cdecl;  static;

    class function XferInfo(
          ClientP : pointer;
          DlTotal, DlNow, UlTotal, UlNow : TCurlOff) : integer;  cdecl;  static;

    property ErrorMessage: String
      read fsErrorMessage
      write fsErrorMessage;

    property Header: UnicodeString
      read fsHeader
      write fsHeader;
  end;


implementation

constructor tNovuscURLUtils.Create;
begin
  fsErrorMessage := '';
end;

destructor tNovuscURLUtils.Destroy;
begin
end;

class function tNovuscURLUtils.XferInfo(
      ClientP : pointer;
      DlTotal, DlNow, UlTotal, UlNow : TCurlOff) : integer;
begin
  Result := 0;
end;

class function tNovuscURLUtils.HeaderFunc(
      var Buffer;
      Size, NItems : NativeUInt;
      OutStream : pointer) : NativeUInt;  cdecl;
begin
  Result := Size * NItems;
end;

function tNovuscURLUtils.DownloadFile(aURL: UnicodeString; aDownloadPath: String): boolean;
Var
  fs : TFileStream;
  FCurl : ICurl;
  lsFilename: UnicodeString;
begin
  Try
    Result := False;

    FCurl := CurlGet;
    Fcurl.SetUrl(aURL);

    lsFilename := TNovusWebUtils.GetURLFilename(aURL);

    fs := TFileStream.Create(TNovusFileUtils.TrailingBackSlash(aDownloadPath) + lsFilename, fmCreate);
    Fcurl.SetRecvStream(fs, [csfAutoDestroy]);
    Fcurl.SetFollowLocation(true);

    fcurl.SetOpt(CURLOPT_NOPROGRESS, 0);
    fcurl.SetOpt(CURLOPT_XFERINFOFUNCTION, @XferInfo);
    fcurl.SetOpt(CURLOPT_XFERINFODATA, Self);

    fcurl.SetOpt(CURLOPT_HEADERFUNCTION, @HeaderFunc);
    fcurl.SetOpt(CURLOPT_HEADERDATA, Self);

    Fcurl.SetOpt(CURLOPT_SSL_VERIFYPEER, 0);
    Fcurl.SetOpt(CURLOPT_SSL_VERIFYHOST, 0);
    try
     Fcurl.Perform;
     if Fcurl.ResponseCode <> HTTP_OK then
        fsErrorMessage := Format('HTTP error %d.', [Fcurl.ResponseCode])
    else result := True;
  except
    on e : ECurlError do
      if e.Code = CURLE_ABORTED_BY_CALLBACK
        then fsErrorMessage := 'Operation stopped.'
        else fsErrorMessage := e.Message;
    on e : Exception do
      fsErrorMessage := e.Message;
    else
      fsErrorMessage := 'Unknown error';
  end;
  Finally
    FCurl := NIL;
  End;

end;


end.
