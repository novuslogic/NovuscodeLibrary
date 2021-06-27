{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusIndyUtils;

interface

uses SysUtils, NovusNumUtils, Winsock, IdTCPClient, NovusUtilities,
  IdSSLOpenSSL,
  IdHttp, IdStack, IdGlobal, classes, NovusWebUtils, System.IOUtils,
  NovusFileUtils;

Type
  TDownloadResponse = record
    fbResult: Boolean;
    fiErrorCode: Integer;
    fsErrorMessage: String;

    property Result: Boolean read fbResult write fbResult;
    property ErrorCode: Integer read fiErrorCode write fiErrorCode;
    property ErrorMessage: String read fsErrorMessage write fsErrorMessage;
  end;

    TNovusIndyUtils = class(tNovusUtilities)
    protected
    public
    /// <summary>
    /// Check if tcp port is open.
    /// </summary>
    class function IsTCPPortUsed(aPort: Word; aAddress: AnsiString): Boolean;
    /// <summary>
    /// Downloads files from the Internet and saves them to a file.
    /// </summary>
    class function URLDownloadToFile(aURL: String; aDownloadPath: String)
      : TDownloadResponse;
  end;

implementation

class function TNovusIndyUtils.IsTCPPortUsed(aPort: Word;
  aAddress: AnsiString): Boolean;
var
  LTcpClient: TIdTCPClient;
begin
  LTcpClient := TIdTCPClient.Create(nil);
  try
    try
      LTcpClient.Host := aAddress;
      LTcpClient.Port := aPort;
      LTcpClient.ConnectTimeout := 200;

      LTcpClient.Connect;
      Result := true;
    except
      Result := false;
    end;
  finally
    freeAndNil(LTcpClient);
  end;
end;

class function TNovusIndyUtils.URLDownloadToFile(aURL: String;
  aDownloadPath: String): TDownloadResponse;
var
  fHTTP: TIdHTTP;
  fIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  FResponse: TMemoryStream;
  lsFilename: String;
begin
  Result.Result := false;

  Result.ErrorCode := -1;

  lsFilename := TNovusWebUtils.GetURLFilename(aURL);

  if Trim(lsFilename) = '' then
    Exit;

  Result.ErrorCode := -2;

  If not DirectoryExists(TNovusFileUtils.TrailingBackSlash(aDownloadPath)) then
    Exit;

  try
    fHTTP := TIdHTTP.Create(NIL);
    fIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(fHTTP);
    try
      fIOHandler.SSLOptions.method := sslvSSLv3;
      fIOHandler.SSLOptions.SSLVersions :=
        [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

      fIOHandler.ReadTimeout := IdTimeoutInfinite;
      fIOHandler.ConnectTimeout := IdTimeoutInfinite;

      fHTTP.IOHandler := fIOHandler;

      FResponse := TMemoryStream.Create;

      fHTTP.Get(aURL, FResponse);

      Result.Result := (fHTTP.Response.ResponseCode = 200);

      if Result.Result then
        FResponse.SaveToFile(TNovusFileUtils.TrailingBackSlash(aDownloadPath) +
          lsFilename);

    except
      on E: EIdHTTPProtocolException do
      begin
        Result.Result := false;

        Result.ErrorCode := E.ErrorCode;
        Result.ErrorMessage := E.ErrorMessage;
        fHTTP.Disconnect;
      end;

    end;
  finally
    fHTTP.Disconnect;

    fIOHandler.Free;
    fHTTP.Free;
    FResponse.Free;
  end;

end;

end.
