{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusIndyUtils;

interface

uses SysUtils, NovusNumUtils, Winsock, IdTCPClient, NovusUtilities, IdSSLOpenSSL,
     IdHttp, IdStack, IdGlobal, classes, NovusWebUtils, System.IOUtils,
     NovusFileUtils;

Type
  TNovusIndyUtils = class(tNovusUtilities)
  protected
  public
    /// <summary>
    /// Check if tcp port is open.
    /// </summary>
    class function IsTCPPortUsed(aPort: Word; aAddress: AnsiString): Boolean;
    /// <summary>
    ///  Downloads files from the Internet and saves them to a file.
    /// </summary>
    class function URLDownloadToFile(aURL: String; aDownloadPath: String): Integer;
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

class function TNovusIndyUtils.URLDownloadToFile(aURL: String; aDownloadPath: String): Integer;
var
  fHTTP: TIdHTTP;
  fIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  FResponse:  TMemoryStream;
  lsFilename: String;
begin
  Result := -1;

  lsFilename := TNovusWebUtils.GetURLFilename(aURL);

  if Trim(lsFilename) = '' then Exit;

  Result := -2;

  If not DirectoryExists(TNovusFileUtils.TrailingBackSlash(aDownloadPath)) then Exit;


  try
    fHttp := tIDHttp.Create(NIL);
    FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHttp);

    fIOHandler.SSLOptions.method := sslvSSLv3;
    fIOHandler.SSLOptions.SSLVersions := [sslvTLSv1,sslvTLSv1_1,sslvTLSv1_2];

    fIOHandler.ReadTimeout := IdTimeoutInfinite;
    fIOHandler.ConnectTimeout := IdTimeoutInfinite;

    FHttp.IOHandler := FIOHandler;

    FResponse := TMemoryStream.Create;

    FHttp.Get(aURL, FResponse);

    FResponse.SaveToFile(TNovusFileUtils.TrailingBackSlash(aDownloadPath) + lsFilename);

    result := 200;

  finally
    fIOHandler.Free;
    fHttp.Free;
    FResponse.Free;
  end;







end;



end.
