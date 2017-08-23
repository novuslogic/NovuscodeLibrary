{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusIndyUtils;

interface

uses SysUtils, NovusNumUtils, Winsock, IdTCPClient, NovusUtilities;

Type
  TNovusIndyUtils = class(tNovusUtilities)
  protected
  public
  (*
    moved to NovusWebUtils unit

    class function UrlEncode(const DecodedStr: String; Pluses: Boolean): String;
    class function UrlDecode(const EncodedStr: String): String;
    *)
    /// <summary>
    /// Check if tcp port is open.
    /// </summary>
    class function IsTCPPortUsed(aPort: Word; aAddress: AnsiString): Boolean;
  end;

implementation

(*
class function TNovusIndyUtils.UrlEncode(const DecodedStr: String;
  Pluses: Boolean): String;
var
  I: Integer;
begin
  Result := '';
  if Length(DecodedStr) > 0 then
    for I := 1 to Length(DecodedStr) do
    begin
      if not(DecodedStr[I] in ['0' .. '9', 'a' .. 'z', 'A' .. 'Z', ' ']) then
        Result := Result + '%' + IntToHex(Ord(DecodedStr[I]), 2)
      else if not(DecodedStr[I] = ' ') then
        Result := Result + DecodedStr[I]
      else
      begin
        if not Pluses then
          Result := Result + '%20'
        else
          Result := Result + '+';
      end;
    end;
end;

class function TNovusIndyUtils.UrlDecode(const EncodedStr: String): String;
var
  I: Integer;
begin
  Result := '';
  if Length(EncodedStr) > 0 then
  begin
    I := 1;
    while I <= Length(EncodedStr) do
    begin
      if EncodedStr[I] = '%' then
      begin
        Result := Result +
          Chr(TNovusNumUtils.HexToInt64(EncodedStr[I + 1] + EncodedStr[I + 2]));
        I := Succ(Succ(I));
      end
      else if EncodedStr[I] = '+' then
        Result := Result + ' '
      else
        Result := Result + EncodedStr[I];

      I := Succ(I);
    end;
  end;
end;
*)

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

end.
