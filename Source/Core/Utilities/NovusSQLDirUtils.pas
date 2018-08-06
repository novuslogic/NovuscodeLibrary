{$I ..\..\core\NovusCodeLibrary.inc}
unit NovusSQLDirUtils;

interface

Uses SDEngine, SysUtils, SDConsts, Activex, NovusUtilities,
  Classes;

type
  TNovusSQLDirUtils = class(tNovusUtilities)
  public
    class function GetServerTypeAsString(ServerType: TSDServerType): String;
    class function GetServerTypeAsInteger(ServerType: String): Integer;

    class procedure SetupSDDatabase(Const ADatabase: tsddatabase;
      AServerName: String; ARemoteDatabase: String; AAliasName: String;
      AServerType: TSDServerType; AUserName, APassword: string;
      AParams: tStringList; ASQLLibrary: string; ALoginPrompt: Boolean = False;
      APort: Integer = 0);
  end;

implementation

class procedure TNovusSQLDirUtils.SetupSDDatabase;
begin
  ADatabase.Connected := False;

  ADatabase.LoginPrompt := False;

  ADatabase.KeepConnection := True;

  If ADatabase.DatabaseName <> AAliasName then
    ADatabase.DatabaseName := AAliasName;

  ADatabase.ServerType := TSDServerType(AServerType);

  ADatabase.Params.Clear;

  if TSDServerType(AServerType) <> stOLEDB then
  begin
    ADatabase.Params.Values[szUSERNAME] := AUserName;
    ADatabase.Params.Values[szPASSWORD] := APassword;
  end;

  if TSDServerType(AServerType) = stOLEDB then
  begin
    ADatabase.Params.Add('INIT COM=FALSE');

    if ASQLLibrary <> '' then
      ADatabase.Remotedatabase := 'Provider=' + ASQLLibrary + ';' + 'User ID=' +
        AUserName + ';' + 'Password=' + APassword + ';' + 'Initial Catalog=' +
        ARemoteDatabase + ';' + 'Data Source=' + AServerName + ';'
    else
      ADatabase.Remotedatabase := 'User ID=' + AUserName + ';' + 'Password=' +
        APassword + ';' + 'Initial Catalog=' + ARemoteDatabase + ';' +
        'Data Source=' + AServerName + ';'
  end
  else If TSDServerType(AServerType) = stSQLServer then
  begin
    ADatabase.Params.Add('USE OLEDB=TRUE');
    ADatabase.Params.Add('INIT COM=FALSE');

    ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;
  end
  else If (TSDServerType(AServerType) = stFirebird) or
    (TSDServerType(AServerType) = stInterbase) then
  begin
    ADatabase.Params.Add('SQL Dialect=3');

    If Trim(AServerName) = '' then
      ADatabase.Remotedatabase := ARemoteDatabase
    else If Uppercase(Trim(AServerName)) = 'LOCALHOST' then
      ADatabase.Remotedatabase := ARemoteDatabase
    else
      ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;

    if APort <> 0 then
        ADatabase.Params.Add('Port='+ IntToStr(aPort));


    If ASQLLibrary <> '' then
      ADatabase.Params.Add('Firebird API Library=' + ASQLLibrary);
  end
  else
  begin
    ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;
  end;

  if Assigned(AParams) then
    ADatabase.Params.Add(AParams.Text);
end;

class function TNovusSQLDirUtils.GetServerTypeAsString
  (ServerType: TSDServerType): String;
begin
  case ServerType of
    stSQLBase:
      Result := 'SQLBase';
    stOracle:
      Result := 'Oracle';
    stSQLServer:
      Result := 'SQLServer';
    stSybase:
      Result := 'Sybase';
    stDB2:
      Result := 'DB2';
    stInformix:
      Result := 'Informix';
    stODBC:
      Result := 'ODBC';
    stInterbase:
      Result := 'Interbase';
    stFirebird:
      Result := 'Firebird';
    stMySQL:
      Result := 'MySQL';
    stPostgreSQL:
      Result := 'PostgreSQL';
    stOLEDB:
      Result := 'OLEDB';
  else
    Result := '';
  end;
end;

class function TNovusSQLDirUtils.GetServerTypeAsInteger
  (ServerType: String): Integer;
begin
  Result := -1;

  ServerType := Uppercase(ServerType);

  if ServerType = 'SQLBASE' then
    Result := Integer(stSQLBase)
  else if ServerType = 'ORACLE' then
    Result := Integer(stOracle)
  else if ServerType = 'SQLSERVER' then
    Result := Integer(stSQLServer)
  else If ServerType = 'SYBASE' then
    Result := Integer(stSybase)
  else if ServerType = 'DB2' then
    Result := Integer(stDB2)
  else if ServerType = 'INFORMIX' then
    Result := Integer(stInformix)
  else if ServerType = 'ODBC' then
    Result := Integer(stODBC)
  else if ServerType = ' INTERBASE' then
    Result := Integer(stInterbase)
  else if ServerType = 'FIREBIRD' then
    Result := Integer(stFirebird)
  else if ServerType = 'MYSQL' then
    Result := Integer(stMySQL)
  else if ServerType = 'POSTGRESQL' then
    Result := Integer(stPostgreSQL)
  else if ServerType = 'OLEDB' then
    Result := Integer(stOLEDB);
end;

end.
