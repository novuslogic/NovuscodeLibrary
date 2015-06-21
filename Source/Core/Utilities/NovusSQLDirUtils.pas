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
                  AServerName: String;
                  ARemoteDatabase: String;
                  AAliasName: String;
                  AServerType: TSDServerType;
                  AUserName,
                  APassword: string;
                  AParams: tStringList;
                  ASQLLibrary: string;
                  ALoginPrompt: Boolean = False;
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

//  Provider=SQLOLEDB.11;Persist Security Info=False;User ID=sa;Initial Catalog=tourworkpad;Data Source=127.0.0.1;

  if TSDServerType(AServerType) = stOLEDB then
    begin
      ADatabase.Params.Add('INIT COM=FALSE');

      if ASQLLibrary <> '' then
        ADatabase.Remotedatabase := 'Provider=' + aSQLLibrary + ';' +
        'User ID=' + aUsername + ';' +
        'Password=' + aPassword + ';' +
        'Initial Catalog=' + ARemoteDatabase +';' +
        'Data Source=' + aServerName + ';'
      else
         ADatabase.Remotedatabase := 'User ID=' + aUsername + ';' +
        'Password=' + aPassword + ';' +
        'Initial Catalog=' + ARemoteDatabase +';' +
        'Data Source=' + aServerName + ';'
    end
  else
  If TSDServerType(AServerType) = stSQLServer then
    begin
      ADatabase.Params.Add('USE OLEDB=TRUE');
      ADatabase.Params.Add('INIT COM=FALSE');

      ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;
    end
  else
  If (TSDServerType(AServerType) = stFirebird) or
     (TSDServerType(AServerType) = stInterbase) then
    begin
      ADatabase.Params.Add('SQL Dialect=3');

      If Trim(AServername) = '' then
        ADatabase.Remotedatabase := ARemoteDatabase
      else
      If Uppercase(Trim(AServername)) = 'LOCALHOST' then
        ADatabase.Remotedatabase := ARemoteDatabase
      else
        ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;


      If ASQLLibrary <> '' then
        ADatabase.Params.Add('Firebird API Library=' + ASQLLibrary);
    end
  else
    begin
      ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;
    end;

  if Assigned(AParams) then
    ADatabase.Params.Add(AParams.Text);


  (*
  ADatabase.Connected := False;

  ADatabase.LoginPrompt := ALoginPrompt;

  ADatabase.KeepConnection := True;

  ADatabase.DatabaseName := AAliasName;

  ADatabase.Params.Clear;

  ADatabase.ServerType := TSDServerType(AServerType);

  if TSDServerType(AServerType) = stOLEDB then
    begin
      ADatabase.Params.Add('INIT COM=FALSE');

      if ASQLClientLib <> '' then
        ADatabase.Remotedatabase := 'Provider=' + ASQLClientLib + ';' +
        'User ID=' + aUsername + ';' +
        'Password=' + aPassword + ';' +
        'Initial Catalog=' + ARemoteDatabase +';' +
        'Data Source=' + aServerName + ';'
      else
         ADatabase.Remotedatabase := 'User ID=' + aUsername + ';' +
        'Password=' + aPassword + ';' +
        'Initial Catalog=' + ARemoteDatabase +';' +
        'Data Source=' + aServerName + ';'
    end
  else
  If AServerType = stSQLServer then
    begin
      ADatabase.ServerType := AServerType;

      ADatabase.Params.Add('USE OLEDB=TRUE');
      ADatabase.Params.Add('INIT COM=FALSE');

      ADatabase.Params.Add('User name=' + AUserName);
      ADatabase.Params.Add('Password=' +  APassword);

      ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;
    end
  else
  If AServerType = stFirebird then
    begin
      ADatabase.ServerType := TSDServerType(AServerType);

      ADatabase.Params.Add('User name=' + AUserName);
      ADatabase.Params.Add('Password=' +  APassword);
      ADatabase.Params.Add('SQL Dialect=3');

      If Uppercase(AServername) = 'LOCALHOST' then
        ADatabase.Remotedatabase := ARemoteDatabase
      else
        ADatabase.Remotedatabase := AServerName + ':' + ARemoteDatabase;

    end
  else
  If AServerType = stMySQL then
    begin
      ADatabase.ServerType :=TSDServerType(AServerType);

    end;

  if Assigned(AParams) then
    ADatabase.Params.Add(AParams.Text);

  If (ASQLClientLib <> '') and (TSDServerType(AServerType) <> stOLEDB) then
    ADatabase.Params.Add(TNovusSQLDirUtils.GetServerTypeAsString(AServerType) + ' API Library=' + ASQLClientLib);
  *)
end;

class function TNovusSQLDirUtils.GetServerTypeAsString(ServerType:
TSDServerType): String;
begin
  case ServerType of
   stSQLBase: Result := 'SQLBase';
   stOracle: Result := 'Oracle';
   stSQLServer: Result := 'SQLServer';
   stSybase: Result := 'Sybase';
   stDB2: Result := 'DB2';
   stInformix: Result := 'Informix';
   stODBC: Result := 'ODBC';
   stInterbase: result := 'Interbase';
   stFirebird: result := 'Firebird';
   stMySQL: result := 'MySQL';
   stPostgreSQL: result := 'PostgreSQL';
   stOLEDB: result := 'OLEDB';
  else
    Result := '';
  end;
end;

class function TNovusSQLDirUtils.GetServerTypeAsInteger(ServerType: String):
Integer;
begin
  Result := -1;

  ServerType := uppercase(serverType);

  if ServerType ='SQLBASE' then
    Result := integer(stSQLBase)
  else
  if Servertype = 'ORACLE' then
    Result := integer(stOracle)
  else
  if ServerType = 'SQLSERVER' then
    Result := integer(stSQLServer)
  else
  If Servertype = 'SYBASE' then
    Result := integer(stSybase)
  else
  if Servertype = 'DB2' then
    result := integer(stDB2)
  else
  if Servertype = 'INFORMIX' then
    result := integer(stInformix)
  else
  if Servertype = 'ODBC' then
    result := integer(stODBC)
  else
  if Servertype = ' INTERBASE' then
    result := integer(stInterbase)
  else
  if Servertype = 'FIREBIRD' then
    result := integer(stFirebird)
  else
  if Servertype = 'MYSQL' then
    result := integer(stMySQL)
  else
  if Servertype = 'POSTGRESQL' then
    result := integer(stPostgreSQL)
  else
  if Servertype = 'OLEDB' then
    result := integer(stOLEDB);
end;


end.

