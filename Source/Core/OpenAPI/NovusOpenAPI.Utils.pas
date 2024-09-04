unit NovusOpenAPI.Utils;

interface

Uses System.SysUtils;

type
  TNovusOpenAPIUtils = class
  protected
  private
  public
    class function OpenAPIParamToDelphiFunctionType(aParamName: String): String;
    class function OpenAPITypeToDelphiType(const OpenAPIType, OpenAPIFormat: string): string;
    class function IsComponentsSchemas(aRef: string): Boolean;
    class function GetComponentsSchemasType(aRef: string): string;
  end;

implementation

class function TNovusOpenAPIUtils.OpenAPIParamToDelphiFunctionType(aParamName: String): String;
begin
  Result := StringReplace(aParamName, '-', '_', [rfReplaceAll]);
end;

class function TNovusOpenAPIUtils.OpenAPITypeToDelphiType(const OpenAPIType, OpenAPIFormat: string): string;
begin
  if OpenAPIType = 'string' then
    Result := 'string'
  else if OpenAPIType = 'integer' then
    Result := 'Integer'
  else if OpenAPIType = 'boolean' then
    Result := 'Boolean'
  else if (OpenAPIType = 'number') and (OpenAPIFormat = 'float') then
    Result := 'Single'
  else if (OpenAPIType = 'number') and (OpenAPIFormat = 'double') then
    Result := 'Double';

end;

class function TNovusOpenAPIUtils.IsComponentsSchemas(aRef: string): Boolean;
begin
  // Check if the substring '#/components/schemas' exists in the string aRef
  Result := Pos('#/components/schemas', aRef) > 0;
end;


class function TNovusOpenAPIUtils.GetComponentsSchemasType(aRef: string): string;
const
  Prefix = '#/components/schemas/';
var
  PrefixLength, TypeStartPos: Integer;
begin
  // Initialize result to an empty string
  Result := '';

  // Check if the string starts with the prefix
  if Pos(Prefix, aRef) = 1 then
  begin
    PrefixLength := Length(Prefix);
    TypeStartPos := PrefixLength + 1;

    // Extract the type name after the prefix
    Result := Copy(aRef, TypeStartPos, Length(aRef) - PrefixLength);
  end;
end;


end.

