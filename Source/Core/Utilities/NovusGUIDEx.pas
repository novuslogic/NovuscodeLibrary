unit NovusGUIDEx;

interface

uses SysUtils, NovusUtilities;

type
  TGuidExUtils = class(TNovusUtilities)

    class function NewGuid : TGuid;

    class function EqualGuids(Guid1, Guid2 : TGuid) : boolean;

    class function NewGuidString : String;

    class function NewGuidStringA : String;


    class function IsEmptyGuid(Guid : TGuid) : boolean;

    class function ToQuotedString(Guid : TGuid) : string;

    class function FromString(Value : string) : TGuid;
    class function EmptyGuid : TGuid;
    class function ToString(Guid : TGuid) : string;
  end;

implementation

uses NovusStringUtils;

{ TGuidExUtils }

class function TGuidExUtils.EmptyGuid: TGuid;
begin
  result := FromString('{00000000-0000-0000-0000-000000000000}');
end;

class function TGuidExUtils.EqualGuids(Guid1, Guid2: TGuid): boolean;
begin
  result := IsEqualGUID(Guid1, Guid2);
end;

class function TGuidExUtils.FromString(Value: string): TGuid;
begin
  result := StringToGuid(Value);
end;

class function TGuidExUtils.IsEmptyGuid(Guid : TGuid): boolean;
begin
  result := EqualGuids(Guid,EmptyGuid);
end;


class function TGuidExUtils.NewGuidString: String;
begin
  Result := TGuidExUtils.ToString(TGuidExUtils.NewGuid);
end;

class function TGuidExUtils.NewGuidStringA: String;
begin
  Result := tNovusStringUtils.StripChar(TGuidExUtils.NewGuidString, '{');

  Result := tNovusStringUtils.StripChar(Result, '}')
end;


class function TGuidExUtils.NewGuid: TGuid;
var
  Guid : TGuid;
begin
  CreateGUID(Guid);
  Result := Guid;
end;

class function TGuidExUtils.ToQuotedString(Guid: TGuid): string;
begin
  result := QuotedStr(ToString(Guid));
end;

class function TGuidExUtils.ToString(Guid: TGuid): string;
begin
  result := GuidToString(Guid);
end;

end.//GuidEx



