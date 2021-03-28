unit NovusGUID;

interface

uses SysUtils, NovusUtilities;

type
  TNovusGuid = class(TNovusUtilities)
    class function NewGuid: TGuid;
    class function EqualGuids(Guid1, Guid2: TGuid): boolean;
    class function NewGuidString: String;
    class function NewGuidNoBracketsString: String;
    class function IsEmptyGuid(Guid: TGuid): boolean;
    class function ToQuotedString(Guid: TGuid): string;
    class function FromString(Value: string): TGuid;
    class function EmptyGuid: TGuid;
    class function ToString(Guid: TGuid): string;
  end;

implementation

uses NovusStringUtils;

class function TNovusGuid.EmptyGuid: TGuid;
begin
  result := FromString('{00000000-0000-0000-0000-000000000000}');
end;

class function TNovusGuid.EqualGuids(Guid1, Guid2: TGuid): boolean;
begin
  result := IsEqualGUID(Guid1, Guid2);
end;

class function TNovusGuid.FromString(Value: string): TGuid;
begin
  result := StringToGuid(Value);
end;

class function TNovusGuid.IsEmptyGuid(Guid: TGuid): boolean;
begin
  result := EqualGuids(Guid, EmptyGuid);
end;

class function TNovusGuid.NewGuidString: String;
begin
  result := TNovusGuid.ToString(TNovusGuid.NewGuid);
end;

class function TNovusGuid.NewGuidNoBracketsString: String;
begin
  result := tNovusStringUtils.StripChar(TNovusGuid.NewGuidString, '{');

  result := tNovusStringUtils.StripChar(result, '}')
end;

class function TNovusGuid.NewGuid: TGuid;
var
  Guid: TGuid;
begin
  CreateGUID(Guid);
  result := Guid;
end;

class function TNovusGuid.ToQuotedString(Guid: TGuid): string;
begin
  result := QuotedStr(ToString(Guid));
end;

class function TNovusGuid.ToString(Guid: TGuid): string;
begin
  result := GuidToString(Guid);
end;

end.
