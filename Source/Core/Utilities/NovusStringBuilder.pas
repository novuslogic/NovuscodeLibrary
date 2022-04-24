unit NovusStringBuilder;

interface

Uses System.SysUtils, System.Classes, NovusUtilities;

type
  tNovusStringBuilder = class(tStringBuilder)
  private
  protected
  public
    function AppendasString(const Value: string): TStringBuilder;
    function ToStringAll: String;
    function SaveToFile(aFilename: String): boolean;
  end;

implementation

function tNovusStringBuilder.AppendasString(const Value: string)
  : TStringBuilder;
begin
  Result := Self.Append(Value);
end;

function tNovusStringBuilder.ToStringAll: String;
begin
  Result := Self.ToString;
end;

function tNovusStringBuilder.SaveToFile(aFilename: String): boolean;
var
  fStringList: tStringList;
  fWriter: TWriter;
begin
  Result := False;

  if Self.Length = 0 then Exit;
  Try
  Try
    fStringList := tStringList.Create;

    fStringList.Text := Self.ToStringAll;

    fStringList.SaveToFile(aFilename);
  Finally
    fStringList.Free;
  End;
  Except
    raise Exception.Create(TNovusUtilities.GetExceptMess);
  end;

end;

end.
