unit NovusBOMap;

interface

Uses NovusInfrastructre, NovusList, NovusBOField, SysUtils;

Type
  TNovusBOMap = class(TNovusInfrastructre)
  private
  protected
    FoFieldList: tNovusList;
    fsTableName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddBOField(ABOField: tNovusBOField): tNovusBOField;

    function FieldByName(aFieldName: String): tNovusBOField;

    property oFieldList: tNovusList read FoFieldList write FoFieldList;

    property TableName: String read fsTableName write fsTableName;
  end;

implementation

constructor TNovusBOMap.Create;
begin
  inherited Create;

  FoFieldList := tNovusList.Create(tNovusBOField);

  FoFieldList.Clear;
end;

destructor TNovusBOMap.Destroy;
begin
  FoFieldList.Free;

  inherited Destroy;
end;

function TNovusBOMap.AddBOField(ABOField: tNovusBOField): tNovusBOField;
begin
  FoFieldList.Add(ABOField);
end;

function TNovusBOMap.FieldByName(aFieldName: String): tNovusBOField;
Var
  I: Integer;
  loNovusBOField: tNovusBOField;
begin
  Result := NIL;

  for I := 0 to FoFieldList.Count - 1 do
  begin
    loNovusBOField := tNovusBOField(FoFieldList.items[I]);

    If Uppercase(loNovusBOField.FieldName) = Uppercase(aFieldName) then
    begin
      Result := loNovusBOField;
      Break;
    end;
  end;
end;

end.
