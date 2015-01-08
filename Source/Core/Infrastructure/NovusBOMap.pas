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

    function AddBOField(ABOField: tNovusBOField):  tNovusBOField;

    function FieldByName(aFieldName: String): TNovusBOField;

    property oFieldList: tNovusList
      read FoFieldList
      write FoFieldList;

    property TableName: String
      read fsTablename
      write fsTableName;



  end;

implementation

constructor TNovusBOMap.create;
begin
  inherited create;

  FoFieldList := tNovusList.Create(tNovusBOField);

  FoFieldList.Clear;
end;

destructor TNovusBOMap.destroy;
begin
  FoFieldList.Free;

  inherited destroy;
end;

function TNovusBOMap.AddBOField(ABOField: tNovusBOField):  tNovusBOField;
begin
  FoFieldList.Add(ABOField);
end;

function TNovusBOMap.FieldByName(aFieldName: String): TNovusBOField;
Var
  I: Integer;
  loNovusBOField: tNovusBOField;
begin
  Result := NIL;

  for I := 0 to FoFieldList.Count - 1 do
    begin
      loNovusBOField := tNovusBOField(FoFieldList.items[i]);

      If Uppercase(loNovusBOField.FieldName) = Uppercase(aFieldName) then
        begin
          Result := loNovusBOField;
          Break;
        end;
    end;
end;



end.
