unit NovusList;

interface

uses
  contnrs, NovusInfrastructre;

type
  TNovusList = class(TNovusInfrastructre)
  protected
    foParentObject: TObject;
    fsaClassname: String;
    faclass: TClass;
    FList: TObjectList;
  private
    function Get(Index: Integer): TObject;
    procedure Put(Index: Integer; Item: TObject);
    function GetCount: Integer;
  public
    constructor Create(aClass: TClass); virtual;
    destructor Destroy; override;

    procedure InitClass(aClass: tClass);

    procedure Insert(AItem: TObject; AIndex: Integer);

    function Add(AItem: TObject): Integer;
    function Delete(AItem: TObject): Boolean;
    function Equals(AList: TNovusList): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TObject read Get write Put; default;
    procedure Clear;

    procedure CopyFrom(aNovusList: TNovusList);

    property List: TObjectList read FList write FList;

    property aClass: TClass read faclass write faclass;

    property aClassname: string read fsaClassname write fsaClassname;

    property oParentObject: TObject read foParentObject write foParentObject;
  end;

implementation

uses
  SysUtils,
  NovusBO;

constructor TNovusList.Create(aClass: TClass);
begin
  inherited Create;

  InitClass(aClass);
end;

destructor TNovusList.Destroy;
begin
  Clear;

  FList.Free;
  inherited Destroy;
end;

procedure TNovusList.InitClass(aClass: tClass);
begin
  foParentObject := NIL;

  aClassname := aClass.Classname;

  FList := TObjectList.Create;
  faclass := aClass;
end;

function TNovusList.Get(Index: Integer): TObject;
begin
  Result := FList[Index];
end;

function TNovusList.Add(AItem: TObject): Integer;
var
  Test: Boolean;
begin
  try
    Test := AItem is faclass;
  except
    on Exception do
      raise EInvalidCast.Create
        (Format('NovusList: Cannot add a non-object to a list of %s objects',
        [faclass.Classname]));
  end;
  if Test then
    Result := FList.Add(AItem)
  else
    raise EInvalidCast.Create
      (Format('NovusList: Cannot add a %s object to a list of %s objects',
      [AItem.Classname, faclass.Classname]));
end;

procedure TNovusList.Insert(AItem: TObject; AIndex: Integer);
var
  Test: Boolean;
begin
  try
    Test := AItem is faclass;
  except
    on Exception do
      raise EInvalidCast.Create
        (Format('NovusList: Cannot insert a non-object to a list of %s objects',
        [faclass.Classname]));
  end;
  if Test then
    FList.Insert(AIndex, AItem)
  else
    raise EInvalidCast.Create
      (Format('NovusList: Cannot insert a %s object to a list of %s objects',
      [AItem.Classname, faclass.Classname]));
end;

function TNovusList.Delete(AItem: TObject): Boolean;
Var
  liIndex: Integer;
begin
  Result := False;

  liIndex := FList.IndexOf(AItem);
  If liIndex > -1 then
  begin
    FList.Remove(AItem);

    FList.Pack;

    Result := True;
  end;
end;

procedure TNovusList.Put(Index: Integer; Item: TObject);
var
  Test: Boolean;
begin
  try
    Test := Item is faclass;
  except
    on Exception do
      raise EInvalidCast.Create
        (Format('NLSafeList: Cannot put a non-object into a list of %s objects',
        [faclass.Classname]));
  end;
  if Test then
    FList[Index] := Item
  else
    raise EInvalidCast.Create
      (Format('NLSafeList: Cannot put a %s object into a list of %s objects',
      [TObject(Item).Classname, faclass.Classname]));
end;

function TNovusList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TNovusList.Equals(AList: TNovusList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AList.Count <> FList.Count then
    Exit;
  for I := 0 to AList.Count - 1 do
    if AList[I] <> FList[I] then
      Exit;
  Result := True;
end;

procedure TNovusList.Clear;
begin
  FList.Clear;
end;

procedure TNovusList.CopyFrom(aNovusList: TNovusList);
Var
  I: Integer;
begin
  Clear;

  for I := 0 to aNovusList.Count - 1 do
    Add(aNovusList.Items[I]);
end;

end.
