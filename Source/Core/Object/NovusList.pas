unit NovusList;

interface

uses
  contnrs, NovusObject, Classes, Generics.Collections, NovusUtilities;

type
  TNovusList = class;

  tHashItem = class(TObject)
  protected
  private
    fiIndex: Integer;
    fsKey: string;
  public
    property Index: Integer read fiIndex write fiIndex;
    property Key: string read fsKey write fsKey;
  end;


  TNovusList = class(TNovusObject)
  protected
    fbInSensitiveKey: Boolean;
    foParentObject: TObject;
    fsaClassname: String;
    faclass: TClass;
    FList: TObjectList;
    FHash: TDictionary<String,tHashItem>;
  private
    function Get(Index: Integer): TObject;
    procedure Put(Index: Integer; Item: TObject);
    function GetCount: Integer;
    function InternalAdd(AItem: TObject): Integer;
  public
    constructor Create(aClass: TClass = NIL); virtual;
    destructor Destroy; override;

    procedure InitClass(aClass: tClass);

    procedure Insert(AItem: TObject; AIndex: Integer);

    function GetEnumerator: TlistEnumerator;

    function Add(AItem: TObject): Integer; overload;
    function Add(aKey: string; AItem: TObject): Integer; overload;
    function Delete(AItem: TObject): Boolean;
    function Equals(AList: TNovusList): Boolean;
    procedure Clear;

    procedure CopyFrom(aNovusList: TNovusList);
    function FindItem(aKey: string): TObject;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TObject read Get write Put; default;


    property List: TObjectList read FList write FList;

    property aClass: TClass read faclass write faclass;

    property aClassname: string read fsaClassname write fsaClassname;

    property oParentObject: TObject read foParentObject write foParentObject;

    property InSensitiveKey: boolean
       read fbInSensitiveKey
       write fbInSensitiveKey;
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

  FHash.Clear;
  FHash.Free;
  FList.Free;
  inherited Destroy;
end;

procedure TNovusList.InitClass(aClass: tClass);
begin
  foParentObject := NIL;


  if aClass <> NIL then
     aClassname := aClass.Classname;

  FHash:= TDictionary<String, tHashItem>.Create;

  FList := TObjectList.Create;
  FList.OwnsObjects := false;
  fbInSensitiveKey := false;

  faclass := aClass;
end;

function TNovusList.Get(Index: Integer): TObject;
begin
  Result := FList[Index];
end;

function TNovusList.Add(AItem: TObject): Integer;
begin
  Result := InternalAdd(AItem);
end;


function TNovusList.Add(aKey: string; AItem: TObject): Integer;
Var
   FHashItem : tHashItem;
begin
  Result := InternalAdd(AItem);

  FHashItem := tHashItem.Create;
  FHashItem.Index := Result;

  if fbInSensitiveKey then
    FHashItem.Key := Uppercase(aKey)
  else
    FHashItem.Key := aKey;

  //FHash.Add(aKey, FHashItem);

  FHash.Add(FHashItem.Key, FHashItem);
end;

function TNovusList.InternalAdd(AItem: TObject): Integer;
var
  Test: Boolean;
begin
  if (faclass <> NIL) then
    begin
      try
        Test := (AItem is faclass);
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
    end
   else
     Result := FList.Add(AItem);
end;

procedure TNovusList.Insert(AItem: TObject; AIndex: Integer);
var
  Test: Boolean;
begin
  if faclass <> nil then
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

    end
  else
     FList.Insert(AIndex, AItem);

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
  if faclass <>  NIL then
    begin
      try
        Test := Item is faclass;
      except
        on Exception do
          raise EInvalidCast.Create
            (Format('NovusList: Cannot put a non-object into a list of %s objects',
            [faclass.Classname]));
      end;
      if Test then
        FList[Index] := Item
      else
        raise EInvalidCast.Create
          (Format('NovusList: Cannot put a %s object into a list of %s objects',
          [TObject(Item).Classname, faclass.Classname]));
    end
  else
    FList[Index] := Item;
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
Var
  I: Integer;
begin
  While(FList.Count <>0) do
     Delete(FList[0]);

  FList.Clear;
  FHash.Clear;
end;

function TNovusList.FindItem(aKey: string): TObject;
Var
  FHashItem : tHashItem;
 // pIndex: Pointer;
begin
  Result := NIL;

  if fbInSensitiveKey then
     aKey := Uppercase(aKey);

  Try
    if FHash.TryGetValue(Akey,FHashItem ) then
      begin
        Result := FList.Items[FHashItem.Index];
      end;
    Except
       on Exception do
            raise EInvalidCast.Create
              (Format('NovusList: Exception FindItem %s ',
              [TNovusUtilities.GetExceptMess]));

  End;
end;

procedure TNovusList.CopyFrom(aNovusList: TNovusList);
Var
  I: Integer;
begin
  Clear;

  for I := 0 to aNovusList.Count - 1 do
    Add(aNovusList.Items[I]);
end;

function TNovusList.GetEnumerator: TlistEnumerator;
begin
  Result := FList.GetEnumerator;
end;

end.
