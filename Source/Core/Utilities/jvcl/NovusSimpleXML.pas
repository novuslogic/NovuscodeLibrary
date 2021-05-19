unit NovusSimpleXML;

interface

Uses NovusUtilities, JvSimpleXML, SysUtils, classes{, dialogs};

Type
  TNovusSimpleXML = Class(TNovusUtilities)
  private
  protected
  public
    class function HasProperties(aNode: TJvSimpleXmlElem;
      aName: string): String;
    class function FindNodeByValue(aNodeList: TJvSimpleXmlElem;
      NodeName: String; NodeValueName, NodeValue: String): TJvSimpleXmlElem;
    class function FindNode(aNode: TJvSimpleXmlElem; NodeName: String;
      Var Index: Integer): TJvSimpleXmlElem;
    class procedure ListNodeNames(aNodeList: TJvSimpleXmlElem;
      Var aStringList: tStringList);
  end;

implementation

class function TNovusSimpleXML.HasProperties(aNode: TJvSimpleXmlElem;
  aName: string): string;
begin
  Result := '';
  if not Assigned(aNode) then
    Exit;
  if not aNode.HasProperties then
    Exit;
  try
    Result := aNode.Properties.ItemNamed[aName].Value;
  except
    Result := '';
  end;
end;

class function TNovusSimpleXML.FindNodeByValue(aNodeList: TJvSimpleXmlElem;
  NodeName: String; NodeValueName, NodeValue: String): TJvSimpleXmlElem;
Var
  fJvSimpleXmlElem: TJvSimpleXmlElem;
  I, Index: Integer;
begin
  Result := NIL;

  Index := 0;

  fJvSimpleXmlElem := FindNode(aNodeList, NodeName, Index);
  While (fJvSimpleXmlElem <> NIL) do
  begin
    For I := 0 to fJvSimpleXmlElem.Properties.Count - 1 do
    begin
      If Uppercase(fJvSimpleXmlElem.Properties[I].Name)
        = Uppercase(NodeValueName) then
      begin
        If Uppercase(fJvSimpleXmlElem.Properties[I].Value) = Uppercase(NodeValue)
        then
        begin
          Result := fJvSimpleXmlElem;

          Exit;
        end;
      end;
    end;

    fJvSimpleXmlElem := FindNode(aNodeList, NodeName, Index);
  end;
end;

class procedure TNovusSimpleXML.ListNodeNames(aNodeList: TJvSimpleXmlElem;
  Var aStringList: tStringList);
Var
  I: Integer;
begin
  aStringList.Clear;

  For I := 0 to aNodeList.Items.Count - 1 do
    aStringList.Add(aNodeList.Items[I].Name);
end;

class function TNovusSimpleXML.FindNode(aNode: TJvSimpleXmlElem;
  NodeName: String; Var Index: Integer): TJvSimpleXmlElem;
Var
  I: Integer;
begin
  Result := NIL;

  If Uppercase(aNode.Name) = Uppercase(NodeName) then
  begin
    Result := aNode;
    Exit;
  end;

  For I := Index to aNode.Items.Count - 1 do
  begin
   // If Uppercase(aNode.Items[I].Name) = Uppercase(NodeName) then
    if SameText(Uppercase(aNode.Items[I].Name),Uppercase(NodeName)) then
    begin
      Result := aNode.Items[I];

      Index := I + 1;

      Break;
    end;
  end;
end;

end.
