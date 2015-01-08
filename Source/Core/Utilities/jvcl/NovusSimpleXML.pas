unit NovusSimpleXML;

interface

Uses NovusUtilities, JvSimpleXML, SysUtils, classes;

Type
  TNovusSimpleXML = Class(TNovusUtilities)
  private
  protected
  public
    class function FindNodeByValue(aNodeList: TJvSimpleXmlElem; NodeName: String; NodeValueName, NodeValue: String): TJvSimpleXmlElem;
    class function FindNode(aNodeList: TJvSimpleXmlElem; NodeName: String; Var Index: Integer): TJvSimpleXmlElem;
    class procedure ListNodeNames(aNodeList: TJvSimpleXmlElem; Var aStringList: tStringList);
  end;

implementation

class function TNovusSimpleXML.FindNodeByValue(aNodeList: TJvSimpleXmlElem; NodeName: String; NodeValueName, NodeValue: String): TJvSimpleXmlElem;
Var
  fJvSimpleXmlElem: TJvSimpleXmlElem;
  I, Index: Integer;
begin
  Result := NIL;

  Index := 0;

  fJvSimpleXmlElem := FindNode(aNodeList, NodeName,Index);
  While(fJvSimpleXmlElem <> NIL) do
    begin
      For I := 0 to fJvSimpleXmlElem.Properties.Count-1 do
        begin
          If Uppercase(fJvSimpleXmlElem.Properties[I].Name) = uppercase(NodeValueName) then
            begin
              If Uppercase(fJvSimpleXmlElem.Properties[I].Value) = Uppercase(NodeValue) then
                begin
                  Result := fJvSimpleXmlElem;

                  Exit;
                end;
            end;
        end;

      fJvSimpleXmlElem := FindNode(aNodeList, NodeName,Index);
    end;
end;

class procedure TNovusSimpleXML.ListNodeNames(aNodeList: TJvSimpleXmlElem; Var aStringList: tStringList);
Var
  I: integer;
begin
  aStringList.Clear;

  For I := 0 to aNodeList.Items.count -1 do
    astringList.Add(aNodeList.Items[i].Name);
end;


class function TNovusSimpleXML.FindNode(aNodeList: TJvSimpleXmlElem; NodeName: String; Var Index: Integer): TJvSimpleXmlElem;
Var
  I: integer;
begin
  Result := NIL;

  For I := Index to aNodeList.Items.count -1 do
     begin
       If Uppercase(aNodeList.Items[i].Name) = Uppercase(NodeName) then
         begin
           Result := aNodeLIst.Items[I];

           Index := i + 1;

           Break;
         end;
     end;
end;


end.




