unit NovusTemplate;

interface

Uses NovusParser, Novuslist, Classes, SysUtils, NovusStringUtils,
     NovusStringParser;

type
  TOnGetTagValueEvent = procedure (Sender : TObject; Var ATagValue: String) of object;

  TTemplateTags = Class(tNovuslist);

  TTag = class(TCollectionItem)
  private
    fiTagIndex: INteger;
    FsRawTagEx: String;
    FsRawTag: String;
    FValues: tStringList;
    FsTagName: string;
    fsTagValue: String;
    FOnGetTagValueEvent  : TOnGetTagValueEvent;
    function GetTagValue: String;
    procedure SetTagName(const Value: string);
    procedure SetTagValue(const Value: string);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property TagName: string
       read FsTagName
       write SetTagName;

    property TagValue: string
       read GetTagValue
       write SetTagValue;

    property Values: tStringList
       read FValues
       write FValues;

    property RawTag: String
      read fsRawTag
      write fsRawTag;

    property RawTagEx: String
      read fsRawTagEx
      write fsRawTagEx;

    property OnGetTagValueEvent  : TOnGetTagValueEvent
      read FOnGetTagValueEvent
      write FOnGetTagValueEvent;

    property TagIndex: INteger
      read fiTagIndex
      write fiTagIndex;
  end;

  TTemplateTag = class(TTag)
  private
    FiSourceLineNo: Integer;
    FiSourcePos: Integer;
  protected
  public
    property SourceLineNo: Integer
       read FiSourceLineNo
       write FiSourceLineNo;

    property SourcePos: Integer
      read fiSourcePos
      write fiSourcePos;
  end;

  TTagClass = class of TTag;

  TTags = class(TCollection)
  private
    fiIndexPos: Integer;
    FOwner: TComponent;
    function GetItem(Index: Integer): TTag;
    procedure SetItem(Index: Integer; Value: TTag);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TTag;
    function AddItem(Item: TTag; Index: Integer): TTag;
    function Insert(Index: Integer): TTag;
    property Items[Index: Integer]: TTag read GetItem write SetItem; default;

    function CreateTag: TTag;
    function GetTagClass: TTagClass;
  end;

  TNovusTemplate = class(tNovusParser)
  private
  protected
    FStartToken: Char;
    FEndToken: Char;
    FSecondToken: Char;


    FTemplateTags: TTemplateTags;
    FTemplateDoc: tStringList;
    FOutputDoc: tStringlist;
    FParserStream: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadParserStream;

    procedure ParseTemplate;

    procedure InsertAllTagValues;

    function InsertOutputDoc(ATemplateTag: TTemplateTag): Boolean;
    function TagValuesAllExists:boolean;

    function FindTagNameIndexOf(ATagName: String; AIndex: Integer = 0): Integer;
    function FindNextTagNameIndexOf(ATagName: String; AIndex: Integer): Integer;

    procedure CreateTemplateTag;
    function AddTemplateTag(ATemplateTag: tTemplateTag): Integer;

    procedure InsertTemplateTag(ATemplateTag: TTemplateTag);
    procedure InsertLineNo(ALineNo: Integer; AValue: String);

    procedure ParseTagValues;
    procedure ParseTags;


    property TemplateTags: TTemplateTags
       read FTemplateTags
       write FTemplateTags;

    property TemplateDoc: tStringList
      read FTemplateDoc
      write FTemplateDoc;

    Property OutputDoc: tStringlist
      read FOutputDoc
      write FOutputDoc;

    property StartToken: Char
       read FStartToken
       write FStartToken;

    property EndToken: Char
       read FEndToken
       write FEndToken;

    property SecondToken: char
      read FSecondToken
      write FSecondToken;
  end;


implementation

// novustemplate

constructor TNovusTemplate.create;
begin
  inherited create;

  FStartToken := '{';
  FEndToken := '}';

  FTemplateTags := TTemplateTags.Create(TTemplateTag);
  FTemplateDoc :=  tStringList.Create;
  FOutputDoc := tStringlist.Create;
  FParserStream := TMemoryStream.Create;
end;

destructor TNovusTemplate.destroy;
begin
  inherited destroy;

  FOutputDoc.Free;
  FTemplateDoc.Free;
  FTemplateTags.Free;
end;

function TNovusTemplate.TagValuesAllExists:boolean;
Var
  I: integer;
  FTemplateTag: tTemplateTag;
  Count: Integer;
begin
  Result := False;

  Count := 0;
  for I := 0 to TemplateTags.Count - 1 do
    begin
      FTemplateTag := TTemplateTag(TemplateTags.items[i]);

      if FTemplateTag.TagValue <> '' then Inc(Count, 1);
    end;

  if (Count = TemplateTags.Count) then Result := True;
end;


procedure TNovusTemplate.InsertAllTagValues;
Var
  I: integer;
  FTemplateTag : ttemplateTag;
begin
  for I := 0 to TemplateTags.Count -1 do
    begin
      FTemplateTag := ttemplateTag(TemplateTags.items[i]);

      if I <= TemplateTags.Count - 2  then
        InsertOutputDoc(FTemplateTag)
      else
        InsertOutputDoc(FTemplateTag);


    end;
end;

(*
procedure TNovusTemplate.ParseLineNo(ALineNo: Integer);
begin
  Reset;

  SourceLineNo := ALineNo;

  Token := toBOF;

  while True do
    begin
      while not (Token in [toEOF,FStartToken, FSecondToken]) do
          SkipToken(FStartToken, FSecondToken);

      if Token = toEOF then
        Break;

      AddTemplateTag;

      SkipToken(FStartToken, FSecondToken);

      if SourceLineNo > ALineNo  then Break;
    end;
end;
*)
(*
procedure TNovusTemplate.ReParseTemplate(ALineNo: Integer);
Var
  LTemplateTags: tNovusList;
  LTemplateTag: tTemplateTag;
  I: Integer;
begin
  LTemplateTags := TTemplateTags.Create(TTemplateTag);

  for I := 0 to TemplateTags.Count - 1 do
    begin
      LTemplateTag := tTemplateTag(TemplateTags.Items[i]);

      LTemplateTags.Add(LTemplateTag);
    end;

  TemplateTags.Clear;

  Reset;

  while True do
    begin
      while not (Token in [toEOF,FStartToken, FSecondToken]) do
          SkipToken(FStartToken, FSecondToken);

      if Token = toEOF then
        Break;

      AddTemplateTag;

      SkipToken(FStartToken, FSecondToken);

    end;

  FOutputDoc.Clear;

  FOutputDoc.Assign(FTemplateDoc);

  LTemplateTags.Free;
end;
*)

procedure TNovusTemplate.ParseTemplate;
begin
  TemplateTags.Clear;

  LoadParserStream;

  Reset;

  while True do
    begin
      while not (Token in [toEOF,FStartToken, FSecondToken]) do
           SkipToken(FStartToken, FSecondToken);
     

      if Token = toEOF then
        Break;

      CreateTemplateTag;

      SkipToken(FStartToken, FSecondToken);
    end;

  FOutputDoc.Assign(FTemplateDoc);
end;

procedure TNovusTemplate.LoadParserStream;
begin
  FParserStream.Clear;

  FParserStream.Position := 0;

  FTemplateDoc.SaveToStream(FParserStream, TEncoding.Unicode);

  LoadFromStream(FParserStream);
end;


function TNovusTemplate.InsertOutputDoc(ATemplateTag: TTemplateTag): Boolean;
Var
  liDiffPos: INteger;
  FsInput: String;
  I, LiIndex: Integer;
  FTemplateTag: TTemplateTag;
  FNextTemplateTag: TTemplateTag;
  lsTagName:String;
begin
  Result := False;

  If Not Assigned(ATemplateTag) then Exit;


  FTemplateTag := ATemplateTag;

  FsInput := FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1];

  if FSecondToken <> #0 then
    lsTagName := FStartToken + FSecondToken + FTemplateTag.RawTag + FSecondToken + FEndToken
  else
    lsTagName := FStartToken + FTemplateTag.RawTag + FEndToken;

  if FTemplateTag.TagValue <> '' then
     FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] := TNovusStringUtils.ReplaceStrPos(FsInput, lsTagName, FTemplateTag.TagValue, FTemplateTag.SourcePos -1, true)
  else
    begin
      System.Delete(FsInput, (FTemplateTag.SourcePos), Length(lsTagName));

      FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] := FsInput;
    end;


  if Length(FTemplateTag.TagValue) > Length(lsTagName) then
    begin
      liDiffPos := (Length(FTemplateTag.TagValue) - Length(lsTagName));

      if Length(FTemplateTag.TagValue) <= Length(FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1]) then
        begin
          for I := FTemplateTag.TagIndex + 1 to TemplateTags.Count -1 do
            begin
              FNextTemplateTag := tTemplateTag(TemplateTags.Items[i]);

              if FTemplateTag.SourceLineNo = FNextTemplateTag.SourceLineNo then
                FNextTemplateTag.SourcePos := FNextTemplateTag.SourcePos + liDiffPos;
            end;
        end;
    end
  else
  if Length(FTemplateTag.TagValue) < Length(lsTagName) then
    begin
      liDiffPos := (Length(lsTagName) - Length(FTemplateTag.TagValue));

      for I := FTemplateTag.TagIndex + 1 to TemplateTags.Count -1 do
        begin
          FNextTemplateTag := tTemplateTag(TemplateTags.Items[i]);

          if FTemplateTag.SourceLineNo = FNextTemplateTag.SourceLineNo then
            FNextTemplateTag.SourcePos := FNextTemplateTag.SourcePos - liDiffPos;
        end;
    end;

end;

procedure TNovusTemplate.InsertTemplateTag(ATemplateTag: TTemplateTag);
begin
  TemplateTags.Insert(aTemplateTag, aTemplateTag.TagIndex);
end;

procedure TNovusTemplate.InsertLineNo(ALineNo: Integer; AValue: String);
Var
  LTemplateTag: TTemplateTag;
  I: Integer;
  lsRawTag : String;
begin
  FTemplateDoc.Insert(ALineNo, AValue);
  LoadParserStream;

  FoutputDoc.Insert(ALineNo, AValue);

  for I := 0 to FTemplateTags.Count - 1 do
    begin
      LTemplateTag := tTemplateTag(FTemplateTags.Items[i]);

      if LTemplateTag.SourceLineNo > ALineNo then
        LTemplateTag.SourceLineNo := LTemplateTag.SourceLineNo + 1;
    end;

end;


procedure TNovusTemplate.CreateTemplateTag;
var
  FTemplateTag: TTemplateTag;
  fsTag: String;
begin
  FTemplateTag := TTemplateTag.Create(NIL);

  FTemplateTag.SourceLineNo := SourceLineNo;

  FTemplateTag.SourcePos := SourcePos;

  If FEndToken <> #0 then
    fsTag := SkipToToken(FEndToken, FSecondToken)
  else
    fsTag := SkipToNonAlpha;

  FTemplateTag.TagName := fsTag;
  FTemplateTag.RawTag := FTemplateTag.TagName;

  if FSecondToken = #0 then
    FTemplateTag.RawTagEx :=  FStartToken + FTemplateTag.TagName + FEndToken
  else
    FTemplateTag.RawTagEx :=  FStartToken + FSecondToken + FTemplateTag.TagName + FSecondToken + FEndToken;

  AddTemplateTag(FTemplateTag);
end;

function TNovusTemplate.AddTemplateTag;
begin
  ATemplateTag.TagIndex := TemplateTags.Count;

  TemplateTags.Add(ATemplateTag);

  Result := ATemplateTag.TagIndex;
end;

procedure TNovusTemplate.ParseTagValues;
Var
  I, X: Integer;
  FTemplateTag: TTemplateTag;
  fsTagName: String;
  FStrParser: tNovusStringParser;
begin
  for I := 0 to TemplateTags.Count -1 do
    begin
      FTemplateTag := TTemplateTag(TemplateTags.items[i]);

      FStrParser := tNovusStringParser.Create(FTemplateTag.RawTag);

      FTemplateTag.TagName := FStrParser.Items[0];

      For X := 1 to FStrParser.WordCount -1 do
        FTemplateTag.Values.Add(FStrParser.Items[x]);

      FStrParser.Free;
    end;
end;


procedure TNovusTemplate.ParseTags;
Var
  FTemplateTag: TTemplateTag;
  I: Integer;
  fsInput: String;
begin
  For I := 0 to TemplateTags.Count -1 do
    begin
      FTemplateTag := TTemplateTag(TemplateTags.items[i]);

      FsInput := FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1];

      FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] := TNovusStringUtils.ReplaceStrPos(FsInput, '{' + FTemplateTag.TagName + '}', FTemplateTag.TagValue, FTemplateTag.SourcePos -1);
    end;
end;


function TNovusTemplate.FindNextTagNameIndexOf(ATagName: String; AIndex: Integer): Integer;
begin
  Result := FindTagNameIndexOf(ATagName, AIndex);
end;

function TNovusTemplate.FindTagNameIndexOf(ATagName: String; AIndex: Integer = 0): Integer;
Var
  I: integer;
begin
  Result := -1;

  for I := AIndex to TemplateTags.Count -1 do
    begin
      if Trim(uppercase(TTemplateTag(TemplateTags.items[i]).TagName)) = Trim(Uppercase(ATagName)) then
        begin
          Result := i;

          break;
        end;
    end;


end;

// TTag

procedure TTag.SetTagName(const Value: string);
begin
  if FsTagName <> Value then
  begin
    FsTagName := Value;
    Changed(False);
  end;
end;

function TTag.GetTagValue: String;
begin
  if Assigned(OnGetTagValueEvent) then
    OnGetTagValueEvent(Self, fsTagValue);

  Result := fsTagValue;
end;

procedure TTag.SetTagValue(const Value: string);
begin
  if FsTagValue <> Value then
  begin
    FsTagValue := Value;
    Changed(False);
  end;
end;

constructor TTag.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  fValues := tStringList.Create;

  fsTagName := '';
  fsTagValue := '';
end;

destructor TTag.Destroy;
begin
  fValues.Free;

  inherited;
end;

procedure TTag.Assign(Source: TPersistent);
begin
  if Source is TTag then
  begin
    TagName := TTag(Source).TagName;
    TagValue := TTag(Source).TagValue;
  end
  else
  inherited Assign(Source);
end;

// TTags

constructor TTags.Create;
begin
  inherited Create(TTag);

  FOwner := AOwner;

end;

function TTags.GetItem(Index: Integer): TTag;
begin
  Result := TTag(inherited GetItem(Index));
end;

procedure TTags.SetItem(Index: Integer; Value: TTag);
begin
  inherited SetItem(Index, Value);
end;

function TTags.Add: TTag;
begin
  Result := TTag(inherited Add);
end;

function TTags.AddItem(Item: TTag; Index: Integer): TTag;
begin
  if Item = nil then
    Result := CreateTag
  else
    Result := Item;
  if Assigned(Result) then
  begin
    Result.Collection := Self;
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;

function TTags.Insert(Index: Integer): TTag;
begin
  Result := AddItem(nil, Index);
end;

function TTags.CreateTag: TTag;
var
  LClass: TTagClass;
begin
  Result := TTag.Create(Self);
end;

function TTags.GetTagClass: TTagClass;
begin
  Result := TTag;
end;

function TTags.GetOwner: TPersistent;
begin
  Result := FOwner;
end;







end.




