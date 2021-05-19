unit NovusTemplate;

interface

Uses NovusParser, Novuslist, Classes, SysUtils, NovusStringUtils,
   NovusStringParser;

type
  TOnGetTagValueEvent = procedure(Sender: TObject; Var ATagValue: String)
    of object;

  TTemplateTags = Class(tNovuslist)
  private
  protected
  public
    /// <summary>
    ///   Find Tagname from TTemplateTags class
    /// </summary>
    function FindTagNameIndexOf(ATagName: String; AIndex: Integer = 0): Integer;
  End;

  TTag = class(TCollectionItem)
  private
    fiTagIndex: INteger;
    FsRawTagEx: String;
    FsRawTag: String;
    FValues: tStringList;
    FsTagName: string;
    fsTagValue: String;
    FOnGetTagValueEvent: TOnGetTagValueEvent;
    function GetTagValue: String;
    procedure SetTagName(const Value: string);
    procedure SetTagValue(const Value: string);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property TagName: string read FsTagName write SetTagName;

    property TagValue: string read GetTagValue write SetTagValue;

    property Values: tStringList read FValues write FValues;

    property RawTag: String read FsRawTag write FsRawTag;

    property RawTagEx: String read FsRawTagEx write FsRawTagEx;

    property OnGetTagValueEvent: TOnGetTagValueEvent read FOnGetTagValueEvent
      write FOnGetTagValueEvent;

    property TagIndex: INteger read fiTagIndex write fiTagIndex;
  end;

  TTemplateTag = class(TTag)
  private
    FiSourceLineNo: INteger;
    FiSourcePos: INteger;
  protected
  public
    property SourceLineNo: INteger read FiSourceLineNo write FiSourceLineNo;

    property SourcePos: INteger read FiSourcePos write FiSourcePos;
  end;

  TTagClass = class of TTag;

  TTags = class(TCollection)
  private
    fiIndexPos: INteger;
    FOwner: TComponent;
    function GetItem(Index: INteger): TTag;
    procedure SetItem(Index: INteger; Value: TTag);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TTag;
    function AddItem(Item: TTag; Index: INteger): TTag;
    function Insert(Index: INteger): TTag;
    property Items[Index: INteger]: TTag read GetItem write SetItem; default;

    function CreateTag: TTag;
    function GetTagClass: TTagClass;
  end;

  TNovusTemplate = class(tNovusParser)
  private
  protected
    FStartToken: Char;
    FEndToken: Char;
    FSecondToken: Char;
    fbIgnoreBlankValue: boolean;
    FTemplateTags: TTemplateTags;
    FTemplateDoc: tStringList;
    FOutputDoc: tStringList;
    FParserStream: TMemoryStream;
    fsLastMessage: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadParserStream;

    function ParseTemplate : Boolean;

    procedure InsertAllTagValues;

    function InsertOutputDoc(ATemplateTag: TTemplateTag): Boolean;
    function TagValuesAllExists: Boolean;

    function FindTagNameIndexOf(ATagName: String; AIndex: INteger = 0): Integer;
    function FindNextTagNameIndexOf(ATagName: String; AIndex: INteger): Integer;

    procedure CreateTemplateTag;
    function AddTemplateTag(ATemplateTag: TTemplateTag): INteger;

    procedure InsertTemplateTag(ATemplateTag: TTemplateTag);
    procedure InsertLineNo(ALineNo: INteger; AValue: String);

    procedure ParseTagValues;
    procedure ParseTags;

    property TemplateTags: TTemplateTags read FTemplateTags write FTemplateTags;

    property TemplateDoc: tStringList read FTemplateDoc write FTemplateDoc;

    Property OutputDoc: tStringList read FOutputDoc write FOutputDoc;

    property StartToken: Char read FStartToken write FStartToken;

    property EndToken: Char read FEndToken write FEndToken;

    property SecondToken: Char read FSecondToken write FSecondToken;

    property IgnoreBlankValue: Boolean read fbIgnoreBlankValue write fbIgnoreBlankValue;

    property LastMessage: String read fsLastMessage write fsLastMessage;
  end;

implementation

Uses NovusUtilities;

// novustemplate

constructor TNovusTemplate.Create;
begin
  inherited Create;

  FStartToken := '{';
  FEndToken := '}';

  FTemplateTags := TTemplateTags.Create(TTemplateTag);
  FTemplateDoc := tStringList.Create;
  FOutputDoc := tStringList.Create;
  FParserStream := TMemoryStream.Create;

  IgnoreBlankValue := False;
end;

destructor TNovusTemplate.Destroy;
begin
  inherited Destroy;

  FParserStream.Free;
  FOutputDoc.Free;
  FTemplateDoc.Free;
  FTemplateTags.Free;
end;

function TNovusTemplate.TagValuesAllExists: Boolean;
Var
  I: INteger;
  FTemplateTag: TTemplateTag;
  Count: INteger;
begin
  Result := False;

  Count := 0;
  for I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TTemplateTag(TemplateTags.Items[I]);

    if FTemplateTag.TagValue <> '' then
      Inc(Count, 1);
  end;

  if (Count = TemplateTags.Count) then
    Result := True;
end;

procedure TNovusTemplate.InsertAllTagValues;
Var
  I: INteger;
  FTemplateTag: TTemplateTag;
begin
  if TemplateTags.Count = 0 then
    begin
      FOutputDoc.Text := FTemplateDoc.Text;

      Exit;
    end;

  for I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TTemplateTag(TemplateTags.Items[I]);

    if (IgnoreBlankValue = True) and (FTemplateTag.TagValue = '') then
      Continue;

    if I <= TemplateTags.Count - 2 then
      InsertOutputDoc(FTemplateTag)
    else
      InsertOutputDoc(FTemplateTag);

  end;
end;

function TNovusTemplate.ParseTemplate: boolean;
begin
  Try
    TemplateTags.Clear;

    LoadParserStream;

    Reset;

    while True do
    begin
      while not(Token in [toEOF, FStartToken, FSecondToken]) do
        SkipToken(FStartToken, FSecondToken);

      if Token = toEOF then
        Break;

      if (FSecondToken = #0) then
        CreateTemplateTag
      else if GetLastToken(-1) = FStartToken then
        CreateTemplateTag;

      SkipToken(FStartToken, FSecondToken);
    end;

    FOutputDoc.Assign(FTemplateDoc);

    Result := True;
  Except
    fsLastMessage := TNovusUtilities.GetExceptMess;
    Result := False;
  End;

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
  I, LiIndex: INteger;
  FTemplateTag: TTemplateTag;
  FNextTemplateTag: TTemplateTag;
  lsTagName: String;
begin
  Result := False;

  If Not Assigned(ATemplateTag) then
    Exit;

  FTemplateTag := ATemplateTag;

  FsInput := FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1];

  if FSecondToken <> #0 then
    lsTagName := FStartToken + FSecondToken + FTemplateTag.RawTag + FSecondToken
      + FEndToken
  else
    lsTagName := FStartToken + FTemplateTag.RawTag + FEndToken;

  if FTemplateTag.TagValue <> '' then
    FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] :=
      TNovusStringUtils.ReplaceStrPos(FsInput, lsTagName, FTemplateTag.TagValue,
      FTemplateTag.SourcePos - 1, True)
  else
  begin
    System.Delete(FsInput, (FTemplateTag.SourcePos), Length(lsTagName));

    FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] := FsInput;
  end;

  if Length(FTemplateTag.TagValue) > Length(lsTagName) then
  begin
    liDiffPos := (Length(FTemplateTag.TagValue) - Length(lsTagName));

    if Length(FTemplateTag.TagValue) <=
      Length(FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1]) then
    begin
      for I := FTemplateTag.TagIndex + 1 to TemplateTags.Count - 1 do
      begin
        FNextTemplateTag := TTemplateTag(TemplateTags.Items[I]);

        if FTemplateTag.SourceLineNo = FNextTemplateTag.SourceLineNo then
          FNextTemplateTag.SourcePos := FNextTemplateTag.SourcePos + liDiffPos;
      end;
    end;
  end
  else if Length(FTemplateTag.TagValue) < Length(lsTagName) then
  begin
    liDiffPos := (Length(lsTagName) - Length(FTemplateTag.TagValue));

    for I := FTemplateTag.TagIndex + 1 to TemplateTags.Count - 1 do
    begin
      FNextTemplateTag := TTemplateTag(TemplateTags.Items[I]);

      if FTemplateTag.SourceLineNo = FNextTemplateTag.SourceLineNo then
        FNextTemplateTag.SourcePos := FNextTemplateTag.SourcePos - liDiffPos;
    end;
  end;

end;

procedure TNovusTemplate.InsertTemplateTag(ATemplateTag: TTemplateTag);
begin
  TemplateTags.Insert(ATemplateTag, ATemplateTag.TagIndex);
end;

procedure TNovusTemplate.InsertLineNo(ALineNo: INteger; AValue: String);
Var
  LTemplateTag: TTemplateTag;
  I: INteger;
  lsRawTag: String;
begin
  FTemplateDoc.Insert(ALineNo, AValue);
  LoadParserStream;

  FOutputDoc.Insert(ALineNo, AValue);

  for I := 0 to FTemplateTags.Count - 1 do
  begin
    LTemplateTag := TTemplateTag(FTemplateTags.Items[I]);

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
    FTemplateTag.RawTagEx := FStartToken + FTemplateTag.TagName + FEndToken
  else
    FTemplateTag.RawTagEx := FStartToken + FSecondToken + FTemplateTag.TagName +
      FSecondToken + FEndToken;

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
  I, X: INteger;
  FTemplateTag: TTemplateTag;
  FsTagName: String;
  FStrParser: tNovusStringParser;
begin
  for I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TTemplateTag(TemplateTags.Items[I]);

    FStrParser := tNovusStringParser.Create(FTemplateTag.RawTag);

    FTemplateTag.TagName := FStrParser.Items[0];

    For X := 1 to FStrParser.Count - 1 do
      FTemplateTag.Values.Add(FStrParser.Items[X]);

    FStrParser.Free;
  end;
end;

procedure TNovusTemplate.ParseTags;
Var
  FTemplateTag: TTemplateTag;
  I: INteger;
  FsInput: String;
begin
  For I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TTemplateTag(TemplateTags.Items[I]);

    FsInput := FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1];

    FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] :=
      TNovusStringUtils.ReplaceStrPos(FsInput, '{' + FTemplateTag.TagName + '}',
      FTemplateTag.TagValue, FTemplateTag.SourcePos - 1);
  end;
end;

function TNovusTemplate.FindNextTagNameIndexOf(ATagName: String;
  AIndex: INteger): INteger;
begin
  Result := FindTagNameIndexOf(ATagName, AIndex);
end;

function TNovusTemplate.FindTagNameIndexOf(ATagName: String;
  AIndex: integer = 0): INteger;
Var
  I: INteger;
begin
  Result := -1;

  for I := AIndex to TemplateTags.Count - 1 do
  begin
    if Trim(uppercase(TTemplateTag(TemplateTags.Items[I]).TagName))
      = Trim(uppercase(ATagName)) then
    begin
      Result := I;

      Break;
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
  if fsTagValue <> Value then
  begin
    fsTagValue := Value;
    Changed(False);
  end;
end;

constructor TTag.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FValues := tStringList.Create;

  FsTagName := '';
  fsTagValue := '';
end;

destructor TTag.Destroy;
begin
  FValues.Free;

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

function TTags.GetItem(Index: INteger): TTag;
begin
  Result := TTag(inherited GetItem(Index));
end;

procedure TTags.SetItem(Index: INteger; Value: TTag);
begin
  inherited SetItem(Index, Value);
end;

function TTags.Add: TTag;
begin
  Result := TTag(inherited Add);
end;

function TTags.AddItem(Item: TTag; Index: INteger): TTag;
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

function TTags.Insert(Index: INteger): TTag;
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


function TTemplateTags.FindTagNameIndexOf(ATagName: String;
  AIndex: integer = 0): INteger;
Var
  I: INteger;
begin
  Result := -1;

  for I := AIndex to Self.Count - 1 do
  begin
    if Trim(uppercase(TTemplateTag(Self.Items[I]).TagName))
      = Trim(uppercase(ATagName)) then
    begin
      Result := I;

      Break;
    end;
  end;

end;

end.
