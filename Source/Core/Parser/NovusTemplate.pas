unit NovusTemplate;

interface

Uses NovusParser, Novuslist, Classes, SysUtils, NovusStringUtils,
  NovusStringParser;

type
  TOnGetTagValueEvent = procedure(Sender: TObject; Var ATagValue: String)
    of object;

  TTemplateLineInfo = class(Tobject)
  private
  protected
    fiLineNo: Integer;
  public
    property LineNo: Integer read fiLineNo write fiLineNo;
  end;

  TTemplateTags = Class(tNovuslist)
  private
  protected
  public
    /// <summary>
    /// Find Tagname from TTemplateTags class
    /// </summary>
    function FindTagNameIndexOf(ATagName: String; AIndex: Integer = 0): Integer;
  End;

  TTemplateTag = class
  private
    FiSourceLineNo: Integer;
    FiSourcePos: Integer;
    fiTagIndex: Integer;
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
    constructor Create; virtual;
    destructor Destroy; virtual;

    procedure Assign(Source: TTemplateTag);
  published
    property TagName: string read FsTagName write SetTagName;

    property TagValue: string read GetTagValue write SetTagValue;

    property Values: tStringList read FValues write FValues;

    property RawTag: String read FsRawTag write FsRawTag;

    property RawTagEx: String read FsRawTagEx write FsRawTagEx;

    property OnGetTagValueEvent: TOnGetTagValueEvent read FOnGetTagValueEvent
      write FOnGetTagValueEvent;
    property TagIndex: Integer read fiTagIndex write fiTagIndex;
    property SourceLineNo: Integer read FiSourceLineNo write FiSourceLineNo;
    property SourcePos: Integer read FiSourcePos write FiSourcePos;
  end;

  TNovusTemplate = class(tNovusParser)
  private
  protected
    FStartToken: Char;
    FEndToken: Char;
    FSecondToken: Char;
    fbSwapTagNameBlankValue: boolean;
    FTemplateTags: TTemplateTags;
    FTemplateDoc: tStringList;
    FOutputDoc: tStringList;
    fsLastMessage: String;



    function InternalParseTemplate(aLoadParserStream: boolean = true): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ParseTemplate(aLoadParserStream: boolean = true): boolean; virtual;

    procedure LoadTemplateDoc;

    procedure InsertAllTagValues;
//    procedure Add(aLine: String);

    function InsertOutputDoc(ATemplateTag: TTemplateTag): boolean;
    function TagValuesAllExists: boolean;

    function FindTagNameIndexOf(ATagName: String; AIndex: Integer = 0): Integer;
    function FindNextTagNameIndexOf(ATagName: String; AIndex: Integer): Integer;

    procedure CreateTemplateTag;
    function AddTemplateTag(ATemplateTag: TTemplateTag): Integer;

    procedure InsertTemplateTag(ATemplateTag: TTemplateTag);
    procedure InsertLineNo(ALineNo: Integer; AValue: String);

    procedure ParseTagValues;
    procedure ParseTags;

    property TemplateTags: TTemplateTags read FTemplateTags write FTemplateTags;

    property TemplateDoc: tStringList read FTemplateDoc write FTemplateDoc;

    Property OutputDoc: tStringList read FOutputDoc write FOutputDoc;

    property StartToken: Char read FStartToken write FStartToken;

    property EndToken: Char read FEndToken write FEndToken;

    property SecondToken: Char read FSecondToken write FSecondToken;

    property SwapTagNameBlankValue: boolean read fbSwapTagNameBlankValue
      write fbSwapTagNameBlankValue;

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

  SwapTagNameBlankValue := False;
end;

destructor TNovusTemplate.Destroy;
begin
  inherited Destroy;

  TNovusUtilities.ClearStringList(FOutputDoc);
  FOutputDoc.Free;

  TNovusUtilities.ClearStringList(FTemplateDoc);
  FTemplateDoc.Free;

  FTemplateTags.Free;
end;

function TNovusTemplate.TagValuesAllExists: boolean;
Var
  I: Integer;
  FTemplateTag: TTemplateTag;
  Count: Integer;
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
    Result := true;
end;

(*
  function TNovusTemplate.Execute: Boolean;
  begin
  Result := InternalParseTemplate;
  end;
*)

function TNovusTemplate.ParseTemplate(aLoadParserStream
  : boolean = true): boolean;
begin
  Result := InternalParseTemplate(aLoadParserStream);
end;

(*
procedure TNovusTemplate.Add(aLine: String);
begin
  TemplateDoc.Add(aLine)
end;
*)

procedure TNovusTemplate.InsertAllTagValues;
Var
  I: Integer;
  FTemplateTag: TTemplateTag;
begin
  if TemplateTags.Count = 0 then
  begin
    //FOutputDoc.Text := FTemplateDoc.Text;
    TNovusUtilities.CloneStringList(FOutputDoc, FTemplateDoc);

    Exit;
  end;

  for I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TTemplateTag(TemplateTags.Items[I]);

    if I <= TemplateTags.Count - 2 then
      InsertOutputDoc(FTemplateTag)
    else
      InsertOutputDoc(FTemplateTag);

  end;
end;

function TNovusTemplate.InternalParseTemplate(aLoadParserStream
  : boolean = true): boolean;
begin
  Try
    TemplateTags.Clear;

    if aLoadParserStream then
      LoadTemplateDoc;

    Reset;

    while true do
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


    //FOutputDoc.Assign(FTemplateDoc);
    TNovusUtilities.CloneStringList(FTemplateDoc, FOutputDoc);

    Result := true;
  Except
    fsLastMessage := TNovusUtilities.GetExceptMess;
    Result := False;
  End;

end;

procedure TNovusTemplate.LoadTemplateDoc;
Var
  liLineNo: Integer;
begin
  TemplateDoc.Clear;

  for liLineNo := 0 to ParseStringList.Count -1 do
    begin
      var lTemplateLineInfo := TTemplateLineInfo.Create;

      lTemplateLineInfo.LineNo := liLineNo;

      TemplateDoc.AddObject(ParseStringList.Strings[liLineNo], lTemplateLineInfo);
    end;

  (*
  FParserStream.Clear;

  FParserStream.Position := 0;

  FTemplateDoc.SaveToStream(FParserStream, TEncoding.Unicode);

  LoadFromStream(FParserStream);
  *)
end;

function TNovusTemplate.InsertOutputDoc(ATemplateTag: TTemplateTag): boolean;
Var
  liDiffPos: Integer;
  FsInput: String;
  I, LiIndex: Integer;
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
      FTemplateTag.SourcePos - 1, true)
  else
  begin
    if (SwapTagNameBlankValue = true) and (FTemplateTag.TagValue = '') then
    begin
      System.Delete(FsInput, (FTemplateTag.SourcePos), Length(lsTagName));

      FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] := FsInput;
    end
    else
      FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] := '';
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

procedure TNovusTemplate.InsertLineNo(ALineNo: Integer; AValue: String);
Var
  LTemplateTag: TTemplateTag;
  I: Integer;
  lsRawTag: String;
begin
  FTemplateDoc.Insert(ALineNo, AValue);
  LoadTemplateDoc;

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
  FTemplateTag := TTemplateTag.Create;

  FTemplateTag.SourceLineNo := SourceLineNo;

  FTemplateTag.SourcePos := GetSourcePos;

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
  I, X: Integer;
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
  I: Integer;
  FsInput: String;
begin
  For I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TTemplateTag(TemplateTags.Items[I]);

    FsInput := FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1];

    FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] :=
      TNovusStringUtils.ReplaceStrPos(FsInput, StartToken + FTemplateTag.TagName
      + EndToken, FTemplateTag.TagValue, FTemplateTag.SourcePos - 1);
  end;
end;

function TNovusTemplate.FindNextTagNameIndexOf(ATagName: String;
  AIndex: Integer): Integer;
begin
  Result := FindTagNameIndexOf(ATagName, AIndex);
end;

function TNovusTemplate.FindTagNameIndexOf(ATagName: String;
  AIndex: Integer = 0): Integer;
Var
  I: Integer;
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

// TTemplateTag

procedure TTemplateTag.SetTagName(const Value: string);
begin
  if FsTagName <> Value then
  begin
    FsTagName := Value;
  end;
end;

function TTemplateTag.GetTagValue: String;
begin
  if Assigned(OnGetTagValueEvent) then
    OnGetTagValueEvent(Self, fsTagValue);

  Result := fsTagValue;
end;

procedure TTemplateTag.SetTagValue(const Value: string);
begin
  if fsTagValue <> Value then
  begin
    fsTagValue := Value;
  end;
end;

constructor TTemplateTag.Create;
begin
  FValues := tStringList.Create;

  FsTagName := '';
  fsTagValue := '';
end;

destructor TTemplateTag.Destroy;
begin
  FValues.Free;

  inherited;
end;

procedure TTemplateTag.Assign(Source: TTemplateTag);
begin
  if Source is TTemplateTag then
  begin
    TagName := TTemplateTag(Source).TagName;
    TagValue := TTemplateTag(Source).TagValue;
  end
end;

function TTemplateTags.FindTagNameIndexOf(ATagName: String;
  AIndex: Integer = 0): Integer;
Var
  I: Integer;
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
