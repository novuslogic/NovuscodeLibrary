unit NovusTemplate2;

interface

Uses NovusParser, Novuslist, Classes, SysUtils, NovusStringUtils,
  NovusStringParser, NovusParser.Common;

type
  TNovusTemplate2 = class(tNovusParser)
  private
  protected
    FStartToken: Char;
    FEndToken: Char;
    FSecondToken: Char;
    fbSwapTagNameBlankValue: boolean;
    FTemplateTags: TNovusTemplateTags;
    FTemplateDoc: tStringList;
    FOutputDoc: tStringList;
    fsLastMessage: String;

    function InternalParseTemplate(aLoadParserStream: boolean = true): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ParseTemplate(aLoadParserStream: boolean = true): boolean; virtual;

    function LoadTemplateDocFile(afilename: String): boolean;

    procedure LoadTemplateDoc;

    procedure InsertAllTagValues;

    procedure AddLine(aString: string);

    function InsertOutputDoc(ATemplateTag: TNovusTemplateTag): boolean;
    function TagValuesAllExists: boolean;

    function FindTagNameIndexOf(ATagName: String; AIndex: Integer = 0): Integer;
    function FindNextTagNameIndexOf(ATagName: String; AIndex: Integer): Integer;

    procedure CreateTemplateTag;
    function AddTemplateTag(ATemplateTag: TNovusTemplateTag): Integer;

    procedure InsertTemplateTag(ATemplateTag: TNovusTemplateTag);
    procedure InsertLineNo(ALineNo: Integer; AValue: String);

    procedure ParseTagValues;
    procedure ParseTags;

    property TemplateTags: TNovusTemplateTags read FTemplateTags write FTemplateTags;

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

// novustemplate2
constructor TNovusTemplate2.Create;
begin
  inherited Create;

  FStartToken := '{';
  FEndToken := '}';

  FTemplateTags := TNovusTemplateTags.Create(TNovusTemplateTag);
  FTemplateDoc := tStringList.Create;
  FOutputDoc := tStringList.Create;

  SwapTagNameBlankValue := False;
end;

destructor TNovusTemplate2.Destroy;
begin
  inherited Destroy;

  TNovusUtilities.ClearStringList(FOutputDoc);
  FOutputDoc.Free;

  TNovusUtilities.ClearStringList(FTemplateDoc);
  FTemplateDoc.Free;

  FTemplateTags.Free;
end;

function TNovusTemplate2.TagValuesAllExists: boolean;
Var
  I: Integer;
  FTemplateTag: TNovusTemplateTag;
  Count: Integer;
begin
  Result := False;

  Count := 0;
  for I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TNovusTemplateTag(TemplateTags.Items[I]);

    if FTemplateTag.TagValue <> '' then
      Inc(Count, 1);
  end;

  if (Count = TemplateTags.Count) then
    Result := true;
end;

function TNovusTemplate2.ParseTemplate(aLoadParserStream
  : boolean = true): boolean;
begin
  Result := InternalParseTemplate(aLoadParserStream);
end;


function TNovusTemplate2.LoadTemplateDocFile(afilename: String): boolean;
begin
  Result := LoadFromFile(afilename);

  if Result then LoadTemplateDoc;
end;


procedure TNovusTemplate2.AddLine(aString: string);
begin
  Add(aString);

  var lTemplateLineInfo := TNovusTemplateLineInfo.Create;

  lTemplateLineInfo.LineNo :=ParseStringList.Count-1;

  TemplateDoc.AddObject(aString, lTemplateLineInfo);
end;


procedure TNovusTemplate2.InsertAllTagValues;
Var
  I: Integer;
  FTemplateTag: TNovusTemplateTag;
begin
  if TemplateTags.Count = 0 then
  begin
    TNovusUtilities.CloneStringList(FOutputDoc, FTemplateDoc);

    // Strip out #0
    FOutputDoc.Text :=  TNovusStringUtils.StripChar(FOutputDoc.Text, #0);

    Exit;
  end;

  for I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TNovusTemplateTag(TemplateTags.Items[I]);

    if I <= TemplateTags.Count - 2 then
      InsertOutputDoc(FTemplateTag)
    else
      InsertOutputDoc(FTemplateTag);

  end;

  // Strip out #0
  FOutputDoc.Text :=  TNovusStringUtils.StripChar(FOutputDoc.Text, #0);
end;

function TNovusTemplate2.InternalParseTemplate(aLoadParserStream
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

    TNovusUtilities.CloneStringList(FTemplateDoc, FOutputDoc);

    Result := true;
  Except
    fsLastMessage := TNovusUtilities.GetExceptMess;
    Result := False;
  End;

end;

procedure TNovusTemplate2.LoadTemplateDoc;
Var
  liLineNo: Integer;
begin
  TemplateDoc.Clear;

  for liLineNo := 0 to ParseStringList.Count -1 do
    begin
      var lTemplateLineInfo := TNovusTemplateLineInfo.Create;

      lTemplateLineInfo.LineNo := liLineNo;

      TemplateDoc.AddObject(ParseStringList.Strings[liLineNo], lTemplateLineInfo);
    end;
end;

function TNovusTemplate2.InsertOutputDoc(ATemplateTag: TNovusTemplateTag): boolean;
Var
  liDiffPos: Integer;
  FsInput: String;
  I, LiIndex: Integer;
  FTemplateTag: TNovusTemplateTag;
  FNextTemplateTag: TNovusTemplateTag;
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
        FNextTemplateTag := TNovusTemplateTag(TemplateTags.Items[I]);

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
      FNextTemplateTag := TNOvusTemplateTag(TemplateTags.Items[I]);

      if FTemplateTag.SourceLineNo = FNextTemplateTag.SourceLineNo then
        FNextTemplateTag.SourcePos := FNextTemplateTag.SourcePos - liDiffPos;
    end;
  end;

end;

procedure TNovusTemplate2.InsertTemplateTag(ATemplateTag: TNovusTemplateTag);
begin
  TemplateTags.Insert(ATemplateTag, ATemplateTag.TagIndex);
end;

procedure TNovusTemplate2.InsertLineNo(ALineNo: Integer; AValue: String);
Var
  LTemplateTag: TNovusTemplateTag;
  I: Integer;
  lsRawTag: String;
begin
  FTemplateDoc.Insert(ALineNo, AValue);
  LoadTemplateDoc;

  FOutputDoc.Insert(ALineNo, AValue);

  for I := 0 to FTemplateTags.Count - 1 do
  begin
    LTemplateTag := TNovusTemplateTag(FTemplateTags.Items[I]);

    if LTemplateTag.SourceLineNo > ALineNo then
      LTemplateTag.SourceLineNo := LTemplateTag.SourceLineNo + 1;
  end;

end;

procedure TNovusTemplate2.CreateTemplateTag;
var
  FTemplateTag: TNovusTemplateTag;
  fsTag: String;
begin
  FTemplateTag := TNovusTemplateTag.Create;

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

function TNovusTemplate2.AddTemplateTag;
begin
  ATemplateTag.TagIndex := TemplateTags.Count;

  TemplateTags.Add(ATemplateTag);

  Result := ATemplateTag.TagIndex;
end;

procedure TNovusTemplate2.ParseTagValues;
Var
  I, X: Integer;
  FTemplateTag: TNovusTemplateTag;
  FsTagName: String;
  FStrParser: tNovusStringParser;
begin
  for I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TNovusTemplateTag(TemplateTags.Items[I]);

    FStrParser := tNovusStringParser.Create(FTemplateTag.RawTag);

    FTemplateTag.TagName := FStrParser.Items[0];

    For X := 1 to FStrParser.Count - 1 do
      FTemplateTag.Values.Add(FStrParser.Items[X]);

    FStrParser.Free;
  end;
end;

procedure TNovusTemplate2.ParseTags;
Var
  FTemplateTag: TNovusTemplateTag;
  I: Integer;
  FsInput: String;
begin
  For I := 0 to TemplateTags.Count - 1 do
  begin
    FTemplateTag := TNovusTemplateTag(TemplateTags.Items[I]);

    FsInput := FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1];

    FOutputDoc.Strings[FTemplateTag.SourceLineNo - 1] :=
      TNovusStringUtils.ReplaceStrPos(FsInput, StartToken + FTemplateTag.TagName
      + EndToken, FTemplateTag.TagValue, FTemplateTag.SourcePos - 1);
  end;
end;

function TNovusTemplate2.FindNextTagNameIndexOf(ATagName: String;
  AIndex: Integer): Integer;
begin
  Result := FindTagNameIndexOf(ATagName, AIndex);
end;

function TNovusTemplate2.FindTagNameIndexOf(ATagName: String;
  AIndex: Integer = 0): Integer;
Var
  I: Integer;
begin
  Result := -1;

  for I := AIndex to TemplateTags.Count - 1 do
  begin
    if Trim(uppercase(TNovusTemplateTag(TemplateTags.Items[I]).TagName))
      = Trim(uppercase(ATagName)) then
    begin
      Result := I;

      Break;
    end;
  end;

end;



end.
