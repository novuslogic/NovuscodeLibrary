unit NovusParser.Common;

interface

Uses Novuslist, System.Classes, System.SysUtils;

type
  TNovusTemplateTag = class;


  TOnGetTagValueEvent = procedure(Sender: TObject; Var ATagValue: String)
    of object;

  TNovusTemplateLineInfo = class(Tobject)
  private
  protected
    fiLineNo: Integer;
  public
    property LineNo: Integer read fiLineNo write fiLineNo;
  end;

  TNovusTemplateTags = Class(tNovuslist)
  private
  protected
  public
    /// <summary>
    /// AddTag to list
    /// </summary>
    function AddTag(aTagName: string; aTagValue: string = ''): TNovusTemplateTag;

    /// <summary>
    /// Find Tagname from TNovusTemplateTags class
    /// </summary>
    function FindTagNameIndexOf(aTagName: String; AIndex: Integer = 0): Integer;
  End;

  TNovusTemplateTag = class
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

    procedure Assign(Source: TNovusTemplateTag);
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

const
  toEOL = char(6);
  toBOF = char(7);
  TAB = #09;


implementation


// TNovusTemplateTag
procedure TNovusTemplateTag.SetTagName(const Value: string);
begin
  if FsTagName <> Value then
  begin
    FsTagName := Value;
  end;
end;

function TNovusTemplateTag.GetTagValue: String;
begin
  if Assigned(OnGetTagValueEvent) then
    OnGetTagValueEvent(Self, fsTagValue);

  Result := fsTagValue;
end;

procedure TNovusTemplateTag.SetTagValue(const Value: string);
begin
  if fsTagValue <> Value then
  begin
    fsTagValue := Value;
  end;
end;

constructor TNovusTemplateTag.Create;
begin
  FValues := tStringList.Create;

  FsTagName := '';
  fsTagValue := '';
end;

destructor TNovusTemplateTag.Destroy;
begin
  FValues.Free;

  inherited;
end;

procedure TNovusTemplateTag.Assign(Source: TNovusTemplateTag);
begin
  if Source is TNovusTemplateTag then
  begin
    TagName := TNovusTemplateTag(Source).TagName;
    TagValue := TNovusTemplateTag(Source).TagValue;
  end
end;


//TNovusTemplateTags

function TNovusTemplateTags.AddTag(aTagName: string; aTagValue: string = ''): TNovusTemplateTag;
begin
  Result := TNovusTemplateTag.Create;
  Result.TagName := aTagName;
  Result.TagValue := aTagValue;

  self.Add(Result);
end;

function TNovusTemplateTags.FindTagNameIndexOf(ATagName: String;
  AIndex: Integer = 0): Integer;
Var
  I: Integer;
begin
  Result := -1;

  for I := AIndex to Self.Count - 1 do
  begin
    if Trim(uppercase(TNovusTemplateTag(Self.Items[I]).TagName))
      = Trim(uppercase(ATagName)) then
    begin
      Result := I;

      Break;
    end;
  end;

end;



end.
