unit NovusEnvironment;

interface

Uses NovusUtilities, NovusParser, NovusTemplate, SysUtils;

type
  tNovusEnvironment = class(tNovusUtilities)
  protected
  private
  public
    class function ParseGetEnvironmentVar(aInput: String): String;
  end;

implementation

class function tNovusEnvironment.ParseGetEnvironmentVar(aInput: String): String;
Var
  loTemplate: tNovusTemplate;
  I: INteger;
  FTemplateTag: TTemplateTag;
begin
  result := aInput;

  if aInput = '' then Exit;

  Try
    loTemplate := tNovusTemplate.Create;

    loTemplate.StartToken := '{';
    loTemplate.EndToken := '}';
    loTemplate.SecondToken := '%';

    loTemplate.TemplateDoc.Text := Trim(aInput);

    loTemplate.ParseTemplate;

    For I := 0 to loTemplate.TemplateTags.Count -1 do
      begin
        FTemplateTag := TTemplateTag(loTemplate.TemplateTags.items[i]);

        FTemplateTag.TagValue := GetEnvironmentVariable(FTemplateTag.TagName);
      end;

    loTemplate.InsertAllTagValues;

    Result := Trim(loTemplate.OutputDoc.Text);

  Finally
    loTemplate.Free;
  End;
end;

end.
