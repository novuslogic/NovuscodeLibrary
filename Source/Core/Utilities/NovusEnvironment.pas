unit NovusEnvironment;

interface

Uses NovusUtilities, NovusParser, NovusTemplate2, SysUtils, NovusParser.Common;

type
  tEnvironmentTokenType = (ETTToken1, ETTToken2);

  tNovusEnvironment = class(tNovusUtilities)
  protected
  private
  public
    /// <summary>
    ///   Parse enciroment variables in string
    /// </summary>
    /// <param name="aInput">
    ///   input string
    /// </param>
    /// <param name="aEnvironmentTokenType">
    ///   ETTToken1, ETTToken2
    /// </param>
    /// <remarks>
    ///   Defaults to ETTToken2
    /// </remarks>
    class function ParseGetEnvironmentVar(aInput: String;
      aEnvironmentTokenType: tEnvironmentTokenType = ETTToken2): String;
  end;

implementation

class function tNovusEnvironment.ParseGetEnvironmentVar(aInput: String;
  aEnvironmentTokenType: tEnvironmentTokenType): String;
Var
  loTemplate: tNovusTemplate2;
  I: Integer;
  FTemplateTag: TNovusTemplateTag;
begin
  result := aInput;

  if aInput = '' then
    Exit;

  Try
    loTemplate := tNovusTemplate2.Create;

    case aEnvironmentTokenType of
      ETTToken1:
      begin
        loTemplate.StartToken := '%';
        loTemplate.EndToken := '%';
        loTemplate.SecondToken :=#0;
      end;
      ETTToken2:
      begin
        loTemplate.StartToken := '{';
        loTemplate.EndToken := '}';
         loTemplate.SecondToken := '%';
      end;
    end;


    //loTemplate.TemplateDoc.Text := Trim(aInput);
    loTemplate.LoadFromString(Trim(aInput));

    loTemplate.ParseTemplate;

    For I := 0 to loTemplate.TemplateTags.Count - 1 do
    begin
      FTemplateTag := TNovusTemplateTag(loTemplate.TemplateTags.items[I]);

      FTemplateTag.TagValue := GetEnvironmentVariable(FTemplateTag.TagName);
    end;

    loTemplate.InsertAllTagValues;

    result := Trim(loTemplate.OutputDoc.Text);

  Finally
    loTemplate.Free;
  End;
end;

end.
