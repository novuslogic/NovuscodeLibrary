unit NovusCommentsParserCell;

interface

uses NovusInterpreter, NovusParser, System.Classes, System.SysUtils;

type
  tNovusCommentsTypes = (tncCLang,tncPascal);

  tNovusCommentsKeyword =  class(tNovusKeyword)
  private
  protected
    function GetIsMultiLineComment: boolean;
  public
    class function Init(aParserCell: tNovusParserCell): tNovusKeyword; override;

    property IsMultiLineComment: boolean
      read GetIsMultiLineComment;
  end;

  tNovusCommentsParserCell = class(tNovusParserCell)
  protected
  private
    fbIsMultiLineComment: boolean;
    fCommentsTypes: tNovusCommentsTypes;
  public
    class function Init(aInterpreter: tNovusInterpreter; aCommentsTypes: tNovusCommentsTypes = tncCLang): tNovusCommentsParserCell; overload;
    function ParseNextToken: Char; override;

    property IsMultiLineComment: boolean
      read fbIsMultiLineComment;

    property CommentsTypes: tNovusCommentsTypes
      read fCommentsTypes
      write fCommentsTypes;
  end;


implementation

// tNovusCommentsParserCell
class function tNovusCommentsParserCell.Init(aInterpreter: tNovusInterpreter; aCommentsTypes: tNovusCommentsTypes): tNovusCommentsParserCell;
begin
  Result := tNovusCommentsParserCell.Create(aInterpreter);
  Result.CommentsTypes := aCommentsTypes;
end;

function tNovusCommentsParserCell.ParseNextToken: Char;
Var
  lch: char;
  loToken: tToken;
  liStartTokenPos,
  liStartSourceLineNo,
  liStartColumnPos:integer;
begin
  Result := #0;

  With foInterpreter do
    begin
      if (Token = '/') or (Token = '(') and (fCommentsTypes = tncPascal) then
        begin
          if PeekJustNextToken = '/' then
            begin
              loToken := tToken.Create(Self);

              loToken.StartTokenPos := TokenPos-1;

              loToken.StartSourceLineNo := SourceLineNo;
              loToken.StartColumnPos := ColumnPos;

              Result := SkipToEOL(false);
              loToken.EndSourceLineNo := loToken.StartSourceLineNo;

              loToken.EndColumnPos := ColumnPos;

              if Result = toEOL then ColumnPos := 1;

              loToken.EndTokenPos := TokenPos + 2;

              loToken.RawToken := CopyParseString(loToken.StartTokenPos, TokenPos);

              oInterpreter.AddToken(loToken);

              Exit;
            end
          else
          if PeekJustNextToken = '*' then
            begin
              fbIsMultiLineComment := true;

              liStartTokenPos := TokenPos -2;
              liStartSourceLineNo := SourceLineNo;
              liStartColumnPos := ColumnPos;

              lch := NextToken;
              while True do
                 begin
                 while (lch <> '*') and (lch <> toEOF) do
                   begin
                     lch := NextToken;
                     if lch = toEOF then  exit;
                   end;

                 lch := NextToken;
                 if ((lch = '/') and (fCommentsTypes = tncCLang)) or
                    ((lch = ')') and (fCommentsTypes = tncPascal)) then
                  begin
                    loToken := tToken.Create(self);

                    fbIsMultiLineComment := False;

                    loToken.StartTokenPos := liStartTokenPos;
                    loToken.StartSourceLineNo := liStartSourceLineNo;
                    loToken.StartColumnPos := liStartColumnPos;

                    loToken.EndSourceLineNo := SourceLineNo;
                    loToken.EndColumnPos := ColumnPos;
                    loToken.EndTokenPos := TokenPos + 2;

                    loToken.RawToken := CopyParseString(loToken.StartTokenPos, TokenPos);

                    oInterpreter.AddToken(loToken);

                    break;
                  end;

                 end;
              end;
        end;
    end;
end;



//tNovusCommentskeyword
class function tNovusCommentskeyword.Init(aParserCell: tNovusParserCell): tNovusKeyword;
begin
  Result := tNovusCommentskeyword.Create(aParserCell);
end;

function tNovusCommentskeyword.GetIsMultiLineComment: boolean;
begin
  Result :=  (foParserCell as tNovusCommentsParserCell).IsMultiLineComment;
end;



end.
