unit NovusVCLControlsUtils;

interface

Uses NovusUtilities, VCL.Controls, VCL.Menus, SysUtils;

type
  tCaptions = array of string;

  tNovusVCLControlsUtils = class(tNovusUtilities)
  private
  protected
  public
    class function FindMenuItembyCaption(aCaptions: tCaptions;
      aMenuItems: tMenuItem): tMenuItem;
  end;

implementation

class function tNovusVCLControlsUtils.FindMenuItembyCaption
  (aCaptions: tCaptions; aMenuItems: tMenuItem): tMenuItem;

  function FindMenuItems(aFindMenuItems: tMenuItem): tMenuItem;
  var
    i, x: Integer;
    lArrayLength: Cardinal;
    lsCaption: String;
  begin
    Result := Nil;

    for i := 0 To aFindMenuItems.Count - 1 do
    begin
      lsCaption := StringReplace(aFindMenuItems[i].Caption, '&', '', []);
      if Uppercase(aCaptions[0]) = Uppercase(lsCaption) then
      begin
        lArrayLength := Length(aCaptions);
        if lArrayLength = 1 then
          Result := aFindMenuItems[i]
        else
        begin
          for x := 1 to lArrayLength - 1 do
            aCaptions[x - 1] := aCaptions[x];
          SetLength(aCaptions, lArrayLength - 1);
          Result := FindMenuItems(aFindMenuItems.Items[i]);
        end;
        break;
      end;
    end;
  end;

begin
  Result := FindMenuItems(aMenuItems);
end;

end.
