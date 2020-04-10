unit NovusObject;

interface

Uses Classes, NovusUtilities;

Type
  TNovusObject = Class(TPersistent)
  private
  protected
  public
     function CopyObject(Src: TObject; Related : Boolean = FALSE): Boolean; virtual;
  end;

implementation

function TNovusObject.CopyObject;
begin
  Result := TNovusUtilities.CopyObject(Src, Self, Related);
end;

end.


