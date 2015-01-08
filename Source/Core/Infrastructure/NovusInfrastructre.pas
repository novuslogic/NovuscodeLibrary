unit NovusInfrastructre;

interface

Uses Classes, NovusUtilities;

Type
  TNovusInfrastructre = Class(TPersistent)
  private
  protected
  public
     function CopyObject(Src: TObject; Related : Boolean = FALSE): Boolean; virtual;
  end;

implementation

function TNovusInfrastructre.CopyObject;
begin
  Result := TNovusUtilities.CopyObject(Src, Self, Related);
end;

end.


