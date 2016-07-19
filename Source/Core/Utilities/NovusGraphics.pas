unit NovusGraphics;

interface

Uses NovusUtilities, SysUtils, Classes, Windows, Messages, typinfo,
     Graphics;



Type
  TNovusGraphics = class(TNovusUtilities)
  public
     class function TwipsPerPixelX(Canvas : TCanvas) : Extended;
     class function TwipsPerPixelY(Canvas : TCanvas) : Extended;
  end;

implementation

class function TNovusGraphics.TwipsPerPixelX(Canvas : TCanvas) : Extended;
begin
  result := (1440 / GetDeviceCaps(Canvas.Handle, LOGPIXELSX));
end;

class function TNovusGraphics.TwipsPerPixelY(Canvas : TCanvas) : Extended;
begin
  result := (1440 / GetDeviceCaps(Canvas.Handle, LOGPIXELSY));
end;


end.
