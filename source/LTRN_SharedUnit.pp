unit LTRN_SharedUnit;

{$mode delphi}

interface

uses AnimationListUnit, ImageListUnit, LTRN_CurtainUnit, sdl;

const
  DATAFILENAME='LittleTrain.data';
  OptionsKey=SDLK_F12;

var
  Animations:TAnimationList;
  Sprites:TImageList;
  Curtain:TCurtain;

procedure LoadGraphics;

implementation

uses sysutils, ImageListLoaderUnit, ImageUnit, RawPictureUnit, MKFonts;

procedure LoadGraphics;
const images='abcdefghijklmnopqr!%#stuvwxyz=';
var i:integer;atm:TImage;
begin
  Sprites:=TImageList.Create;

  Animations:=TAnimationList.Create;
  Animations.LoadFromFile('sprites.bin');
//  Sprites.ListItems;
//  Sprites.SaveItems;

  for i:=0 to length(images)-1 do begin
    atm:=TImage.Create(32,32);
    Animations[images[i+1]].PutFrame(atm,0,0,0);
//    atm.PutimagePart(0,0,0,0,31,31,Sprites.FindImage(images[i+1]));
    atm.ShrinkImage4;
    Sprites.Add(atm,images[i+1]+'~');
  end;
  atm:=TImage.Create(32,32);
  Animations['#'].PutFrame(atm,0,0,0);
  Sprites.Add(atm,'#');
end;

end.


