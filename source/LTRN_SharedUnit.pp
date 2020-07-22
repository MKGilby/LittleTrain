unit LTRN_SharedUnit;

{$mode delphi}

interface

uses AnimationListUnit, ImageListUnit, MusicListUnit, WaveListUnit,
  LTRN_CurtainUnit, ASHInterpreterUnit, FontListUnit, sdl;

const
  DATAFILENAME='LittleTrain.data';
  OptionsKey=SDLK_F12;

var
  Animations:TAnimationList;
  Sprites:TImageList;
  Curtain:TCurtain;
  ASHI:TASHInterpreter;
  Fonts:TFontList;
  Muzax:TMusicList;
  Waves:TWaveList;

procedure LoadGraphics;

implementation

uses sysutils, {ImageListLoaderUnit, }ImageUnit, RawPictureUnit,
  Logger;

procedure LoadGraphics;
const images='abcdefghijklmnopqr!%#stuvwxyz=';
var i:integer;atm:TImage;
//  ASHI:TASHInterpreter;
begin
  Sprites:=TImageList.Create;

  Animations:=TAnimationList.Create;
  ASHI.RunBlock('Images',Animations.InternalImageList);
  ASHI.RunBlock('Anims',Animations);
//  Animations.List;

  for i:=0 to length(images)-1 do begin
//    Log.Trace(inttostr(i)+', '+images[i+1]);
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


