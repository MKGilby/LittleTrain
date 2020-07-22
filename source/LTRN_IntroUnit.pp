{$mode delphi}
{$smartlink on}

unit LTRN_IntroUnit;

interface

procedure Intro;

implementation

uses
  SysUtils, SDL, MK_SDL, ImageListUnit, FontUnit,
  ImageUnit, Logger, LTRN_MapImagesUnit, LTRN_SharedUnit;

var IL:TImageList;
    atm:TImage;

procedure RealIntro;
var esc:boolean;
    
  procedure PlayImage(x,y:integer;atm:TImage;len,static:integer);
  var s,cnt,maxcnt:Integer;
  begin
    len:=3400*len-1000;
    s:=SDL_GetTicks;
    ClearScreen(0,0,0);
    for cnt:=0 to 63 do begin
      SDL_SetAlpha(atm.Surface,SDL_SRCALPHA,cnt<<2);
      barWH(x,y,atm.Surface.w,atm.Surface.h,0,0,0);
      PutImage(x,y,atm);
      maxcnt:=cnt;
      HandleMessages;
      if keys[SDLK_Space] then break;
      if keys[SDLK_Escape] then begin esc:=true;exit;end;
      Flip;
    end;
    if maxcnt=63 then begin

      repeat
        if keys[SDLK_Space] then break;
        if keys[SDLK_Escape] then begin esc:=true;exit;end;
        if len-(SDL_GetTicks-s)>40 then MapImages.CreateNextMapImage;
        HandleMessages;
        Flip;
      until (SDL_GetTicks-s>len) or keys[SDLK_Space];
    end;
    for cnt:=maxcnt downto 0 do begin
      HandleMessages;
      if keys[SDLK_Escape] then begin esc:=true;exit;end;
      SDL_SetAlpha(atm.surface,SDL_SRCALPHA+SDL_RLEACCEL,cnt<<2);
      barWH(x,y+static,atm.Surface.w,atm.Surface.h-static,0,0,0);
      PutImagePartWH(x,y+static,0,static,atm.Surface.w,atm.Surface.h-static,atm);
      Flip;
    end;
  end;

begin
  esc:=false;
  PlayImage(12,205,IL.FindImage('mksztsz.tga'),1,0);
  if esc then exit;

  Fonts.FontByIndex[3].SetAlpha(192);
  Fonts.FontByIndex[3].OutText(atm,'music by',320,120,mjCenter);
  atm.PutImage(245,160,IL.FindImage('infamous_logo.tga'));
  Fonts.FontByIndex[3].SetAlpha(255);
  Fonts.FontByIndex[3].OutText(atm,'www.infamousuk.com',320,350,mjCenter);
  PlayImage(0,0,atm,1,0);
  if esc then exit;

  atm.bar(0,120,639,381,0,0,0);
  Fonts.FontByIndex[3].SetAlpha(192);
  Fonts.FontByIndex[3].OutText(atm,'sound fx by',320,200,mjCenter);
  Fonts.FontByIndex[3].SetAlpha(255);
  Fonts.FontByIndex[3].OutText(atm,'Mike Fraley',320,250,mjCenter);
  PlayImage(0,0,atm,1,0);
  if esc then exit;

  PlayImage(16,196,IL.FindImage('logobig.tga'),1,0);
  if esc then exit;

  atm.bar(0,200,639,299,0,0,0);
  atm.PutImage(149,3,IL.FindImage('logo.tga'));
  Fonts.FontByIndex[3].OutText(atm,'Original (C)''93 Miroslav Nemecek',320,64,mjCenter);
  Fonts.FontByIndex[3].OutText(atm,'This version (C) 2019 MKSZTSZ',320,96,mjCenter);

  Fonts.FontByIndex[3].SetAlpha(192);
  Fonts.FontByIndex[3].OutText(atm,'Tools used to create this game:',20,150,mjLeft);
  Fonts.FontByIndex[3].SetAlpha(255);
  Fonts.FontByIndex[3].OutText(atm,'- Lazarus V2.0.6',60,182,mjLeft);
  Fonts.FontByIndex[3].OutText(atm,'- JEDI-SDL',60,214,mjLeft);
  Fonts.FontByIndex[3].OutText(atm,'- BASS V2.4',60,246,mjLeft);
  Fonts.FontByIndex[3].OutText(atm,'- PoV-Ray V3.61',60,278,mjLeft);

  Fonts.FontByIndex[3].SetAlpha(192);
  Fonts.FontByIndex[3].OutText(atm,'Visit:',20,332,mjLeft);
  Fonts.FontByIndex[3].SetAlpha(255);
  Fonts.FontByIndex[3].OutText(atm,'- www.infamousuk.com',60,364,mjLeft);
  Fonts.FontByIndex[3].OutText(atm,'- www.un4seen.com',60,396,mjLeft);
  Fonts.FontByIndex[3].OutText(atm,'- www.mksztsz.hu',60,428,mjLeft);

  PlayImage(0,0,atm,2,60);
end;

procedure Intro;
begin
  IL:=TImageList.Create;
  ASHI.RunBlock('Intro',IL);
//  LoadImageListByteCode('intro.bin',IL);
  atm:=TImage.Create(640,480);
  RealIntro;
  FreeAndNIL(atm);
  FreeAndNil(IL);
  keys[SDLK_Escape]:=false;
end;

end.
