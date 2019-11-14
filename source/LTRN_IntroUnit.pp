{$mode delphi}
{$smartlink on}

unit LTRN_IntroUnit;

interface

procedure Intro;

implementation

uses
  SysUtils, SDL, MK_SDL, MKFonts, ImageListUnit, ImageListLoaderUnit,
  ImageUnit, Logger, LTRN_MapImagesUnit, FontCollectionUnit;

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
  PlayImage(12,205,IL.FindImage('mksztsz'),1,0);
  if esc then exit;

  FC.FontByIndex[3].SetAlpha(192);
  FC.FontByIndex[3].OutText(atm,'music by',320,120,mjCenter);
  atm.PutImage(245,160,IL.FindImage('infamous_logo'));
  FC.FontByIndex[3].SetAlpha(255);
  FC.FontByIndex[3].OutText(atm,'www.infamousuk.com',320,350,mjCenter);
  PlayImage(0,0,atm,1,0);
  if esc then exit;

  atm.bar(0,120,639,381,0,0,0);
  FC.FontByIndex[3].SetAlpha(192);
  FC.FontByIndex[3].OutText(atm,'sound fx by',320,200,mjCenter);
  FC.FontByIndex[3].SetAlpha(255);
  FC.FontByIndex[3].OutText(atm,'Mike Fraley',320,250,mjCenter);
  PlayImage(0,0,atm,1,0);
  if esc then exit;

  PlayImage(0,196,IL.FindImage('logobig'),1,0);
  if esc then exit;

  atm.bar(0,200,639,299,0,0,0);
  atm.PutImage(140,0,IL.FindImage('logo'));
  FC.FontByIndex[3].OutText(atm,'Original (C)''93 Miroslav Nemecek',320,64,mjCenter);
  FC.FontByIndex[3].OutText(atm,'This version (C) 2019 MKSZTSZ',320,96,mjCenter);

  FC.FontByIndex[3].SetAlpha(192);
  FC.FontByIndex[3].OutText(atm,'Tools used to create this game:',20,150,mjLeft);
  FC.FontByIndex[3].SetAlpha(255);
  FC.FontByIndex[3].OutText(atm,'- Lazarus V2.0.2',60,182,mjLeft);
  FC.FontByIndex[3].OutText(atm,'- JEDI-SDL',60,214,mjLeft);
  FC.FontByIndex[3].OutText(atm,'- BASS V2.4',60,246,mjLeft);
  FC.FontByIndex[3].OutText(atm,'- PoV-Ray V3.61',60,278,mjLeft);

  FC.FontByIndex[3].SetAlpha(192);
  FC.FontByIndex[3].OutText(atm,'Visit:',20,332,mjLeft);
  FC.FontByIndex[3].SetAlpha(255);
  FC.FontByIndex[3].OutText(atm,'- www.infamousuk.com',60,364,mjLeft);
  FC.FontByIndex[3].OutText(atm,'- www.un4seen.com',60,396,mjLeft);
  FC.FontByIndex[3].OutText(atm,'- www.mksztsz.hu',60,428,mjLeft);

  PlayImage(0,0,atm,2,60);
end;

procedure Intro;
begin
  IL:=TImageList.Create;
  LoadImageListByteCode('intro.bin',IL);
  atm:=TImage.Create(640,480);
  RealIntro;
  FreeAndNIL(atm);
  FreeAndNil(IL);
  keys[SDLK_Escape]:=false;
end;

end.
