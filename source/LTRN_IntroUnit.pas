{$mode delphi}
{$smartlink on}

unit LTRN_IntroUnit;

interface

procedure Intro;

implementation

uses
  SysUtils, SDL2, mk_sdl2, Logger, LTRN_SharedUnit, ARGBImageUnit, Font2Unit;

var
  atm:TStreamingTexture;

procedure RealIntro;
var esc:boolean;atmI:TARGBImage;
    
  procedure PlayImage(len,static:integer);
  var s,cnt,maxcnt:Integer;
  begin
    atm.Update;
    len:=3400*len-1000;
    s:=SDL_GetTicks;
    SDL_RenderClear(PrimaryWindow.Renderer);
    for cnt:=0 to 63 do begin
      bar(0,0,PrimaryWindow.Width,PrimaryWindow.Height,0,0,0);
      SDL_SetTextureAlphaMod(atm.Texture,cnt<<2);
      PutTexture(0,0,atm);
      maxcnt:=cnt;
      HandleMessages;
      if keys[SDL_SCANCODE_SPACE] then break;
      if keys[SDL_SCANCODE_ESCAPE] then begin esc:=true;exit;end;
      Flip;
    end;
    if maxcnt=63 then begin
      repeat
        if keys[SDL_SCANCODE_SPACE] then break;
        if keys[SDL_SCANCODE_ESCAPE] then begin esc:=true;exit;end;
        HandleMessages;
        Flip;
      until (SDL_GetTicks-s>len) or keys[SDL_SCANCODE_SPACE];
    end;
    for cnt:=maxcnt downto 0 do begin
      HandleMessages;
      if keys[SDL_SCANCODE_ESCAPE] then begin esc:=true;exit;end;
      bar(0,static,PrimaryWindow.Width,PrimaryWindow.Height-static,0,0,0);
      SDL_SetTextureAlphaMod(atm.Texture,cnt<<2);
      PutTexturePart(0,static,0,static,atm.Width,atm.Height-static,atm);
      Flip;
    end;
  end;

begin
  esc:=false;
  atm.ARGBImage.Clear;
  atmI:=TARGBImage.Create('mksztsz.png');
  atmI.CopyTo(0,0,atmi.Width,atmi.Height,12,205,atm.ARGBImage);
  FreeAndNil(atmI);
  PlayImage(1,0);
  if esc then exit;

  atm.ARGBImage.Clear;
  MM.Fonts['3'].SetAlpha(192);
  MM.Fonts['3'].OutText(atm.ARGBImage,'music by',320,120,mjCenter);
  atmI:=TARGBImage.Create('infamous_logo.png');
  atmI.CopyTo(0,0,atmi.Width,atmi.Height,245,160,atm.ARGBImage);
  FreeAndNil(atmI);
  MM.Fonts['3'].SetAlpha(255);
  MM.Fonts['3'].OutText(atm.ARGBImage,'www.infamousuk.com',320,350,mjCenter);
  PlayImage(1,0);
  if esc then exit;

  atm.ARGBImage.Clear;
  MM.Fonts['3'].SetAlpha(192);
  MM.Fonts['3'].OutText(atm.ARGBImage,'sound fx by',320,200,mjCenter);
  MM.Fonts['3'].SetAlpha(255);
  MM.Fonts['3'].OutText(atm.ARGBImage,'Mike Fraley',320,250,mjCenter);
  PlayImage(1,0);
  if esc then exit;

  atm.ARGBImage.Clear;
  atmI:=TARGBImage.Create('logobig.png');
  atmI.CopyTo(0,0,atmi.Width,atmi.Height,16,196,atm.ARGBImage);
  FreeAndNil(atmI);
  PlayImage(1,0);
  if esc then exit;

  atm.ARGBImage.Clear;
  MM.Images.ItemByName['Logo'].CopyTo(0,0,atmi.Width,atmi.Height,149,3,atm.ARGBImage);
//  atmI:=TARGBImage.Create('logo.png');
//  atmI.CopyTo(0,0,atmi.Width,atmi.Height,149,3,atm.ARGBImage);
//  FreeAndNil(atmI);
  MM.Fonts['3'].OutText(atm.ARGBImage,'Original (C)''93 Miroslav Nemecek',320,64,mjCenter);
  MM.Fonts['3'].OutText(atm.ARGBImage,'This version (C) 2022 MKSZTSZ',320,96,mjCenter);

  MM.Fonts['3'].SetAlpha(192);
  MM.Fonts['3'].OutText(atm.ARGBImage,'Tools used to create this game:',20,150,mjLeft);
  MM.Fonts['3'].SetAlpha(255);
  MM.Fonts['3'].OutText(atm.ARGBImage,'- Lazarus V2.2.2',60,182,mjLeft);
  MM.Fonts['3'].OutText(atm.ARGBImage,'- JEDI-SDL',60,214,mjLeft);
  MM.Fonts['3'].OutText(atm.ARGBImage,'- BASS V2.4',60,246,mjLeft);
  MM.Fonts['3'].OutText(atm.ARGBImage,'- PoV-Ray V3.61',60,278,mjLeft);

  MM.Fonts['3'].SetAlpha(192);
  MM.Fonts['3'].OutText(atm.ARGBImage,'Visit:',20,332,mjLeft);
  MM.Fonts['3'].SetAlpha(255);
  MM.Fonts['3'].OutText(atm.ARGBImage,'- www.infamousuk.com',60,364,mjLeft);
  MM.Fonts['3'].OutText(atm.ARGBImage,'- www.un4seen.com',60,396,mjLeft);
  MM.Fonts['3'].OutText(atm.ARGBImage,'- www.mksztsz.hu',60,428,mjLeft);

  PlayImage(2,60);
end;

procedure Intro;
begin
  atm:=TStreamingTexture.Create(640,480);
  SDL_SetTextureBlendMode(atm.Texture, SDL_BLENDMODE_BLEND);
  RealIntro;
  FreeAndNIL(atm);
  keys[SDLK_Escape]:=false;
end;

end.
