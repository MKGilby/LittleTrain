{$mode delphi}

unit LTRN_MainUnit;

interface

uses
  Classes, LTRN_MapListUnit;

type
  TMain=class
    constructor Create(Version, Build, BDate:String);
    destructor Destroy; override;
    procedure Run;
//  private
//    procedure CreateLittleBlocks;
  end;

implementation

uses sysutils, mk_sdl, MKAudio, sdl, Logger, MusicCollectionUnit, SmallFontUnit,
     MAD4HighLevelUnit, LTRN_VMUUnit, WaveCollectionUnit, FontCollectionUnit,
     ImageUnit, MKStream, LTRN_ScrollUnit, LTRN_OptionsUnit, LTRN_CurtainUnit,
     LTRN_IntroUnit, LTRN_PlayerSelectUnit, LTRN_SharedUnit,
     LTRN_MapSelectorUnit, LTRN_MapImagesUnit;

const
  Fstr='LTRN_MainUnit.pp, ';

constructor TMain.Create(Version, Build, BDate:String);
const Istr=Fstr+'TMain.Create';
begin
  MKStreamOpener.SetBaseDir('..\data');
  Log.SetLogLevel(llStatus);
//  Log.SetLogLevel(llAll);

  randomize;
  while (length(build)>0) and (build[1]='0') do delete(build,1,1);
  if length(build)>0 then build:='.'+build;

// Write log file header
  Log.LogAppHead('LittleTrain V'+Version+Build+' (Build date: '+BDate+')',Istr);

// Set up gfx and sound engine
  Log.LogDebug('Setting up SDL and BASS...',Istr);
  if uppercase(paramstr(1))='FULLSCREEN' then SDLInit(640,480,32,MKSDL_SECONDARYSURFACE or SDL_FULLSCREEN)
//                                         else SDLInit(1280,960,32,MKSDL_SECONDARYSURFACE or MKSDL_2X2Pixels);
                                         else SDLInit(640,480,32,MKSDL_SECONDARYSURFACE);
  SDL_WM_SetCaption(pchar('LittleTrain V'+Version+Build+' by MKSZTSZ - Build date: '+BDate), nil);
  SDL_ShowCursor(SDL_Disable);
  SetFPS(60);
  Init_Audio;

// Open game data file
  Log.LogDebug('Opening game data file...',Istr);
  if FileExists(DATAFILENAME) then MAD4.Mount(DATAFILENAME)
  else MAD4.Mount(paramstr(0));
//  MAD.Verbose:=true;

// Load music from music.mad
  Log.LogDebug('Loading music...',Istr);
  MC.Load('controls.ini','Music');
  MC.List;
  MC.GlobalVolume:=VMU.MusicVolume;

// Load sound fx
  Log.LogDebug('Loading sound fx...',Istr);
  WC.Load('controls.ini','Waves');
  WC.GlobalVolume:=VMU.SoundVolume;

// Load fonts
  Log.LogDebug('Loading fonts...',Istr);
  FC.Load('controls.ini','Game.Fonts');
  CreateSmallFont;
  SmallFont.SetColorKey(0,0,0);

// Load graphics
  Log.LogDebug('Loading graphics...',Istr);
  LoadGraphics;
//  IC.Load('controls.ini','GFX');
//  Log.LogDebug('Creating little blocks...',Istr);
//  CreateLittleBlocks;
  Curtain:=TCurtain.Create(Sprites.FindImage('#'));

// Create scroll
  Log.LogDebug('Creating scroll...',Istr);
  Scroll:=TLTRN_Scroll.Create;

// Loading map pack
  Log.LogDebug('Loading map pack...',Istr);
  MapList:=TMapList.Create('maps.bin');

  Options:=TOptions.Create;
  MapImages:=TMapImages.Create(MapList);
end;

destructor TMain.Destroy;
begin
  FreeAndNIL(MapImages);
  FreeAndNIL(Options);
  FreeAndNil(MapList);
  FreeAndNil(Scroll);
  FreeAndNil(Curtain);
  FreeAndNil(Sprites);
  FreeAndNil(Animations);
  WC.Clear;
  MC.Clear;
  SDLDone;
  inherited ;
end;

procedure TMain.Run;
var i:integer;
begin
  MC['Menu']._music.Play;
  Intro;

  PlayerSelect:=TPlayerSelect.Create;
  i:=PlayerSelect.Run;
  FreeAndNil(PlayerSelect);

  if i in [0..4] then begin
    MapSelector:=TMapSelector.Create(160,i+1);
    MapSelector.Run;
    FreeAndNil(MapSelector);
  end;

  MC['Menu']._music.Stop;
end;

{procedure TMain.CreateLittleBlocks;
const images='abcdefghijklmnopqr!%#';
var i:integer;atm:TImage;
begin
  for i:=0 to 20 do begin
    atm:=TImage.Create(32,32);
    atm.PutimagePart(0,0,0,0,31,31,IC.FindImage(images[i+1]));
    atm.ShrinkImage4;
    IC.Add(atm,images[i+1]+'~');
  end;
end;}

end.

