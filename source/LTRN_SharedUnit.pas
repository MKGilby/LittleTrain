unit LTRN_SharedUnit;

{$mode delphi}

interface

uses MediaManagerUnit, sdl2, LTRN_CurtainUnit, LTRN_ScrollUnit, LTRN_MapListUnit,
  LTRN_OptionsUnit, LTRN_MapImagesUnit;

const
  DATAFILENAME='LittleTrain.data';
  OptionsKey=SDL_SCANCODE_F12;

var
  Curtain:TCurtain;
  Scroll:TLTRN_Scroll;
  MM:TMediaManager;
  MapList:TMapList;
  Options:TOptions;
  MapImages:TMapImages;

procedure LoadAssets;
procedure FreeAssets;

implementation

uses SysUtils, Logger, LTRN_VMUUnit, ARGBImageUnit, mk_sdl2;

procedure CreateShadow;
var atm:TARGBImage;
begin
  atm:=TARGBImage.Create(PrimaryWindow.Width,PrimaryWindow.Height);
  atm.Bar(0,0,atm.Width,atm.Height,0,0,0,144);
  MM.AddImage(atm,'Shadow',MM_CREATETEXTUREWHENNOANIMATIONDATA);
end;

procedure LoadAssets;
//const images='abcdefghijklmnopqr!%#stuvwxyz=';
//var i:integer;atm:TARGBImage;
begin
  Log.LogDebug('Loading assets...');
  MM:=TMediaManager.Create;

  Log.LogDebug('  sprites...');
  MM.Load('sprites01.png');
  MM.Load('logo.png','Logo',MM_CREATETEXTUREWHENNOANIMATIONDATA);

  Log.LogDebug('  musics...');
  MM.TreatMP3AsMusic:=true;
  MM.Load('littletrain_opt.mp3','Menu');
  MM.Load('littletrain_in.mp3','Ingame');
  MM.Load('littletrain_end.mo3','Ending');
  MM.Musics.GlobalVolume:=VMU.MusicVolume;

  Log.LogDebug('  sound fx...');
  MM.LoadWave('pick-up.mp3','Pickup',0.6);
  MM.LoadWave('xplode.mp3','Explosion',1.3);
  MM.LoadWave('exit.mp3','Complete',1.2);
  MM.LoadWave('open.mp3','DoorOpen',0.9);
  MM.LoadWave('menutick.mp3','MenuMoveTick',0.7);
  MM.LoadWave('select.mp3','MenuSelect',0.7);
  MM.Waves.GlobalVolume:=VMU.SoundVolume;

  Log.LogDebug('  fonts...');
  MM.Load('font.png','0');
  MM.Fonts.ItemByName['0'].SetColor(128,255,224);
  MM.Fonts.ItemByName['0'].SetColorKey(0,0,0);
  MM.Load('font.png','1');
  MM.Fonts.ItemByName['1'].SetColor(255,255,128);
  MM.Fonts.ItemByName['1'].SetColorKey(0,0,0);
  MM.Load('font.png','2');
  MM.Fonts.ItemByName['2'].SetColor(255,128,224);
  MM.Fonts.ItemByName['2'].SetColorKey(0,0,0);
  MM.Load('font.png','3');
  MM.Fonts.ItemByName['3'].SetColor(240,240,240);
  MM.Fonts.ItemByName['3'].SetColorKey(0,0,0);
  MM.Load('font2.png','4');
  MM.Fonts.ItemByName['4'].SetColorKey(0,0,0);
  MM.Load('font.png','5');
  MM.Fonts.ItemByName['5'].SetColor(255,0,0);
  MM.Fonts.ItemByName['5'].SetColorKey(0,0,0);
  MM.Load('font.png','6');
  MM.Fonts.ItemByName['6'].SetColor(255,255,0);
  MM.Fonts.ItemByName['6'].SetColorKey(0,0,0);
  MM.Load('font.png','7');
  MM.Fonts.ItemByName['7'].SetColor(0,255,0);
  MM.Fonts.ItemByName['7'].SetColorKey(0,0,0);
  MM.Load('font.png','8');
  MM.Fonts.ItemByName['8'].SetColor(0,64,255);
  MM.Fonts.ItemByName['8'].SetColorKey(0,0,0);

  Log.LogDebug('  maps...');
  MapList:=TMapList.Create('maps.bin');


  Log.LogDebug('Creating persistent entities...');
  Log.LogDebug('  scroll...');
  Scroll:=TLTRN_Scroll.Create;

  Log.LogDebug('  curtain...');
  Curtain:=TCurtain.Create;

  Log.LogDebug('  options dialog...');
  Options:=TOptions.Create;

  Log.LogDebug('  map thumbnails...');
  MapImages:=TMapImages.Create(MapList);
  CreateShadow;
end;

procedure FreeAssets;
begin
  Log.LogDebug('Freeing persistent entities...');
  Log.LogDebug('  options dialog...');
  if Assigned(Options) then FreeAndNil(Options);
  Log.LogDebug('  curtain...');
  if Assigned(Curtain) then FreeAndNil(Curtain);
  Log.LogDebug('  scroll...');
  if Assigned(Scroll) then FreeAndNil(Scroll);
  Log.LogDebug('Freeing assets...');
  Log.LogDebug('  maps...');
  if Assigned(MapList) then FreeAndNil(MapList);
  Log.LogDebug('  media...');
  if Assigned(MM) then FreeAndNil(MM);
end;

end.


