{$mode delphi}

unit LTRN_MainUnit;

interface

uses
  mk_sdl2;

type

  { TMain }

  TMain=class
    constructor Create(iVersion, iBuildDate:String);
    destructor Destroy; override;
    function Run(pFirstRun:boolean):boolean;
  private
//    fVersionString:string;
    fMainWindow:TWindow;
  end;

implementation

uses
  SysUtils, Logger, MKStream, sdl2, MKAudio, MAD4MidLevelUnit, MKToolbox,
  LTRN_VMUUnit, LTRN_SharedUnit, LTRN_IntroUnit, LTRN_SlotSelectUnit,
  LTRN_MapSelectorUnit;

constructor TMain.Create(iVersion, iBuildDate: String);
var MAD4:TMAD4MidLevel;
begin
  randomize;
  iBuildDate:=replace(iBuildDate,'/','.');

{$IFDEF DEBUG}
  // Set data directory path to allow running without datafile
  MKStreamOpener.AddDirectory('..\data',100);
  // Set logging level
  Log.SetLogLevel(llAll);
{$ELSE}
  Log.SetLogLevel(llStatus);
{$ENDIF}

  //  Write log file header
  Log.LogAppHead(Format('LittleTrain V%s (Build date: %s)',[iVersion,iBuildDate]));

  //  Set up gfx and sound engine
  Log.LogStatus('Loading VMU...');
  VMU:=TVMU.Create;

  Log.LogDebug('Setting up SDL and BASS...');
  if VMU.FullScreen then begin
    fMainWindow:=TWindow.CreateFullScreenBordered(640,480,Format('LittleTrain V%s by MKSZTSZ - Build date: %s',[iVersion,iBuildDate]));
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1');
  end else
    fMainWindow:=TWindow.Create(SDL_WINDOWPOS_CENTERED,SDL_WINDOWPOS_CENTERED,640,480,Format('LittleTrain V%s by MKSZTSZ - Build date: %s',[iVersion,iBuildDate]));
  SDL_ShowCursor(SDL_Disable);
  SetFPS(60);
  Init_Audio;
{$IFNDEF DEBUG}
  // Try to mount the main executable, it should contain the datafile at the end.
  try
    MAD4:=TMAD4MidLevel.Create(paramstr(0));
    MKStreamOpener.AddOtherSource(MAD4, 0);
  except
    on exception do ;
  end;
{$ENDIF}
  if FileExists(DATAFILENAME) then MKStreamOpener.AddOtherSource(
    TMAD4MidLevel.Create(DATAFILENAME),
    0
  );

  VMU.CompleteAllMaps(0);

  LoadAssets;
end;

destructor TMain.Destroy;
begin
  FreeAssets;
  Log.LogStatus('Freeing VMU...');
  if Assigned(VMU) then FreeAndNil(VMU);
  Log.LogStatus('Freeing Main Window...');
  if Assigned(fMainWindow) then FreeAndNil(fMainWindow);
  inherited ;
end;

function TMain.Run(pFirstRun:boolean):boolean;
var i:integer;SlotSelect:TSlotSelector;
begin
{  MM.Fonts.DemoFonts;
  repeat
    Flip;
    HandleMessages;
  until keys[SDL_SCANCODE_SPACE];}
  MM.Musics['Menu']._music.Play;
  if pFirstRun then Intro;

  if ReturnTo in [rNone, rSlotSelector] then begin
    SlotSelect:=TSlotSelector.Create;
    if ReturnTo=rNone then
      i:=SlotSelect.Run
    else
      i:=SlotSelect.Run(ReturnData[0]);
    FreeAndNil(SlotSelect);
  end;

  if ReturnTo in [rNone, rMapSelector] then begin
    if ReturnTo=rMapSelector then i:=ReturnData[0];
    if i in [0..4] then begin
      MapSelector:=TMapSelector.Create(160,i);
      i:=MapSelector.Run;
      FreeAndNil(MapSelector);
    end;
  end;

  MM.Musics['Menu']._music.Stop;
  Result:=(i=-1);
end;

end.

