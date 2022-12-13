unit LTRN_MapPlayUnit;

{$mode delphi}

interface

uses
  LTRN_MapBaseUnit;

type
  TMapPlay=class(TMapBase)
    function Play:integer; override; // Result: 0-Completed, 1-Escaped, 2-Dead
  end;

implementation

uses SysUtils, Bass, sdl2, mk_sdl2, MKToolbox, Font2Unit, LTRN_CurtainUnit,
  LTRN_SharedUnit, LTRN_OptionsUnit, LTRN_VMUUnit, Logger;

function TMapPlay.Play:integer;
var i,j:integer;
begin
  fMap.Reset;
  fScore:=0;
  Logo.Draw;

  DrawMap;
  Curtain.StartOpen;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    Logo.Draw;
    MM.Fonts['0'].OutText('Steps:',120,434,mjCenter);
    MM.Fonts['1'].OutText(st(fScore,5,'0'),120,456,mjCenter);
    MM.Fonts['0'].OutText('Map:',520,434,mjCenter);
    MM.Fonts['1'].OutText(st(fMapNo+1,2,'0'),520,456,mjCenter);
    MM.Fonts['0'].OutText('Best:',320,434,mjCenter);
    if fBest=0 then MM.Fonts['2'].OutText('None',320,456,mjCenter)
               else MM.Fonts['2'].OutText(st(fBest,5,'0'),320,456,mjCenter);

    for i:=0 to 19 do
      for j:=0 to 11 do
        if fSprites[i,j]<>nil then fSprites[i,j].Draw;
//        if (i=4) and (j=2) and (fSprites[i,j]<>nil) then fSprites[i,j].LogSpriteData;
    fExit.Draw;
    fPlayer.Draw;
//    bar(50,456,189,479,0,0,0);
//    MultiFont[1].OutText(st(fScore,5,'0'),120,456,mjCenter);
{    if keys[SDL_Scancode_Tab] then begin
      for i:=0 to 19 do
        for j:=0 to 11 do
          SmallFont.OutText(inttostr(fmap.GetTile(i,j)),i*32+16,j*32+56,mjCenter);
    end;}
    Curtain.Draw;
//    if keys[SDL_Scancode_A] then ScreenShot;
    if keys[SDL_SCANCODE_TAB] then begin
      for j:=0 to 11 do
        for i:=0 to 19 do
          MM.Fonts.ItemByName['Debug'].OutText(inttostr(fMap.Tiles[i,j]),i*32+16,j*32+10+48,1);
    end;
    Flip;
//    fPlayer.Clear;
    HandleMessages;
    if keys[OPTIONSKEY] then begin
      Options.Run(SOUND_SETTINGS);
    end;
    if Curtain.State=csIdle then begin
      fPlayer.Move;
      if fPlayer.Moved then inc(fScore,1);
      if fPlayer.PickedUpCargo then begin
        MM.Waves['Pickup']._wave.Play;
        dec(fGoodies);
        if fGoodies=0 then begin
          MM.Waves['DoorOpen']._wave.Play;
          fExit.Animation.Paused:=false;
          fMap.Tiles[fExit.X>>5,(fExit.Y-48)>>5]:=TILE_OPENEDEXIT;
        end;
        FreeAndNil(fSprites[fPlayer.CargoX,fPlayer.CargoY]);
//        fSprites[fPlayer.CargoX,fPlayer.CargoY]:=nil;
        fMap.Tiles[fPlayer.CargoX,fPlayer.CargoY]:=TILE_OCCUPIED;
      end;
    end;
  until keys[SDL_SCANCODE_ESCAPE] or fPlayer.ReachedExit or fPlayer.IsDead;
  Log.Trace('Play loop exited.');
  if keys[SDL_SCANCODE_ESCAPE] then Log.Trace('Escape is pressed.');
  if fPlayer.ReachedExit then Log.Trace('Player is reached exit.');
  if fPlayer.IsDead then Log.Trace('Player is dead.');
  if keys[SDL_SCANCODE_ESCAPE] then Result:=1
                               else Result:=0;
  if fPlayer.IsDead then Result:=2;
  if fPlayer.ReachedExit then begin
    if (fState in [0,1]) or (fBest>fScore) then begin
      fBest:=fScore;
      UpdateImage;
      VMU.SetMapState(fMapNo,fBest);
    end;
  end;
  Curtain.StartClose;
  if Result<>2 then begin
    if fMap.Congratulations then
      BASS_ChannelSlideAttribute(MM.Musics['Ending']._music.Handle, BASS_ATTRIB_VOL,0, 300)
    else
      BASS_ChannelSlideAttribute(MM.Musics['Ingame']._music.Handle, BASS_ATTRIB_VOL,0, 300);
  end;
  repeat
    fPlayer.Draw;
    Curtain.Draw;
    Flip;
  until Curtain.State=csFinished;
//  if Run=0 then fPlayer^.SaveSolution(st(fMapNo,2,'0')+'.sol');
  ClearSprites;
//  FreeAndNil(fMap);
end;

end.

