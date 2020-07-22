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

uses sysutils, Bass, sdl, mk_sdl, ImageUnit, MKToolbox, FontUnit,
  LTRN_SharedUnit, LTRN_OptionsUnit, LTRN_VMUUnit;

function TMapPlay.Play:integer;
var i,j:integer;
    fLogo:TImage;
begin
  fMap.Reset;
  fScore:=0;
  fLogo:=TImage.Create('logo.tga');
  PutImage(149,3,flogo);

  DrawMap;
  Curtain.StartOpen;
  repeat
    ClearScreen(0,0,0);
    PutImage(149,3,flogo);
    Fonts.FontByIndex[0].OutText('Steps:',120,434,mjCenter);
    Fonts.FontByIndex[1].OutText(st(fScore,5,'0'),120,456,mjCenter);
    Fonts.FontByIndex[0].OutText('Map:',520,434,mjCenter);
    Fonts.FontByIndex[1].OutText(st(fMapNo+1,2,'0'),520,456,mjCenter);
    Fonts.FontByIndex[0].OutText('Best:',320,434,mjCenter);
    if fBest=0 then Fonts.FontByIndex[2].OutText('None',320,456,mjCenter)
               else Fonts.FontByIndex[2].OutText(st(fBest,5,'0'),320,456,mjCenter);

    for i:=0 to 19 do
      for j:=0 to 11 do
        if fSprites[i,j]<>nil then fSprites[i,j].Draw;
//        if (i=4) and (j=2) and (fSprites[i,j]<>nil) then fSprites[i,j].LogSpriteData;
    fExit.Draw;
    fPlayer.Draw;
//    bar(50,456,189,479,0,0,0);
//    MultiFont[1].OutText(st(fScore,5,'0'),120,456,mjCenter);
{    if keys[SDLK_Tab] then begin
      for i:=0 to 19 do
        for j:=0 to 11 do
          SmallFont.OutText(inttostr(fmap.GetTile(i,j)),i*32+16,j*32+56,mjCenter);
    end;}
    Curtain.Draw;
//    if keys[SDLK_A] then ScreenShot;
    Flip;
//    fPlayer.Clear;
    HandleMessages;
    if keys[OptionsKey] then Options.Run;
    if Curtain.State=0 then begin
      fPlayer.Move;
      if fPlayer.Moved then inc(fScore,1);
      if fPlayer.PickedUpCargo then begin
        Waves['Pickup']._wave.Play;
        dec(fGoodies);
        if fGoodies=0 then begin
          Waves['DoorOpen']._wave.Play;
          fExit.Paused:=false;
          fMap.Tiles[fExit.X>>5,(fExit.Y-48)>>5]:=34;
        end;
        FreeAndNil(fSprites[fPlayer.CargoX,fPlayer.CargoY]);
//        fSprites[fPlayer.CargoX,fPlayer.CargoY]:=nil;
        fMap.Tiles[fPlayer.CargoX,fPlayer.CargoY]:=31;
      end;
    end;
  until keys[SDLK_Escape] or fPlayer.ReachedExit or fPlayer.IsDead;
  if keys[SDLK_Escape] then Result:=1
                       else Result:=0;
  if fPlayer.IsDead then Result:=2;
  if fPlayer.ReachedExit then begin
    if (fState in [0,1]) or (fBest>fScore) then begin
      fBest:=fScore;
      UpdateImage;
      VMU.SetMapState(fVMUSlot,fMapNo,fBest);
    end;
  end;
  Curtain.StartClose;
  if Result<>2 then begin
    if fMap.Congratulations then
      BASS_ChannelSlideAttribute(Muzax['Ending']._music.Handle, BASS_ATTRIB_VOL,0, 300)
    else
      BASS_ChannelSlideAttribute(Muzax['Ingame']._music.Handle, BASS_ATTRIB_VOL,0, 300);
  end;
  repeat
    fPlayer.Draw;
    Curtain.Draw;
    Flip;
  until Curtain.State=3;
//  if Run=0 then fPlayer^.SaveSolution(st(fMapNo,2,'0')+'.sol');
  ClearSprites;
//  FreeAndNil(fMap);
  FreeAndNil(fLogo);
end;

end.

