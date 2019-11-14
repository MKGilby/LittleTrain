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

uses sysutils, sdl, mk_sdl, ImageUnit, FontCollectionUnit, MKFonts, MKToolbox,
  LTRN_SharedUnit, LTRN_OptionsUnit, WaveCollectionUnit, LTRN_VMUUnit, Bass,
  MusicCollectionUnit;

function TMapPlay.Play:integer;
var i,j:integer;
    fLogo:TImage;
begin
  fMap.Reset;
  fScore:=0;
  fLogo:=TImage.Create('logo.tga');
  PutImage(140,0,flogo);

  DrawMap;
  Curtain.StartOpen;
  repeat
    ClearScreen(0,0,0);
    PutImage(140,0,flogo);
    FC.FontByIndex[0].OutText('Steps:',120,434,mjCenter);
    FC.FontByIndex[1].OutText(st(fScore,5,'0'),120,456,mjCenter);
    FC.FontByIndex[0].OutText('Map:',520,434,mjCenter);
    FC.FontByIndex[1].OutText(st(fMapNo+1,2,'0'),520,456,mjCenter);
    FC.FontByIndex[0].OutText('Best:',320,434,mjCenter);
    if fBest=0 then FC.FontByIndex[2].OutText('None',320,456,mjCenter)
               else FC.FontByIndex[2].OutText(st(fBest,5,'0'),320,456,mjCenter);

    for i:=0 to 19 do
      for j:=0 to 11 do
        if fSprites[i,j]<>nil then fSprites[i,j].Draw;
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
        WC['Pickup']._wave.Play;
        dec(fGoodies);
        if fGoodies=0 then begin
          WC['DoorOpen']._wave.Play;
          for i:=0 to 19 do
            for j:=0 to 11 do
              if fMap.Tiles[i,j]=33 then begin
                fSprites[i,j].Paused:=false;
                fMap.Tiles[i,j]:=34;
              end;
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
    WC['Complete']._wave.Play;
    if (fState in [0,1]) or (fBest>fScore) then begin
      fBest:=fScore;
      UpdateImage;
      VMU.SetMapState(fVMUSlot,fMapNo,fBest);
    end;
  end;
  Curtain.StartClose;
  if Result<>2 then begin
    if fMap.Congratulations then
      BASS_ChannelSlideAttribute(MC['Ending']._music.Handle, BASS_ATTRIB_VOL,0, 300)
    else
      BASS_ChannelSlideAttribute(MC['Ingame']._music.Handle, BASS_ATTRIB_VOL,0, 300);
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

