unit LTRN_MapCongratsUnit;

{$mode delphi}

interface

uses
  LTRN_MapBaseUnit, AnimatedSpriteUnit, Lists, LTRN_PlayerUnit;

type
  TWagons=TGenericList<TAnimatedSprite>;

  TBlueEngine=class
    constructor Create(iPlayer:TPlayer);
    destructor Destroy; override;
    procedure Draw;
    procedure Move;
    procedure Start;
    procedure Reset;
  private
    fPlayer:TPlayer;
    fSprite:TAnimatedSprite;
    fWagons:TWagons;
    fState:integer;  // 0-Waiting to start, 1-Coming in, 2-Waiting a bit, 3-Going out, 4-Finished
    fCounter:integer;  // Tick count within the current state
    function fGetFinished:boolean;
  public
    property Finished:boolean read fGetFinished;
  end;

  TMapCongrats=class(TMapBase)
    function Play:integer; override; // Result: 0-Completed, 1-Escaped, 2-Dead
  end;

implementation

uses sdl, mk_sdl, ImageUnit, MKFonts, FontCollectionUnit, MusicCollectionUnit,
  WaveCollectionUnit, LTRN_SharedUnit, LTRN_OptionsUnit, Bass,
  Logger, SysUtils;

//                                                         +-------------+
// ------------------------------------------------------= | TBlueEngine | =---
//                                                         +-------------+

constructor TBlueEngine.Create(iPlayer:TPlayer);
begin
  fSprite:=TAnimatedSprite.Create(19*32,7*32+48,Animations['*']);
  fSprite.Visible:=false;  // Initially it is invisible. Call Start to start sequence.
  fPlayer:=iPlayer;
  fWagons:=TWagons.Create;
end;

destructor TBlueEngine.Destroy;
begin
  FreeAndNil(fWagons);
  FreeAndNil(fSprite);
  inherited ;
end;

procedure TBlueEngine.Draw;
var i:integer;
begin
  fSprite.Draw;
  for i:=0 to fWagons.Count-1 do fWagons[i].Draw;
end;

procedure TBlueEngine.Move;
var i:integer;
begin
//  Log.Trace(Format('State(cnt): %d (%d) - Engine pos: %d,%d - CurrFrame: %d',[fState,fCounter,fSprite.X,fSprite.Y,fSprite.CurrentFrameIndex]));
  case fState of
    1:begin   // Coming in
        inc(fCounter);
        if fCounter mod 40=0 then begin
          fSprite.MoveRel(-32,0);
          if fSprite.X=15*32 then begin
            fCounter:=0;
            inc(fState);
          end;
        end;
      end;
    2:begin   // Waiting a little bit
        inc(fCounter);
        if fCounter=2*60 then begin
          fCounter:=0;
          inc(fState);
          fSprite.ReverseAnim:=false;
          // Copy wagon sprites from black engine to blue.
          for i:=1 to fPlayer.WagonCount-1 do
            fWagons.Add(fPlayer.Wagons[i]._sprite);
          fPlayer.RemoveWagons;
        end;
      end;
    3:begin   // Going out
        inc(fCounter);
        if fCounter mod 50=0 then begin
          if fSprite.Visible then fSprite.MoveRel(32,0);
          if fSprite.X=20*32 then fSprite.Visible:=false;
          for i:=0 to fWagons.Count-1 do begin
            fWagons[i].MoveRel(32,0);
            if fWagons[i].X=20*32 then fWagons[i].Visible:=false;
          end;
          if not fWagons[0].Visible then begin
            fCounter:=0;
            inc(fState);
          end;
        end;
      end;
  end;
end;

procedure TBlueEngine.Start;
begin
  if fState=0 then begin
    fSprite.MoveTo(19*32,7*32+48);
    fSprite.ReverseAnim:=true;
    fSprite.Visible:=true;
    fWagons.Clear;
    fState:=1;
    fCounter:=0;
  end;
end;

procedure TBlueEngine.Reset;
begin
  fState:=0;
end;

function TBlueEngine.fGetFinished:boolean;
begin
  Result:=fState=4;
end;

//                                                        +--------------+
// -----------------------------------------------------= | TCongratsMap | =---
//                                                        +--------------+

function TMapCongrats.Play:integer;
var i,j:integer;
    fLogo:TImage;
    BlueEngine:TBlueEngine;
    fState,fCounter:integer;
begin
  fMap.Reset;
  fScore:=0;
  fLogo:=TImage.Create('logo.tga');
  PutImage(140,0,flogo);

  DrawMap;
  BlueEngine:=TBlueEngine.Create(fPlayer);
  fState:=2;fCounter:=0;
  Curtain.StartOpen;

  repeat
    ClearScreen(0,0,0);
    PutImage(140,0,flogo);
    FC.FontByIndex[3].OutText('You completed the game!',320,368,mjCenter);
    FC.FontByIndex[1].OutText('I hope you enjoyed playing it,',320,412,mjCenter);
    FC.FontByIndex[0].OutText('as much as I enjoyed making it!',320,434,mjCenter);
    for i:=0 to 19 do
      for j:=0 to 11 do
        if fSprites[i,j]<>nil then fSprites[i,j].Draw;
    fExit.Draw;
    fPlayer.Draw;
    BlueEngine.Draw;
    Curtain.Draw;
//    if keys[SDLK_A] then ScreenShot;
    Flip;

    HandleMessages;
    if keys[OptionsKey] then Options.Run;
    if Curtain.State=0 then begin
      case fState of
        2:begin  // Wait a little bit
            inc(fCounter);
            if fCounter=60*2 then begin
              inc(fState);
              fCounter:=0;
            end;
          end;
        3:begin  // Set the signal to green, and wait a little bit
            if fCounter=0 then begin
              fExit.SetAnimation(Animations['-']);
              fMap.Tiles[fExit.X>>5,(fExit.Y-48)>>5]:=32;
            end;
            inc(fCounter);
            if fCounter=60 then begin
              inc(fState);
              fCounter:=0;
            end;
          end;
        4:begin  // Move around, pick up cargo and come back to signal
            inc(fCounter);
            if fCounter=150 then begin
              fExit.SetAnimation(Animations['=']);
              fMap.Tiles[fExit.X>>5,(fExit.Y-48)>>5]:=61;
            end;
            fPlayer.Move;
            if fPlayer.PickedUpCargo then begin
              WC['Pickup']._wave.Play;
              FreeAndNil(fSprites[fPlayer.CargoX,fPlayer.CargoY]);
              fMap.Tiles[fPlayer.CargoX,fPlayer.CargoY]:=31;
            end;
            if fPlayer.ReachedExit then begin
              fPlayer.ReachedExit:=false;
              inc(fState);
              fCounter:=0;
            end;
          end;
        5:begin  // Wait a little bit then start the blue engine
            inc(fCounter);
            if fCounter=60*4 then begin
              inc(fState);
              fCounter:=0;
              BlueEngine.Start;
            end;
          end;
        6:begin  // Wait until the blue engine finishes
            inc(fCounter);
            BlueEngine.Move;
            if BlueEngine.Finished then begin
              inc(fState);
              fCounter:=0;
            end;
          end;
      end;

    end;
  until keys[SDLK_Escape] or fPlayer.ReachedExit or fPlayer.IsDead;

  if keys[SDLK_Escape] then Result:=1
                       else Result:=0;
  if fPlayer.IsDead then Result:=2;
  Curtain.StartClose;
  if Result<>2 then begin
    BASS_ChannelSlideAttribute(MC['Ending']._music.Handle, BASS_ATTRIB_VOL,0, 300)
  end;
  repeat
    fPlayer.Draw;
    Curtain.Draw;
    Flip;
  until Curtain.State=3;
//  if Run=0 then fPlayer^.SaveSolution(st(fMapNo,2,'0')+'.sol');
  ClearSprites;
//  FreeAndNil(fMap);
  FreeAndNil(BlueEngine);
  FreeAndNil(fLogo);
  Result:=1;
end;

end.

