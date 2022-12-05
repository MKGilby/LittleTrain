unit LTRN_MapCongratsUnit;

{$mode delphi}

interface

uses
  fgl, LTRN_MapBaseUnit, AnimatedSprite2Unit, LTRN_PlayerUnit, Animation2Unit;

type
  TWagons=TFPGObjectList<TAnimatedSprite>;

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
    fPuffing1,fPuffing2:TAnimation;
    fWagons:TWagons;
    fState:integer;  // 0-Waiting to start, 1-Coming in, 2-Waiting a bit, 3-Going out, 4-Finished
    fCounter:integer;  // Tick count within the current state
    function fGetFinished:boolean;
  public
    property Finished:boolean read fGetFinished;
  end;

  TMapCongrats=class(TMapBase)
    function Play:integer; override; // Result: 0-Completed, 1-Escaped, 2-Dead
  private
    procedure CreatePresentsAndPath;
  end;

implementation

uses SysUtils, Bass, Logger, Font2Unit, sdl2, mk_sdl2,
  LTRN_SharedUnit, LTRN_OptionsUnit, LTRN_CurtainUnit;

//                                                         +-------------+
// ------------------------------------------------------= | TBlueEngine | =---
//                                                         +-------------+

constructor TBlueEngine.Create(iPlayer:TPlayer);
begin
  fSprite:=TAnimatedSprite.Create(19*32,7*32+48,MM.Animations.ItemByName['*'].SpawnAnimation);
  fSprite.Visible:=false;  // Initially it is invisible. Call Start to start sequence.
  fPuffing1:=MM.Animations.ItemByName['~'].SpawnAnimation;
  fPuffing2:=MM.Animations.ItemByName['@'].SpawnAnimation;
  fPlayer:=iPlayer;
  fWagons:=TWagons.Create;
end;

destructor TBlueEngine.Destroy;
begin
  if Assigned(fPuffing2) then FreeAndNil(fPuffing2);
  if Assigned(fPuffing1) then FreeAndNil(fPuffing1);
  if Assigned(fWagons) then FreeAndNil(fWagons);
  if Assigned(fSprite) then FreeAndNil(fSprite);
  inherited ;
end;

procedure TBlueEngine.Draw;
var i:integer;
begin
  fSprite.Draw;
  if fSprite.Visible then begin
    if fState=1 then begin
      fPuffing2.PutFrame(fSprite.X,fSprite.Y);
      fPuffing2.Animate;
    end else begin
      fPuffing1.PutFrame(fSprite.X,fSprite.Y);
      fPuffing1.Animate;
    end;
  end;
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
        if fCounter=20 then begin
          fCounter:=0;
          inc(fState);
          fSprite.Animation.ReverseAnim:=false;
          // Copy wagon sprites from black engine to blue.
          for i:=1 to fPlayer.WagonCount-1 do
            fWagons.Add(fPlayer.Wagons[i]);
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
    fSprite.Animation.ReverseAnim:=true;
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
    BlueEngine:TBlueEngine;
    fState,fCounter:integer;
begin
  fMap.Reset;
  fScore:=0;
  Logo.Draw;

  DrawMap;
  BlueEngine:=TBlueEngine.Create(fPlayer);
  fState:=2;fCounter:=0;
  Curtain.StartOpen;

  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    Logo.Draw;
    MM.Fonts['3'].OutText('You completed the game!',320,368,mjCenter);
    MM.Fonts['1'].OutText('I hope you enjoyed playing it,',320,412,mjCenter);
    MM.Fonts['0'].OutText('as much as I enjoyed making it!',320,434,mjCenter);
    for i:=0 to 19 do
      for j:=0 to 11 do
        if fSprites[i,j]<>nil then fSprites[i,j].Draw;
    fExit.Draw;
    fPlayer.Draw;
    BlueEngine.Draw;
    Curtain.Draw;
//    if keys[SDL_scancode_A] then ScreenShot;
    Flip;

    HandleMessages;
    if keys[OPTIONSKEY] then Options.Run;
    if Curtain.State=csIdle then begin
      case fState of
        0:begin  // Wait a little bit
            inc(fCounter);
            if fCounter=60 then begin
              inc(fState);
              fCounter:=0;
            end;
          end;
        1:begin  // Create new presents and path
            inc(fCounter);
            if fCounter=10 then begin
              CreatePresentsAndPath;
              inc(fState);
              fCounter:=0;
            end;
          end;
        2:begin  // Wait a little bit
            inc(fCounter);
            if fCounter=60 then begin
              inc(fState);
              fCounter:=0;
            end;
          end;
        3:begin  // Set the signal to green, and wait a little bit
            if fCounter=0 then begin
              fExit.SetAnimation(MM.Animations.ItemByName['-'].SpawnAnimation);
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
            if fCounter=135 then begin
              fExit.SetAnimation(MM.Animations.ItemByName['='].SpawnAnimation);
              fMap.Tiles[fExit.X>>5,(fExit.Y-48)>>5]:=61;
            end;
            fPlayer.Move;
            if fPlayer.PickedUpCargo then begin
              MM.Waves['Pickup']._wave.Play;
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
              for i:=6 to 18 do fMap.Tiles[i,7]:=32;
              BlueEngine.Reset;
              fState:=0;
              fCounter:=0;
            end;
          end;
      end;

    end;
  until keys[SDL_SCANCODE_ESCAPE] or fPlayer.ReachedExit or fPlayer.IsDead;

  if keys[SDL_SCANCODE_ESCAPE] then Result:=1
                       else Result:=0;
  if fPlayer.IsDead then Result:=2;
  Curtain.StartClose;
  if Result<>2 then begin
    BASS_ChannelSlideAttribute(MM.Musics['Ending']._music.Handle, BASS_ATTRIB_VOL,0, 300)
  end;
  repeat
    fPlayer.Draw;
    Curtain.Draw;
    Flip;
  until Curtain.State=csFinished;
//  if Run=0 then fPlayer^.SaveSolution(st(fMapNo,2,'0')+'.sol');
  ClearSprites;
//  FreeAndNil(fMap);
  FreeAndNil(BlueEngine);
  Result:=1;
end;

procedure TMapCongrats.CreatePresentsAndPath;
var
  path,s:string;i,j,preY:integer;
  cols:array[0..17] of integer;
begin
  path:='LLLLLUU';
  for i:=0 to 17 do cols[i]:=0;
  for i:=0 to 7 do begin
    repeat
      j:=random(17)+1;
    until cols[j]=0;
    cols[j]:=random(4)+2;
  end;
  preY:=5;
  s:='';
  j:=ord('s');
  for i:=0 to 17 do begin
    if cols[i]=0 then
      s+='R'
    else begin
      while preY<cols[i] do begin path+='D';inc(preY);end;
      while preY>cols[i] do begin path+='U';dec(preY);end;
      path+=s;
      s:='R';
      fMap.Tiles[i+1,cols[i]]:=j;
      fSprites[i+1,cols[i]]:=TAnimatedSprite.Create((i+1)<<5,cols[i]<<5+48,MM.Animations.ItemByName[chr(j)].SpawnAnimation);
      inc(j);
    end;
  end;
  path+=s;
  delete(path,length(path),1);
  while preY<7 do begin path+='D';inc(preY);end;
  path+='LLLLLLLLLLLL';
  fPlayer.AddReplay(path);
end;

end.

