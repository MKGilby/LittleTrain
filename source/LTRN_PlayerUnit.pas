{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit LTRN_PlayerUnit;

interface

uses LTRN_MapListUnit, AnimatedSprite2Unit, Animation2Unit, LTRN_TrainPieceUnit;

type

  { TPlayer }

  TPlayer=class
    constructor Create(ix,iy:integer;iMap:TRawMap);
    destructor Destroy; override;
    procedure Draw;
//    procedure Clear;
    procedure Move;
    function PickedUpCargo:boolean;
//    function ReachedExit:boolean;
    function IsDead:boolean;
    function Moved:boolean;
    function CargoX:integer;
    function CargoY:integer;
    procedure AddWagon(iType:integer);
    procedure RemoveWagons;
    procedure SaveSolution(iFilename:string);
    procedure AddReplay(pReplayString:string);
  private
    fDeadAnimLeft,fDeadAnimRight,fDeadAnimUp,fDeadAnimDown:TAnimation;
    fDeadAnimFinalLeft,fDeadAnimFinalRight,
    fDeadAnimFinalUp,fDeadAnimFinalDown:TAnimation;
    fTrain:array of TTrainPiece;
    fPx,fPy:integer;
//    fMoveDelay,fSpeed,fSaveSpeed:integer;
    fMoveDelay,fSpeed:integer;
    fDirX,fDirY:integer;
    fOldDirX,fOldDirY:integer;
    fMap:TRawMap;
    fReplay:string;
    fPickedUp,fMoved:boolean;
    fState:(sPlaying,sExit0,sExit1,sExit2,sExit3);  // Exit0: Just hit exit, Exit1:wagons run into exit, Exit2:really finished.
    fDead:byte;
    fSolution:array of char;
    function fReachedExit:boolean;
    procedure fSetExit(value:boolean);
    function fGetWagonCount:integer;
    function fGetWagon(index:integer):TTrainPiece;
    function GetSpeed:integer;
    procedure LogState;
  public
    property WagonCount:integer read fGetWagonCount;
    property Wagons[index:integer]:TTrainPiece read fGetWagon;
    property ReachedExit:boolean read fReachedExit write fSetExit;
  end;
     
implementation
     
uses SysUtils, SDL2, mk_sdl2, Logger, MKToolbox, LTRN_SharedUnit, LTRN_VMUUnit;
     
constructor TPlayer.Create(ix,iy:integer; iMap:TRawMap);
begin
  fPx:=ix;
  fPy:=iy;
  SetLength(fTrain,1);
  fMap:=iMap;
  fDeadAnimLeft:=MM.Animations.ItemByName['c1L'].SpawnAnimation;
  fDeadAnimRight:=MM.Animations.ItemByName['c1'].SpawnAnimation;
  fDeadAnimUp:=MM.Animations.ItemByName['c1U'].SpawnAnimation;
  fDeadAnimDown:=MM.Animations.ItemByName['c1D'].SpawnAnimation;
  fDeadAnimFinalLeft:=MM.Animations.ItemByName['c2L'].SpawnAnimation;
  fDeadAnimFinalRight:=MM.Animations.ItemByName['c2'].SpawnAnimation;
  fDeadAnimFinalUp:=MM.Animations.ItemByName['c2U'].SpawnAnimation;
  fDeadAnimFinalDown:=MM.Animations.ItemByName['c2D'].SpawnAnimation;
  fTrain[0]:=TEngine.Create(ix<<5,iy<<5+48);
  if not fMap.Congratulations then
    fTrain[0].FaceRight
  else
    fTrain[0].FaceLeft;
  fDirX:=0;fOldDirX:=0;
  fDirY:=0;fOldDirY:=0;
//  if fMap.AutoPlay then Log.Trace('Autoplay!') else Log.Trace('Not autoplay!');
  fReplay:=fMap.Solution;
//  Log.Trace(fReplay);
{  case iSpeed of
    0:fSpeed:=32767;
    1:fSpeed:=60;
    2:fSpeed:=40;
    3:fSpeed:=30;
    4:fSpeed:=20;
    5:fSpeed:=15;
  end;}
  fSpeed:=10;
  fState:=sPlaying;
  fMoveDelay:=GetSpeed;
  fPickedUp:=false;
  fDead:=0;
  fMoved:=false;
  SetLength(fSolution,0);
end;

destructor TPlayer.Destroy;
var i:integer;
begin
  if Assigned(fDeadAnimFinalLeft) then FreeAndNil(fDeadAnimFinalLeft);
  if Assigned(fDeadAnimFinalRight) then FreeAndNil(fDeadAnimFinalRight);
  if Assigned(fDeadAnimFinalUp) then FreeAndNil(fDeadAnimFinalUp);
  if Assigned(fDeadAnimFinalDown) then FreeAndNil(fDeadAnimFinalDown);
  if Assigned(fDeadAnimDown) then FreeAndNil(fDeadAnimDown);
  if Assigned(fDeadAnimUp) then FreeAndNil(fDeadAnimUp);
  if Assigned(fDeadAnimRight) then FreeAndNil(fDeadAnimRight);
  if Assigned(fDeadAnimLeft) then FreeAndNil(fDeadAnimLeft);
  for i:=0 to length(fTrain)-1 do
    FreeAndNil(fTrain[i]);
  inherited ;
end;

procedure TPlayer.Draw;
var i:integer;
begin
  for i:=0 to length(fTrain)-1 do
    fTrain[i].Draw;
end;

procedure TPlayer.Move;
var tx,ty,ttx,tty,i:integer;
begin
  fTrain[0].Animation.Animate;
{  case fState of
    sPlaying:Log.Trace('Playing...');
    sExit0:Log.Trace('Exit0...');
    sExit1:Log.Trace('Exit1...');
    sExit2:Log.Trace('Exit2...');
    sExit3:Log.Trace('Exit3...');
  end;}
//  LogState;
  case fDead of
    0:begin
//        Log.Trace(fReplay);
        if fState=sExit2 then fState:=sExit3;
        if fMap.AutoPlay then begin
          if length(fReplay)>0 then begin
            Keys[SDL_SCANCODE_UP]:=fReplay[1]='U';
            Keys[SDL_SCANCODE_RIGHT]:=fReplay[1]='R';
            Keys[SDL_SCANCODE_DOWN]:=fReplay[1]='D';
            Keys[SDL_SCANCODE_LEFT]:=fReplay[1]='L';
          end else begin
            Keys[SDL_SCANCODE_UP]:=false;
            Keys[SDL_SCANCODE_RIGHT]:=false;
            Keys[SDL_SCANCODE_DOWN]:=false;
            Keys[SDL_SCANCODE_LEFT]:=false;
          end;
        end;
        if keys[SDL_SCANCODE_UP] and (fOldDirY<1) then begin fDirX:=0;fDirY:=-1;end;
        if keys[SDL_SCANCODE_RIGHT] and (fOldDirX>-1) then begin fDirX:=1;fDirY:=0;end;
        if keys[SDL_SCANCODE_DOWN] and (fOldDirY>-1) then begin fDirX:=0;fDirY:=1;end;
        if keys[SDL_SCANCODE_LEFT] and (fOldDirX<1) then begin fDirX:=-1;fDirY:=0;end;
        dec(fMoveDelay);
        if (VMU.Speed=0) and ((fDirX<>0) or (fDirY<>0)) then fMoveDelay:=0;
        if (fMoveDelay=0) then begin
          if (fDirX<>0) or (fDirY<>0) or (fState=sExit1) then begin
            if length(fReplay)>0 then delete(fReplay,1,1);
            fOldDirX:=fDirX;
            fOldDirY:=fDirY;
            SetLength(fSolution,length(fSolution)+1);
            if fDirX=-1 then begin
              fTrain[0].FaceLeft;
              fSolution[length(fSolution)-1]:='L';
            end;
            if fDirX=1 then begin
              fTrain[0].FaceRight;
              fSolution[length(fSolution)-1]:='R';
            end;
            if fDirY=-1 then begin
              fTrain[0].FaceUp;
              fSolution[length(fSolution)-1]:='U';
            end;
            if fDirY=1 then begin
              fTrain[0].FaceDown;
              fSolution[length(fSolution)-1]:='D';
            end;
            fPx+=fDirX;
            fPy+=fDirY;
            if fMap.Tiles[fPx,fPy]=TILE_OPENEDEXIT then begin
              fState:=sExit0;
              fSpeed:=10;
              MM.Waves['Complete']._wave.Play;
            end else
            if fMap.Tiles[fPx,fPy]=TILE_SIGNAL then begin
              fPx-=fDirX;
              fPy-=fDirY;
              if fDirX=1 then
                fTrain[0].FaceRight
              else if fDirX=-1 then
                fTrain[0].FaceLeft
              else if fDirY=1 then
                fTrain[0].FaceDown
              else if fDirY=-1 then
                fTrain[0].FaceUp;
              fDirX:=0;
              fDirY:=0;
              fState:=sExit2;
            end else
            if (fMap.Tiles[fPx,fPy] in [TILE_OCCUPIED,TILE_CLOSEDEXIT,TILE_WALL]) and (fState=sPlaying) then begin
              MM.Waves['Explosion']._wave.Play;
              fDead:=1;
              TEngine(fTrain[0]).Puffing:=false;
              if fDirX=1 then
                fTrain[0].SetAnimation(fDeadAnimRight,true)
              else if fDirX=-1 then
                fTrain[0].SetAnimation(fDeadAnimLeft,true)
              else if fDirY=1 then
                fTrain[0].SetAnimation(fDeadAnimDown,true)
              else if fDirY=-1 then
                fTrain[0].SetAnimation(fDeadAnimUp,true);
  //            Log.Trace(fTrain[0]._sprite.FrameDelay);
//              Log.Trace(Format('Dead1. dX=%d, dY=%d',[fDirX,fDirY]));
//              fTrain[0].LogSpriteData;
              exit;
            end;
            if (fState=sExit1) and (fSpeed>2) then dec(fSpeed);
            if (fDirX<>0) or (fDirY<>0) or (fState=sExit1) then begin
              fMap.Tiles[fTrain[length(fTrain)-1].X>>5,
                        (fTrain[length(fTrain)-1].Y-48)>>5]:=TILE_EMPTY;
              if fMap.Tiles[fPx,fPy]>96 then begin
//                Log.Trace(Format('Pickup x,y,type: %d, %d, %d',[fPx,fPy,fMap.Tiles[fPx,fPy]-32]));
                fPickedUp:=true;
                AddWagon(fMap.Tiles[fPx,fPy]-32);
              end;
              tx:=fTrain[0].X;
              ty:=fTrain[0].Y;
              if fState in [sPlaying,sExit0] then begin
                fTrain[0].MoveRel(fDirX*32,fDirY*32);
                if (fDirX<>0) or (fDirY<>0) then fMoved:=true;
                fMap.Tiles[tx>>5+fDirX,(ty-48)>>5+fDirY]:=TILE_OCCUPIED;
                if fState=sExit0 then begin
                  fState:=sExit1;
                  fDirX:=0;
                  fDirY:=0;
                end;
              end;
              for i:=1 to length(fTrain)-1 do
                if (fTrain[i].X<>fTrain[0].X) or (fTrain[i].Y<>fTrain[0].Y) or (fState=sPlaying) then begin
                  ttx:=fTrain[i].X;
                  tty:=fTrain[i].Y;
                  fTrain[i].X:=tx;
                  fTrain[i].Y:=ty;
                  if ty<tty then
                    fTrain[i].FaceUp
                  else if ty>tty then
                    fTrain[i].FaceDown
                  else if tx<ttx then
                    fTrain[i].FaceLeft
                  else if tx>ttx then
                    fTrain[i].FaceRight;
                  tx:=ttx;
                  ty:=tty;
                end;
//              Log.Trace(Format('Length: %d - T[0](x,y): (%d,%d) - T[last](x,y): (%d,%d)',
//                [length(fTrain),fTrain[0]._sprite.X,fTrain[0]._sprite.Y,fTrain[length(fTrain)-1]._sprite.X,fTrain[length(fTrain)-1]._sprite.Y]));
              // What the heck is this?
              // This one signals the game loop to break when after exiting all
              // the wagons reached the exit. Added the state checking.
              if (fState=sExit1) and (length(fTrain)>1) and
                 (fTrain[length(fTrain)-1].X=fTrain[0].X) and
                 (fTrain[length(fTrain)-1].Y=fTrain[0].Y) then
                   fState:=sExit2;
            end;
            if VMU.Speed=0 then begin
              fDirX:=0;fDirY:=0;
              keys[SDL_SCANCODE_UP]:=false;
              keys[SDL_SCANCODE_LEFT]:=false;
              keys[SDL_SCANCODE_DOWN]:=false;
              keys[SDL_SCANCODE_RIGHT]:=false;
            end;
          end;
          if fState=sPlaying then fMoveDelay:=GetSpeed else fMoveDelay:=fSpeed;
        end;
      end;
    1:begin
        if fTrain[0].Animation.Finished then begin
          fDead:=2;
//          fTrain[0].SetAnimation(fDeadAnimFinal,true);
          if fDirX=1 then
            fTrain[0].SetAnimation(fDeadAnimFinalRight,true)
          else if fDirX=-1 then
            fTrain[0].SetAnimation(fDeadAnimFinalLeft,true)
          else if fDirY=1 then
            fTrain[0].SetAnimation(fDeadAnimFinalDown,true)
          else if fDirY=-1 then
            fTrain[0].SetAnimation(fDeadAnimFinalUp,true);
        end;
      end;
  end;
end;

function TPlayer.PickedUpCargo:boolean;
begin
  PickedUpCargo:=fPickedUp;
  fPickedUp:=false;
end;

function TPlayer.CargoX:integer;
begin
  CargoX:=fPx;
end;

function TPlayer.CargoY:integer;
begin
  CargoY:=fPy;
end;

procedure TPlayer.AddWagon(iType:integer);
begin
//  LogState;
  SetLength(fTrain,length(fTrain)+1);
  case fTrain[length(fTrain)-2].Dir of
    'U':fTrain[length(fTrain)-1]:=TTrainPiece.Create(fTrain[length(fTrain)-2].X,fTrain[length(fTrain)-2].Y+32,chr(iType));
    'D':fTrain[length(fTrain)-1]:=TTrainPiece.Create(fTrain[length(fTrain)-2].X,fTrain[length(fTrain)-2].Y-32,chr(iType));
    'L':fTrain[length(fTrain)-1]:=TTrainPiece.Create(fTrain[length(fTrain)-2].X+32,fTrain[length(fTrain)-2].Y,chr(iType));
    'R':fTrain[length(fTrain)-1]:=TTrainPiece.Create(fTrain[length(fTrain)-2].X-32,fTrain[length(fTrain)-2].Y,chr(iType));
  end;
  with fTrain[Length(fTrain)-2] do fMap.Tiles[X>>5,(Y-48)>>5]:=TILE_OCCUPIED;
//  LogState;
//  fTrain[length(fTrain)-1]._sprite.FrameDelay:=6;
end;

function TPlayer.IsDead:boolean;
begin
  IsDead:=(fDead=2) and (keys[SDL_SCANCODE_SPACE] or keys[SDL_SCANCODE_UP] or keys[SDL_SCANCODE_DOWN] or keys[SDL_SCANCODE_LEFT] or keys[SDL_SCANCODE_RIGHT]);
end;

function TPlayer.Moved:boolean;
begin
  Moved:=fMoved;
  fMoved:=false;
end;

procedure TPlayer.SaveSolution(iFilename:string);
var f:file;i:integer;
begin
  assign(f,iFilename);
  rewrite(f,1);
  for i:=0 to length(fSolution)-1 do
    blockwrite(f,fSolution[i],1);
  close(f);
end;

procedure TPlayer.RemoveWagons;
begin
  SetLength(fTrain,1);
end;

function TPlayer.fGetWagonCount:integer;
begin
  Result:=length(fTrain);
end;

function TPlayer.fGetWagon(index:integer):TTrainPiece;
begin
  if (index>=0) and (index<length(fTrain)) then
    Result:=fTrain[index]
  else
    Result:=nil;
end;

function TPlayer.GetSpeed:integer;
begin
  case VMU.Speed of
    0:Result:=32767;
    1:Result:=60;
    2:Result:=40;
    3:Result:=30;
    4:Result:=20;
    5:Result:=15;
    else Result:=30;
  end;
end;

procedure TPlayer.LogState;
var s1,s2,s3,s4:string;i,j:integer;
begin
  s1:='Type ';
  s2:='Dir  ';
  s3:='X    ';
  s4:='Y    ';
  for i:=0 to length(fTrain)-1 do begin
    s1+=' '+fTrain[i].PieceType+'  ';
    s2+=' '+fTrain[i].Dir+'  ';
    s3+=st(fTrain[i].X,3,' ')+' ';
    s4+=st(fTrain[i].Y,3,' ')+' ';
  end;
  Log.LogDebug('--- Train and map dump starts. ---');
  Log.LogDebug(Format('Train length: %d',[length(fTrain)]));
  Log.LogDebug(s1);
  Log.LogDebug(s2);
  Log.LogDebug(s3);
  Log.LogDebug(s4);
  Log.LogDebug('Map dump');
  for j:=0 to 11 do begin
    s1:='';
    for i:=0 to 19 do s1+=st(fMap.Tiles[i,j],3,' ')+' ';
    Log.LogDebug(s1);
  end;
  Log.LogDebug('--- Train and map dump ends. ---');
end;

procedure TPlayer.AddReplay(pReplayString:string);
begin
  fReplay:=pReplayString;
end;

function TPlayer.fReachedExit:boolean;
begin
  Result:=(fState=sExit3);
end;

procedure TPlayer.fSetExit(value:boolean);
begin
  if value then fState:=sExit2 else fState:=sPlaying;
end;

end.
