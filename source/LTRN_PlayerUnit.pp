{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit LTRN_PlayerUnit;

interface

uses LTRN_MapListUnit, AnimatedSpriteUnit;

type
  TTrainPiece=record
    _sprite:TAnimatedSprite;
    _type:char;
    _dir:char;
  end;
  
  TPlayer=class
    constructor Create(ix,iy,iSpeed:integer;iMap:TRawMap);
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
  private
    fTrain:array of TTrainPiece;
    fPx,fPy:integer;
    fMoveDelay,fSpeed:integer;
    fDirX,fDirY:integer;
    fOldDirX,fOldDirY:integer;
    fMap:TRawMap;
    fReplay:string;
    fPickedUp,fMoved,fExit:boolean;
    fDead:byte;
    fSolution:array of char;
    function fGetWagonCount:integer;
    function fGetWagon(index:integer):TTrainPiece;
  public
    property WagonCount:integer read fGetWagonCount;
    property Wagons[index:integer]:TTrainPiece read fGetWagon;
    property ReachedExit:boolean read fExit write fExit;
  end;
     
implementation
     
uses SysUtils, SDL, MK_SDL, WaveCollectionUnit, Logger, LTRN_SharedUnit;
     
constructor TPlayer.Create(ix,iy,iSpeed:integer;iMap:TRawMap);
begin
  fPx:=ix;
  fPy:=iy;
  SetLength(fTrain,1);
  fMap:=iMap;
  if not fMap.Congratulations then
    fTrain[0]._sprite:=TAnimatedSprite.Create(ix<<5,iy<<5+48,Animations['$'])
  else
    fTrain[0]._sprite:=TAnimatedSprite.Create(ix<<5,iy<<5+48,Animations['$L']);
  fTrain[0]._type:='R';
  fDirX:=0;fOldDirX:=0;
  fDirY:=0;fOldDirY:=0;
//  if fMap.AutoPlay then Log.Trace('Autoplay!') else Log.Trace('Not autoplay!');
  fReplay:=fMap.Solution;
  Log.Trace(fReplay);
  case iSpeed of
    0:fSpeed:=32767;
    1:fSpeed:=60;
    2:fSpeed:=40;
    3:fSpeed:=30;
    4:fSpeed:=20;
    5:fSpeed:=15;
  end;
  fMoveDelay:=fSpeed;
  fPickedUp:=false;
  fExit:=false;
  fDead:=0;
  fMoved:=false;
  SetLength(fSolution,0);
end;

destructor TPlayer.Destroy;
var i:integer;
begin
  for i:=0 to length(fTrain)-1 do
    FreeAndNil(fTrain[i]._sprite);
  inherited ;
end;

procedure TPlayer.Draw;
var i:integer;
begin
  for i:=0 to length(fTrain)-1 do
    fTrain[i]._sprite.Draw;
end;

{procedure TPlayer.Clear;
var i:integer;
begin
  for i:=0 to length(fTrain)-1 do
    fTrain[i]._sprite.Clear;
end;}

procedure TPlayer.Move;
var tx,ty,ttx,tty,i:integer;
begin
  case fDead of
    0:begin
//        Log.Trace(fReplay);
        if fMap.AutoPlay then begin
          if length(fReplay)>0 then begin
            Keys[SDLK_UP]:=fReplay[1]='U';
            Keys[SDLK_RIGHT]:=fReplay[1]='R';
            Keys[SDLK_DOWN]:=fReplay[1]='D';
            Keys[SDLK_LEFT]:=fReplay[1]='L';
          end else begin
            Keys[SDLK_UP]:=false;
            Keys[SDLK_RIGHT]:=false;
            Keys[SDLK_DOWN]:=false;
            Keys[SDLK_LEFT]:=false;
          end;
        end;
        if keys[SDLK_Up] and (fOldDirY<1) then begin fDirX:=0;fDirY:=-1;end;
        if keys[SDLK_Right] and (fOldDirX>-1) then begin fDirX:=1;fDirY:=0;end;
        if keys[SDLK_Down] and (fOldDirY>-1) then begin fDirX:=0;fDirY:=1;end;
        if keys[SDLK_Left] and (fOldDirX<1) then begin fDirX:=-1;fDirY:=0;end;
        dec(fMoveDelay);
        if (fSpeed=32767) and ((fDirX<>0) or (fDirY<>0)) then fMoveDelay:=0;
        if (fMoveDelay=0) then begin
          if (fDirX<>0) or (fDirY<>0) then begin
            if length(fReplay)>0 then delete(fReplay,1,1);
            fOldDirX:=fDirX;
            fOldDirY:=fDirY;
            SetLength(fSolution,length(fSolution)+1);
            if fDirX=-1 then begin
              fTrain[0]._sprite.SetAnimation(Animations['%L']);
//              fTrain[0]._sprite.Image:=Sprites.FindImage('%L');
              fTrain[0]._dir:='L';
              fSolution[length(fSolution)-1]:='L';
            end;
            if fDirX=1 then begin
              fTrain[0]._sprite.SetAnimation(Animations['%']);
//              fTrain[0]._sprite.Image:=Sprites.FindImage('%');
              fTrain[0]._dir:='R';
              fSolution[length(fSolution)-1]:='R';
            end;
            if fDirY=-1 then begin
              fTrain[0]._sprite.SetAnimation(Animations['%U']);
//              fTrain[0]._sprite.Image:=Sprites.FindImage('%U');
              fTrain[0]._dir:='U';
              fSolution[length(fSolution)-1]:='U';
            end;
            if fDirY=1 then begin
              fTrain[0]._sprite.SetAnimation(Animations['%D']);
//              fTrain[0]._sprite.Image:=Sprites.FindImage('%D');
              fTrain[0]._dir:='D';
              fSolution[length(fSolution)-1]:='D';
            end;
            fPx+=fDirX;
            fPy+=fDirY;
            if fMap.Tiles[fPx,fPy]=34 then begin
              fExit:=true;
            end else
            if fMap.Tiles[fPx,fPy]=61 then begin
              fPx-=fDirX;
              fPy-=fDirY;
              if fDirX=1 then
                fTrain[0]._sprite.SetAnimation(Animations['$'])
              else if fDirX=-1 then
                fTrain[0]._sprite.SetAnimation(Animations['$L'])
              else if fDirY=1 then
                fTrain[0]._sprite.SetAnimation(Animations['$D'])
              else if fDirY=-1 then
                fTrain[0]._sprite.SetAnimation(Animations['$U']);
              fDirX:=0;
              fDirY:=0;
              fExit:=true;
            end else
            if fMap.Tiles[fPx,fPy] in [31,33,35] then begin
              WC['Explosion']._wave.Play;
              fDead:=1;
              if fDirX=1 then
                fTrain[0]._sprite.SetAnimation(Animations['c1'],true)
              else if fDirX=-1 then
                fTrain[0]._sprite.SetAnimation(Animations['c1L'],true)
              else if fDirY=1 then
                fTrain[0]._sprite.SetAnimation(Animations['c1D'],true)
              else if fDirY=-1 then
                fTrain[0]._sprite.SetAnimation(Animations['c1U'],true);
  //            Log.Trace(fTrain[0]._sprite.FrameDelay);
              exit;
            end;
            if (fDirX<>0) or (fDirY<>0) then begin
//              fMap.Tiles[fTrain[length(fTrain)-1]._sprite.X>>5,
//                         (fTrain[length(fTrain)-1]._sprite.Y-48)>>5]:=32;
              if fMap.Tiles[fPx,fPy]>96 then begin
                fPickedUp:=true;
                AddWagon(fMap.Tiles[fPx,fPy]-32);
              end;
              tx:=fTrain[0]._sprite.X;
              ty:=fTrain[0]._sprite.Y;
              fTrain[0]._sprite.MoveRel(fDirX*32,fDirY*32);
              if (fDirX<>0) or (fDirY<>0) then fMoved:=true;
              fMap.Tiles[tx>>5+fDirX,(ty-48)>>5+fDirY]:=31;
              for i:=1 to length(fTrain)-1 do begin
                ttx:=fTrain[i]._sprite.X;
                tty:=fTrain[i]._sprite.Y;
                fTrain[i]._sprite.X:=tx;
                fTrain[i]._sprite.Y:=ty;
                if ty<tty then begin
                  fTrain[i]._sprite.SetAnimation(Animations[fTrain[i]._type+'U']);
//                  fTrain[i]._sprite.Image:=Sprites.FindImage(fTrain[i]._type+'U');
                  fTrain[i]._dir:='U';
                end;
                if ty>tty then begin
                  fTrain[i]._sprite.SetAnimation(Animations[fTrain[i]._type+'D']);
//                  fTrain[i]._sprite.Image:=Sprites.FindImage(fTrain[i]._type+'D');
                  fTrain[i]._dir:='D';
                end;
                if tx<ttx then begin
                  fTrain[i]._sprite.SetAnimation(Animations[fTrain[i]._type+'L']);
//                  fTrain[i]._sprite.Image:=Sprites.FindImage(fTrain[i]._type+'L');
                  fTrain[i]._dir:='L';
                end;
                if tx>ttx then begin
                  fTrain[i]._sprite.SetAnimation(Animations[fTrain[i]._type]);
//                  fTrain[i]._sprite.Image:=Sprites.FindImage(fTrain[i]._type);
                  fTrain[i]._dir:='R';
                end;
                tx:=ttx;
                ty:=tty;
              end;
            end;
            if fSpeed=32767 then begin
              fDirX:=0;fDirY:=0;
              keys[SDLK_Up]:=false;
              keys[SDLK_Left]:=false;
              keys[SDLK_Down]:=false;
              keys[SDLK_Right]:=false;
            end;
          end;
          fMoveDelay:=fSpeed;
        end;
      end;
    1:begin
        if fTrain[0]._sprite.Finished then begin
          fDead:=2;
          fTrain[0]._sprite.SetAnimation(Animations['c2'],true);
        end;
      end;
  end;
end;

function TPlayer.PickedUpCargo:boolean;
begin
  PickedUpCargo:=fPickedUp;
  fPickedUp:=false;
end;

{function TPlayer.ReachedExit:boolean;
begin
  ReachedExit:=fExit;
end;}

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
  SetLength(fTrain,length(fTrain)+1);
  case fTrain[length(fTrain)-2]._dir of
    'U':fTrain[length(fTrain)-1]._sprite:=TAnimatedSprite.Create(fTrain[length(fTrain)-2]._sprite.X,fTrain[length(fTrain)-2]._sprite.Y+32,Animations[chr(iType)]);
    'D':fTrain[length(fTrain)-1]._sprite:=TAnimatedSprite.Create(fTrain[length(fTrain)-2]._sprite.X,fTrain[length(fTrain)-2]._sprite.Y-32,Animations[chr(iType)]);
    'L':fTrain[length(fTrain)-1]._sprite:=TAnimatedSprite.Create(fTrain[length(fTrain)-2]._sprite.X+32,fTrain[length(fTrain)-2]._sprite.Y,Animations[chr(iType)]);
    'R':fTrain[length(fTrain)-1]._sprite:=TAnimatedSprite.Create(fTrain[length(fTrain)-2]._sprite.X-32,fTrain[length(fTrain)-2]._sprite.Y,Animations[chr(iType)]);
  end;
  fTrain[length(fTrain)-1]._type:=chr(iType);
//  fTrain[length(fTrain)-1]._sprite.FrameDelay:=6;
end;

function TPlayer.IsDead:boolean;
begin
  IsDead:=(fDead=2) and (keys[SDLK_Space] or keys[SDLK_Up] or keys[SDLK_Down] or keys[SDLK_Left] or keys[SDLK_Right]);
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
  else begin
    Result._sprite:=nil;
    Result._dir:=#0;
    Result._type:=#0;
  end;
end;

end.
