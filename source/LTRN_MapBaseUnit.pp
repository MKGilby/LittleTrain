{$mode delphi}
{$smartlink on}

unit LTRN_MapBaseUnit;

interface

uses AnimatedSpriteUnit, LTRN_PlayerUnit, ImageUnit, Lists, LTRN_MapListUnit;

type
  TMapBase=class
    constructor Create(iX,iY,iMapNo,iVMUSlot:integer);
    destructor Destroy; override;
    function Play:integer; virtual; abstract; // Result: 0-Completed, 1-Escaped, 2-Dead
    procedure Draw(iAlpha:integer=255);
    procedure MoveRelX(idX:integer);
    procedure SetState(iNewState:integer);
  protected
    fX,fY:integer;
    fMapNo:integer;
    fVMUSlot:integer;
    fState:integer;
    fBest:integer;
    fImage:TImage;
    
    fSprites:array[0..19,0..11] of TAnimatedSprite;
    fMap:TRawMap;
    fPlayer:TPlayer;
    fExit:TAnimatedSprite;
    fGoodies:integer;
    fScore:integer;
    fParScore:integer;
    fAutoPlay:boolean;
    fCongratulations:boolean;

//    function CreateMapThumb:PImage;
    procedure UpdateImage;
    procedure DrawMap;
    procedure ClearSprites;
  public
    property BestScore:integer read fBest;
    property ParScore:integer read fParScore;
    property State:integer read fState;
    property AutoPlay:boolean read fAutoPlay;
    property Congratulations:boolean read fCongratulations;
  end;

implementation

uses MKToolBox, Logger, SDL, mk_sdl,
     LTRN_VMUUnit, LTRN_MapImagesUnit, LTRN_SharedUnit;

constructor TMapBase.Create(iX,iY,iMapNo,iVMUSlot:integer);
//var i,j:integer;
begin
  fX:=iX;
  fY:=iY;
  fMapNo:=iMapNo;
  fVMUSlot:=iVMUSlot;

  fMap:=MapList.Items[iMapNo];
  fParScore:=fMap.ParScore;
  fAutoPlay:=fMap.AutoPlay;
  fCongratulations:=fMap.Congratulations;

  fImage:=TImage.Create(168,104);
  fBest:=VMU.GetMapState(iVMUSlot,iMapNo);
  fState:=0;
  fPlayer:=nil;
  fExit:=nil;
  UpdateImage;
end;

destructor TMapBase.Destroy;
begin
  FreeAndNIL(fImage);
//  FreeAndNIL(fThumb);
  inherited ;
end;

procedure TMapBase.Draw(iAlpha:integer);
begin
  if (fX>-168) and (fX<640) then begin
    SDL_SetAlpha(fImage.Surface,SDL_SRCALPHA or SDL_HWACCEL, (255-(abs(236-fX)*255 div 404))*iAlpha div 255);
    if fX<0 then PutImagePart(0,fY,-fX,0,167,103,fImage)
    else if fX>472 then PutImagePart(fX,fY,0,0,640-fX,103,fImage)
    else PutImage(fX,fY,fImage);
  end;
end;

procedure TMapBase.MoveRelX(idX:integer);
begin
  fX+=idX;
end;

procedure TMapBase.UpdateImage;
const Colors:array[0..2,0..2] of integer=((128,0,0),(192,192,64),(0,144,0));
var i:integer;
begin
  if fState=0 then fImage.PutImage(4,4,MapImages[0])
              else fImage.PutImage(4,4,MapImages[fMapNo+1]);
  for i:=0 to 3 do
    fImage.rectangle(i,i,168-i*2,104-i*2,
              Colors[fState,0]*(i+1) div 4,Colors[fState,1]*(i+1) div 4,Colors[fState,2]*(i+1) div 4);
end;

procedure TMapBase.DrawMap;
var i,j:integer;
begin
  fPlayer:=nil;
  fGoodies:=0;
  for j:=0 to 11 do
    for i:=0 to 19 do begin
      BarWH(i<<5,j<<5+48,32,32,0,0,0);
      case fMap.Tiles[i,j] of
        32:;
        33:fExit:=TAnimatedSprite.Create(i<<5,j<<5+48,Animations[chr(33)]);
        35:fSprites[i,j]:=TAnimatedSprite.Create(i<<5,j<<5+48,Animations[chr(35)]);
        37:fPlayer:=TPlayer.Create(i,j,3,fMap);
        61:fExit:=TAnimatedSprite.Create(i<<5,j<<5+48,Animations[chr(61)]);
        else begin
          fSprites[i,j]:=TAnimatedSprite.Create(i<<5,j<<5+48,Animations[chr(fMap.Tiles[i,j])]);
          if fMap.Tiles[i,j] in [97..114] then inc(fGoodies);
          if fMap.Tiles[i,j] in [99,101,111] then fSprites[i,j].LoopDelay:=20+random(60);
        end;
      end;
    end;
end;

procedure TMapBase.ClearSprites;
var i,j:integer;
begin
  for j:=0 to 11 do
    for i:=0 to 19 do
      if fSprites[i,j]<>nil then FreeAndNil(fSprites[i,j]);
  if fPlayer<>nil then FreeAndNil(fPlayer);
  if fExit<>nil then FreeAndNil(fExit);
end;

procedure TMapBase.SetState(iNewState:integer);
begin
  if iNewState in [0..2] then begin
    fState:=iNewState;
    UpdateImage;
  end;
end;



end.
