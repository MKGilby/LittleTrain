{$mode delphi}
{$smartlink on}

unit LTRN_MapBaseUnit;

interface

uses AnimatedSprite2Unit, LTRN_PlayerUnit, LTRN_MapListUnit, mk_sdl2, ARGBImageUnit;

const
  MAPSTATE_COMPLETED=2;
  MAPSTATE_UNLOCKED=1;
  MAPSTATE_LOCKED=0;


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
    fImage:TStreamingTexture;
//    fTexture:TTexture;
    
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

uses MKToolBox, Logger, SDL2, SysUtils,
     LTRN_VMUUnit{, LTRN_MapImagesUnit}, LTRN_SharedUnit;

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

  fImage:=TStreamingTexture.Create(168,104);
  SDL_SetTextureBlendMode(fImage.Texture,SDL_BLENDMODE_BLEND);
  fBest:=VMU.GetMapState(fVMUSlot,iMapNo);
  fState:=0;
  fPlayer:=nil;
  fExit:=nil;
  UpdateImage;
end;

destructor TMapBase.Destroy;
begin
  if Assigned(fImage) then FreeAndNIL(fImage);
  inherited ;
end;

procedure TMapBase.Draw(iAlpha:integer);
begin
  if (fX>-168) and (fX<640) then begin
    SDL_SetTextureAlphaMod(fImage.Texture,(255-(abs(236-fX)*255 div 404))*iAlpha div 255);
    if fX<0 then PutTexturePart(0,fY,-fX,0,167,103,fImage)
    else if fX>472 then PutTexturePart(fX,fY,0,0,640-fX,103,fImage)
    else PutTexture(fX,fY,fImage);
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
  if fState=MAPSTATE_LOCKED then
    MapImages[0].CopyTo(0,0,MapImages[0].Width,MapImages[0].Height,4,4,fImage.ARGBImage)
  else
    MapImages[fMapNo+1].CopyTo(0,0,MapImages[fMapNo+1].Width,MapImages[fMapNo+1].Height,4,4,fImage.ARGBImage);
//  if fState=0 then fImage.PutImage(4,4,MapImages[0])
//              else fImage.PutImage(4,4,MapImages[fMapNo+1]);
  for i:=0 to 3 do
    fImage.ARGBImage.Rectangle(i,i,168-i*2,104-i*2,
              Colors[fState,0]*(i+1) div 4,Colors[fState,1]*(i+1) div 4,Colors[fState,2]*(i+1) div 4);
  fImage.Update;
end;

procedure TMapBase.DrawMap;
var i,j:integer;
begin
  fPlayer:=nil;
  fGoodies:=0;
  for j:=0 to 11 do
    for i:=0 to 19 do begin
      Bar(i<<5,j<<5+48,32,32,0,0,0);
      case fMap.Tiles[i,j] of
        TILE_EMPTY:;
        TILE_CLOSEDEXIT:fExit:=TAnimatedSprite.Create(i<<5,j<<5+48,MM.Animations.ItemByName[chr(33)].SpawnAnimation);
        TILE_WALL:fSprites[i,j]:=TAnimatedSprite.Create(i<<5,j<<5+48,MM.Animations.ItemByName[chr(35)].SpawnAnimation);
        TILE_PLAYER:fPlayer:=TPlayer.Create(i,j,3,fMap);
        TILE_SIGNAL:fExit:=TAnimatedSprite.Create(i<<5,j<<5+48,MM.Animations.ItemByName[chr(61)].SpawnAnimation);
        else begin
          fSprites[i,j]:=TAnimatedSprite.Create(i<<5,j<<5+48,MM.Animations.ItemByName[chr(fMap.Tiles[i,j])].SpawnAnimation);
          if fMap.Tiles[i,j] in [97..114] then inc(fGoodies);
          if fMap.Tiles[i,j] in [99,101,111] then fSprites[i,j].Animation.LoopDelay:=20+random(60);
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
  if (iNewState in [MAPSTATE_COMPLETED,MAPSTATE_LOCKED,MAPSTATE_UNLOCKED]) and
     (fState<>iNewState) then begin
    fState:=iNewState;
    UpdateImage;
  end;
end;



end.
