{ -[Name]-------------------------------------------

     MKSZTSZ Animated Sprite class for SDL2
                  using TAnimation for animations
           
  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2020-2022

  -[Description]------------------------------------

    [to be written]

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.02.10
//     - Initial creation from AnimatedSpriteUnit.
//  V1.01: Gilby - 2020.06.25
//     * Following changes in AnimationDataUnit.
//  V1.02: Gilby - 2021.03.03
//     * SetAnimation is now sets width and height according to new animation.
//     + Using HotPoint info in Draw. Example:
//       X,Y=32,32  HotPointX,HotPointY=3,5  Sprite will be drawn at 29,27
//  V1.03: Gilby - 2022.05.17
//     * Setting framedelay and loopdelay did not update current framedelaycount
//       and loopdelaycount
//  V1.04: Gilby - 2022.06.23
//     * Animation logic moved to TAnimation. (It means to create two animated
//       sprites with not the same animation phase you have to create two
//       animations.)
//       Animation only advances when you call TAnimation.Animate! (not automatically)
//  V1.05: Gilby - 2022.11.04
//     + Added possibility of self-animated sprites.
//       It will call Animation.Animate and Free the animation on Destroy.
//       Self-animated is the default behaviour of TAnimatedSprite since
//       it is closer to the original AnimatedSprite logic.
//       If SetAnimation used on a self-animated sprite, it Frees the
//       current animation and the new one will be Freed on Destroy.

{$ifdef fpc}
  {$smartlink on}
  {$mode delphi}
{$endif}

unit AnimatedSprite2Unit;

interface

uses LogicalSpriteUnit, Animation2Unit;

type

  { TAnimatedSprite }

  TAnimatedSprite=class(TLogicalSprite)
    constructor Create(iX,iY:integer;iAnimation:TAnimation;iSelfAnimated:boolean=true); overload;
    destructor Destroy; override;
    procedure SetAnimation(pAnimation:TAnimation;pResetFrameIndex:boolean=false);
    procedure Draw; override;
    procedure LogSpriteData;
  protected
    fAnimation:TAnimation;
    fSelfAnimated:boolean;
  public
    property Animation:TAnimation read fAnimation;
    property SelfAnimated:boolean read fSelfAnimated;
  end;

//var
//  DefaultSpriteTargetImage:TImage;

implementation

uses Logger, SysUtils, MK_SDL2;

const
  Fstr={$I %FILE%}+', ';
  Version='1.05';

constructor TAnimatedSprite.Create(iX,iY:integer; iAnimation:TAnimation;
  iSelfAnimated:boolean);
begin
  inherited Create(iX,iY,iAnimation.Width,iAnimation.Height);
  fAnimation:=iAnimation;
  fName:=iAnimation.Name;
  fSelfAnimated:=iSelfAnimated;
end;

destructor TAnimatedSprite.Destroy;
begin
  if fSelfAnimated and Assigned(fAnimation) then FreeAndNil(fAnimation);
  inherited Destroy;
end;

procedure TAnimatedSprite.SetAnimation(pAnimation:TAnimation;pResetFrameIndex:boolean=false);
begin
  if Assigned(pAnimation) then begin
    if fSelfAnimated and Assigned(fAnimation) then FreeAndNil(fAnimation);
    fAnimation:=pAnimation;
    fWidth:=fAnimation.Width;
    fHeight:=fAnimation.Height;
    if pResetFrameIndex then fAnimation.ResetFrameIndex;
  end
{$ifdef debug}
    else raise Exception.Create('NIL animation passed!');
{$endif}
end;

procedure TAnimatedSprite.Draw;
begin
  if fDead or not fVisible then exit;
  if fSelfAnimated then fAnimation.Animate;
  with fAnimation.Frames[fAnimation.CurrentFrameIndex] do
    PutTexturePart(fX-fAnimation.HotPointX,fY-fAnimation.HotPointY,x,y,w,h,fAnimation.Texture);
end;

procedure TAnimatedSprite.LogSpriteData;
var s:String;
begin
  Log.LogDebug('------ AnimatedSprite data start ------');
  Log.LogDebug('Name: '+fName);
  Log.LogDebug('Position: '+inttostr(fX)+', '+inttostr(fY));
  Log.LogDebug('Z-Order: '+inttostr(fZOrder));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  s:='[';
  if fVisible then s+='X' else s+=' ';
  s+='] Visible   [';
  if fDead then s+='X' else s+=' ';
  s+='] Dead';
  Log.LogDebug(s);
  fAnimation.LogData;
  Log.LogDebug('------- Sprite data end -------');

end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
//  DefaultSpriteTargetImage:=nil;

end.
