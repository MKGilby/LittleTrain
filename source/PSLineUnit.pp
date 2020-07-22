{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit PSLineUnit;

interface

uses MK_SDL, ImageUnit;

type TPSLine=class
       constructor Create(iText:String;iPosition,iDelay:integer);
       destructor Destroy; override;
       procedure StartOut;
       procedure Draw;
       procedure ChangeText(iNewText:string);
     private
       fDelay:integer;
       fDelaySave:integer;
       fText:String;
       fWImage,fTImage:TImage;
       fFase:word;
       fPosition:integer;
       procedure CreateImages;
     end;

const LineOneStepTime=16;

implementation

uses SDL, LTRN_SharedUnit, FontUnit;

const LineMul=16; // 256 div LineOneStepTime;

constructor TPSLine.Create(iText:String;iPosition,iDelay:integer);
begin
  fText:=iText;
  fPosition:=iPosition;
  fDelay:=iDelay+1;
  fDelaySave:=iDelay+1;
  fFase:=0;
  fTImage:=nil;
  fWImage:=nil;
  CreateImages;
end;

destructor TPSLine.Destroy;
begin
  if fTImage<>nil then FreeAndNIL(fTImage);
  if fWImage<>nil then FreeAndNIL(fWImage);
  inherited ;
end;

procedure TPSLine.CreateImages;
var i,j:integer;
begin
  if fTImage<>nil then FreeAndNIL(fTImage);
  if fWImage<>nil then FreeAndNIL(fWImage);
  fTIMage:=TImage.Create(640,22);
  fWIMage:=TImage.Create(640,22);
  Fonts.OutText(fTImage,fText,320,0,mjCenter);
  SDL_LockSurface(fTImage.Surface);
  SDL_LockSurface(fWImage.Surface);

  for j:=0 to 21 do
    for i:=0 to 639 do
      if fTImage.GetPixel(i,j)<>0 then fWImage.PutPixel(i,j,255,255,255);

  SDL_UnLockSurface(fTImage.Surface);
  SDL_UnLockSurface(fWImage.Surface);
  SDL_SetColorKey(fTImage.Surface,SDL_SRCCOLORKEY or SDL_HWACCEL or SDL_RLEACCEL,0);
  SDL_SetColorKey(fWImage.Surface,SDL_SRCCOLORKEY or SDL_HWACCEL or SDL_RLEACCEL,0);
end;

procedure TPSLine.StartOut;
begin
  fDelay:=fDelaySave;
  fFase:=LineOneStepTime<<1+1;
end;

procedure TPSLine.Draw;
begin
  if (fDelay>0) then begin
    dec(fDelay);
    if fDelay=0 then inc(fFase);
  end;
  if (fFase>0) and (fFase<=LineOneStepTime) then begin
    SDL_SetAlpha(fWImage.Surface,SDL_SRCALPHA or SDL_RLEACCEL,fFase*LineMul-1);
    PutImage(0,fPosition,fWImage);
    inc(fFase);
  end;
  if (fFase>LineOneStepTime) and (fFase<=LineOneStepTime<<1) then begin
    PutImage(0,fPosition,fTImage);
    SDL_SetAlpha(fWImage.Surface,SDL_SRCALPHA or SDL_RLEACCEL,(LineOneStepTime<<1-fFase)*LineMul);
    PutImage(0,fPosition,fWImage);
    inc(fFase);
  end;
  if fFase=LineOneStepTime<<1+1 then begin
    PutImage(0,fPosition,fTImage);
  end;
  if (fFase>LineOneStepTime<<1+1) and (fFase<=LineOneStepTime*3+1) then begin
    SDL_SetAlpha(fTImage.Surface,SDL_SRCALPHA or SDL_RLEACCEL,(LineOneStepTime*3+1-fFase)*LineMul);
    PutImage(0,fPosition,fTImage);
    inc(fFase);
  end;
end;

procedure TPSLine.ChangeText(iNewText:string);
begin
  fText:=iNewText;
  CreateImages;
end;

end.
