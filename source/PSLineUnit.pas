unit PSLineUnit;

{$mode delphi}

interface

uses mk_sdl2, ARGBImageUnit;

type TPSLine=class
       constructor Create(iText:String;iPosition,iDelay:integer);
       destructor Destroy; override;
       procedure StartOut;
       procedure Draw;
       procedure ChangeText(iNewText:string);
     private
       fText:String;
       fWImage,fTImage:TTexture;
       procedure CreateImages;
     protected
       fFase:word;
       fPosition:integer;
       fDelay:integer;
       fDelaySave:integer;
       procedure CreateImages2(atmT:TARGBImage);
     end;

const LINEONESTEPTIME=16;

implementation

uses SDL2, LTRN_SharedUnit, Font2Unit, SysUtils;

const LINEMUL=16; // 256 div LINEONESTEPTIME;

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
  if Assigned(fTImage) then FreeAndNIL(fTImage);
  if Assigned(fWImage) then FreeAndNIL(fWImage);
  inherited ;
end;

procedure TPSLine.CreateImages2(atmT:TARGBImage);
var i,j:integer;atmW:TARGBImage;
begin
  if Assigned(fTImage) then FreeAndNIL(fTImage);
  if Assigned(fWImage) then FreeAndNIL(fWImage);

  atmW:=TARGBImage.Create(640,22);
  atmW.bar(0,0,atmT.Width,atmt.Height,0,0,0,0);
  MM.Fonts.OutText(atmT,fText,320,0,mjCenter);

  for j:=0 to 21 do
    for i:=0 to 639 do
      if atmT.GetPixel(i,j)<>$00000000 then atmW.PutPixel(i,j,255,255,255,255);

  atmT.SetColorkey(0,0,0);
  fTImage:=TStaticTexture.Create(atmT);
  SDL_SetTextureBlendMode(fTImage.Texture,SDL_BLENDMODE_BLEND);
  fWImage:=TStaticTexture.Create(atmW);
  SDL_SetTextureBlendMode(fWImage.Texture,SDL_BLENDMODE_BLEND);
  FreeAndNil(atmW);
end;

procedure TPSLine.CreateImages;
var atmT:TARGBImage;
begin
  atmT:=TARGBImage.Create(640,22);
  atmT.bar(0,0,atmT.Width,atmt.Height,0,0,0,0);
  MM.Fonts.OutText(atmT,fText,320,0,mjCenter);
  CreateImages2(atmT);
  FreeAndNil(atmT);
end;

procedure TPSLine.StartOut;
begin
  fDelay:=fDelaySave;
  fFase:=LINEONESTEPTIME<<1+1;
end;

procedure TPSLine.Draw;
begin
  if (fDelay>0) then begin
    dec(fDelay);
    if fDelay=0 then inc(fFase);
  end;
  if (fFase>0) and (fFase<=LINEONESTEPTIME) then begin
    SDL_SetTextureAlphaMod(fWImage.Texture,fFase*LINEMUL-1);
    PutTexture(0,fPosition,fWImage);
    inc(fFase);
  end;
  if (fFase>LINEONESTEPTIME) and (fFase<=LINEONESTEPTIME<<1) then begin
    PutTexture(0,fPosition,fTImage);
    SDL_SetTextureAlphaMod(fWImage.Texture,(LINEONESTEPTIME<<1-fFase)*LINEMUL);
    PutTexture(0,fPosition,fWImage);
    inc(fFase);
  end;
  if fFase=LINEONESTEPTIME<<1+1 then begin
    PutTexture(0,fPosition,fTImage);
  end;
  if (fFase>LINEONESTEPTIME<<1+1) and (fFase<=LINEONESTEPTIME*3+1) then begin
    SDL_SetTextureAlphaMod(fTImage.Texture,(LINEONESTEPTIME*3+1-fFase)*LINEMUL);
    PutTexture(0,fPosition,fTImage);
    inc(fFase);
  end;
end;

procedure TPSLine.ChangeText(iNewText:string);
begin
  fText:=iNewText;
  CreateImages;
end;

end.
