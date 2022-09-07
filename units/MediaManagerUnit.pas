{ -[Name]-------------------------------------------

             MKSZTSZ MediaManager class

  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2020-

  -[Description]------------------------------------

    Loads media files and lets you use them:
      - Images     (from PNG, TGA, CEL, GSD, you must include ARGBImageXXXReaderUnit in program's uses)
      - Textures   (images prepared for use with SDL2)
      - Animations (from PNG)
      - Masks      (from PNG)
      - Fonts      (from TGA and PNG)
      - Musics     (from MP3 and multiple MOD formats)

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.06.25
//     * Initial creation
//  V1.01: Gilby - 2020.07.10
//     + AddImage added (similar to load but processes an already loaded ARGBImage)
//  V1.02: Gilby - 2021.01.06
//     * In Load and AddImage you can specify if you want to create Texture
//       from the ARGBImage even if it doesn't contain animations.
//       (Default=false)
//  V1.03: Gilby - 2021.04.16
//     * Incorporated masks into MediaManager.
//     + AddMask method added.
//     * Load and AddImage behaviour now can be controlled by flags.
//       See MM_ constants for more info.
//  V1.04: Gilby - 2021.10.10
//     * Incorporated fonts into MediaManager.
//     * Incorporated musics into MediaManager.
//  V1.05: Gilby - 2022.06.23
//     * Reworked for new Animation logic
//  V1.06: Gilby - 2022.08.10
//     * Animations now hold a reference to the original image when created by
//       Load or LoadImage
//  V1.07: Gilby - 2022.08.31
//     * Incorporated waves into MediaManager.
//     + Added TreatMP3AsMusic:boolean property;
//         if true, load will add MP3-s to Musics,
//         if false, load will add MP3-s to Waves.
//       Default value is true.
//     + Added Assigned checks in destructor.

unit MediaManagerUnit;

{$mode delphi}{$H+}

interface

uses Lists, ARGBImageUnit, mk_sdl2, AnimationDataUnit, Mask2Unit, FontList2Unit,
  MusicListUnit, WaveListUnit, Animation2Unit, Bass;

const
  // Create texture from image even when there's no animation data
  MM_CREATETEXTUREWHENNOANIMATIONDATA=1;
  // Create masks for animation frames (if there are any)
  MM_CREATEMASKFORANIMATIONFRAMES=2;
  // Don't create font when font data included in image
  MM_DONTCREATEFONTFROMIMAGE=4;


type
  { TAnimationDataWithTexture }

  TAnimationDataWithTexture=class
    constructor Create(iAnimationData:TAnimationData;iTexture:TTexture;iSourceImage:TARGBImage=nil);
    function SpawnAnimation:TAnimation;
  private
    fAnimationData:TAnimationData;
    fTexture:TTexture;
    fARGBImage:TARGBImage;
  public
    property Animation:TAnimationData read fAnimationData;
    property Image:TARGBImage read fARGBImage;
  end;

  TImages=TNamedList<TARGBImage>;
  TTextures=TNamedList<TTexture>;
  TMasks=TNamedList<TMask>;
  TAnimationDWTs=TNamedList<TAnimationDataWithTexture>;

  { TMediaManager }

  TMediaManager=class
    constructor Create;
    destructor Destroy; override;
    procedure Load(pFilename:string;pName:string='';pFlags:integer=0);
    procedure LoadImage(pFilename:string;pName:string='';pFlags:integer=0);
    procedure LoadMusic(pFilename:string;pName:string='');
    procedure LoadWave(pFilename:string;pName:string='';pVolume:Float=1);
    procedure AddImage(pImage:TARGBImage;pImageName:string;pFlags:integer=0);
    procedure AddMask(pMask:TMask;pMaskName:string);
    // If you get animation with this, the TAnimation instance will be freed
    // at the end of the program.
    function SpawnAnimation(pAnimationName:string):TAnimation;
  private
    fImages:TImages;
    fTextures:TTextures;
    fSpawnedAnimations:TAnimations;
    fAnimationDWTs:TAnimationDWTs;
    fMasks:TMasks;
    fFonts:TFontList;
    fMusics:TMusicList;
    fWaves:TWaveList;
    fTreatMP3AsMusic:boolean;
  public
    property Images:TImages read fImages;
    property Textures:TTextures read fTextures;
    property Animations:TAnimationDWTs read fAnimationDWTs;
    property Masks:TMasks read fMasks;
    property Fonts:TFontList read fFonts;
    property Musics:TMusicList read fMusics;
    property Waves:TWaveList read fWaves;
    property TreatMP3AsMusic:boolean read fTreatMP3AsMusic write fTreatMP3AsMusic;
  end;

implementation

uses SysUtils, Logger, Font2Unit, MKToolbox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.07';

{ TAnimationDataWithTexture }

constructor TAnimationDataWithTexture.Create(iAnimationData: TAnimationData;
  iTexture: TTexture; iSourceImage: TARGBImage=nil);
begin
  fAnimationData:=iAnimationData;
  fTexture:=iTexture;
  fARGBImage:=iSourceImage;
end;

function TAnimationDataWithTexture.SpawnAnimation: TAnimation;
begin
  Result:=TAnimation.Create(fTexture,fAnimationData);
end;

constructor TMediaManager.Create;
begin
  fImages:=TImages.Create;
  fTextures:=TTextures.Create;
  fSpawnedAnimations:=TAnimations.Create;
  fAnimationDWTs:=TAnimationDWTs.Create;
  fMasks:=TMasks.Create;
  fFonts:=TFontList.Create;
  fMusics:=TMusicList.Create;
  fWaves:=TWaveList.Create;
  fTreatMP3AsMusic:=true;
end;

destructor TMediaManager.Destroy;
begin
  if Assigned(fWaves) then FreeAndNil(fWaves);
  if Assigned(fMusics) then FreeAndNil(fMusics);
  if Assigned(fFonts) then FreeAndNil(fFonts);
  if Assigned(fMasks) then FreeAndNil(fMasks);
  if Assigned(fAnimationDWTs) then FreeAndNil(fAnimationDWTs);
  if Assigned(fSpawnedAnimations) then FreeAndNil(fSpawnedAnimations);
  if Assigned(fTextures) then FreeAndNil(fTextures);
  if Assigned(fImages) then FreeAndNil(fImages);
  inherited ;
end;

procedure TMediaManager.Load(pFilename:string;pName:string='';pFlags:integer=0);
var ext:string;i:integer;
begin
  ext:=uppercase(ExtractFileExt(pFilename));
  if length(ext)>1 then delete(ext,1,1);
  if fTreatMP3AsMusic then i:=2 else i:=3;
  i:=strtoint(decode(ext,Format('PNG,1,TGA,1,BMP,1,CEL,1,GSD,1,MO3,2,MP3,%d,WAV,3,0',[i])));
  case i of
    1:LoadImage(pFilename,pName,pFlags);
    2:LoadMusic(pFilename,pName);
    3:LoadWave(pFilename,pName);
    else raise Exception.Create(Format('MediaManager: Unknown file extension! (%s)',[pFilename]));
  end;
end;

procedure TMediaManager.LoadImage(pFilename:string;pName:string;pFlags:integer=0);
var atmI:TARGBImage;
begin
  atmI:=TARGBImage.Create(pFilename);
  if pName='' then pName:=pFilename;
  AddImage(atmI,pName,pFlags);
end;

procedure TMediaManager.LoadMusic(pFilename:string;pName:string);
begin
  Musics.Add(pFilename,pName);
end;

procedure TMediaManager.LoadWave(pFilename: string; pName: string;
  pVolume: Float);
begin
  Waves.Add(pFilename,pName,pVolume);
end;

procedure TMediaManager.AddImage(pImage:TARGBImage;pImageName:string;pFlags:integer=0);
var
  atmT:TTexture;
  atmA:TAnimationDataWithTexture;
  i,j:integer;
begin
  fImages.AddObject(pImageName,pImage);
  if pImage.Animations.Count>0 then begin
    atmT:=TStaticTexture.Create(pImage);
    fTextures.AddObject(pImageName,atmT);
    for i:=0 to pImage.Animations.Count-1 do begin
      atmA:=TAnimationDataWithTexture.Create(pImage.Animations[i],atmT,pImage);
      fAnimationDWTs.AddObject(atmA.Animation.Name,atmA);
      if pFlags and MM_CREATEMASKFORANIMATIONFRAMES<>0 then
        for j:=0 to atmA.Animation.FrameCount-1 do begin
          fMasks.AddObject(
            Format('%s%d',[atmA.Animation.Name,j]),
            TMask.CreateFromImagePart(pImage,atmA.Animation.Frames[j].x,atmA.Animation.Frames[j].y,atmA.Animation.Frames[j].w,atmA.Animation.Frames[j].h)
          );
//          fMasks[fMasks.Count-1].DebugMask;
        end;
    end;
  end else
    if pFlags and MM_CREATETEXTUREWHENNOANIMATIONDATA<>0 then begin
      atmT:=TStaticTexture.Create(pImage);
      fTextures.AddObject(pImageName,atmT);
    end;
  if Assigned(pImage.FontData) and (pFlags and MM_DONTCREATEFONTFROMIMAGE=0) then begin
    fFonts.Add(TFont.Create(pImage),pImageName);
  end;
end;

procedure TMediaManager.AddMask(pMask:TMask;pMaskName:string);
begin
  fMasks.AddObject(pMaskName,pMask);
end;

function TMediaManager.SpawnAnimation(pAnimationName: string): TAnimation;
begin
  Result:=fAnimationDWTs.ItemByName[pAnimationName].SpawnAnimation;
  fSpawnedAnimations.AddObject(Result.Name,Result);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

