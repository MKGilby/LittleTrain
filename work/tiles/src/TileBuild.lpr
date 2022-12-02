program TileBuild;

{$mode delphi}

uses
  ARGBImageUnit,
  ARGBImageBMPReaderUnit,
  ARGBImageCELReaderUnit,
  ARGBImagePNGReaderUnit,
  ARGBImageTGAReaderUnit,
  ARGBImagePNGWriterUnit,
  Logger,
  SysUtils,
  TextureAtlasGeneratorUnit,
  MKToolbox,
  AnimationDataUnit,
  MKStream;

type

  { TMain }

  TMain=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fInFilename:string;
    fAtlas:TTextureAtlasGenerator;
    fWagonBTM,fSurprise,fCongaFnt:TARGBImage;
    procedure ProcessTextures;
    procedure ProcessFile(pLine:string);
    procedure CreatePresents;
    procedure CreateTextWagons;
  end;


  { TMain }

constructor TMain.Create;
begin
  if paramcount=1 then fInFilename:=paramstr(1) else fInFilename:='sprites.txt';
  MKStreamOpener.AddDirectory('.',0);
//  fAtlas:=TTextureAtlasGenerator.Create(529,529,1);
//  fAtlas:=TTextureAtlasGenerator.Create(529,694,1);
//  fAtlas.TextureAtlas.Bar(0,0,fAtlas.TextureAtlas.Width,fAtlas.TextureAtlas.Height,0,0,0,255);
  fWagonBTM:=TARGBImage.Create('new\wagonbtm.cel');
  fSurprise:=TARGBImage.Create('new\surprise.cel');
  fCongaFnt:=TARGBImage.Create('new\congafnt.cel');
end;

destructor TMain.Destroy;
begin
  if Assigned(fCongaFnt) then FreeAndNil(fCongaFnt);
  if Assigned(fSurprise) then FreeAndNil(fSurprise);
  if Assigned(fWagonBTM) then FreeAndNil(fWagonBTM);
  if Assigned(fAtlas) then begin
//    fAtlas.TextureAtlas.WriteFile('sprites01.png','PNG');
    FreeAndNil(fAtlas);
  end;
  inherited Destroy;
end;

procedure TMain.Run;
begin
//  CreateTextWagons;
  ProcessTextures;
//  CreatePresents;
end;

procedure TMain.ProcessTextures;
var
  s,command,param,sheetfilename:string;
  t:textfile;
  mode:(mScanning,mSpriteSheet,mFiles);
  wi,he:integer;
begin
  if not FileExists(fInFilename) then raise Exception.Create('File not found: '+fInFilename);
  mode:=mScanning;
  wi:=-1;
  he:=-1;
  sheetfilename:='';

  assignfile(t,fInFilename);
  reset(t);
  while not eof(t) do begin
    readln(t,s);
    Log.Trace(s);
    if (length(s)>0) and (s[1]=';') then continue;
    s:=alltrim(s);

    case mode of
      mScanning:begin
        if uppercase(s)='[SPRITESHEET]' then mode:=mSpriteSheet
        else if uppercase(s)='[FILES]' then mode:=mFiles;
      end;
      mSpriteSheet:begin
        command:=GetNthSegment(s,' ',1);
        param:=GetNthSegment(s,' ',2);
        if uppercase(command)='WIDTH' then wi:=strtoint(param)
        else if uppercase(command)='HEIGHT' then he:=strtoint(param)
        else if uppercase(command)='NAME' then sheetfilename:=param
        else if command='' then begin
          fAtlas:=TTextureAtlasGenerator.Create(wi,he,1);
          fAtlas.TextureAtlas.Bar(0,0,fAtlas.TextureAtlas.Width,fAtlas.TextureAtlas.Height,0,0,0,255);
          mode:=mScanning;
        end;
      end;
      mFiles:begin
        if s<>'' then begin
          if s='*presents*' then CreatePresents
          else if s='*congrats*' then CreateTextWagons
          else ProcessFile(s);
        end else begin
          fAtlas.TextureAtlas.WriteFile(sheetfilename,'PNG');
          FreeAndNil(fAtlas);
          mode:=mScanning;
        end;
      end;
    end;

  end;
  closefile(t);
  if assigned(fAtlas) then begin
    fAtlas.TextureAtlas.WriteFile(sheetfilename,'PNG');
    FreeAndNil(fAtlas);
  end;
end;

procedure TMain.ProcessFile(pLine: string);
var org:TARGBImage;filename,name:string;i,FrameDelay:integer;atma:TAnimationData;
begin
  filename:='new\'+GetNthSegment(pLine,' ',1);
  org:=TARGBImage.Create;
  try
    org.ReadFile(filename);
    if pos('COLORKEY',uppercase(pLine))>0 then org.SetColorkey(0,0,0);
    name:=GetNthSegment(pLine,' ',2);
    if (length(name)>2) and (name[1]='"') and (name[length(name)]='"') then name:=copy(name,2,length(name)-2);
    FrameDelay:=strtoint(GetNthSegment(pLine,' ',3));
    delete(pLine,1,pos(' ',pLine));
    delete(pLine,1,pos(' ',pLine));
    delete(pLine,1,pos(' ',pLine));

    atma:=TAnimationData.Create(32,32);
    atma.Name:=name;
    atma.FrameDelay:=FrameDelay;
    atma.Looped:=pos('LOOPED',uppercase(pLine))>0;
    atma.Paused:=pos('PAUSED',uppercase(pLine))>0;
    atma.RandomStart:=pos('RANDOMSTART',uppercase(pLine))>0;
    atma.PingPong:=pos('PINGPONG',uppercase(pLine))>0;
    for i:=0 to org.Width div 32-1 do atma.AddFrame(i*32,0);
    org.Animations.AddObject(name,atma);
    fAtlas.AddImage(org);
  finally
    FreeAndNil(org);
  end;

  if pos('ROTATED',uppercase(pLine))>0 then begin
    org:=TARGBImage.Create(filename);
    try
      org.FlipH;
      if pos('COLORKEY',uppercase(pLine))>0 then org.SetColorkey(0,0,0);
      atma:=TAnimationData.Create(32,32);
      atma.Name:=name+'L';
      atma.FrameDelay:=FrameDelay;
      atma.Looped:=pos('LOOPED',uppercase(pLine))>0;
      atma.Paused:=pos('PAUSED',uppercase(pLine))>0;
      atma.RandomStart:=pos('RANDOMSTART',uppercase(pLine))>0;
      atma.PingPong:=pos('PINGPONG',uppercase(pLine))>0;
      for i:=org.Width div 32-1 downto 0 do atma.AddFrame(i*32,0);
      org.Animations.AddObject(name+'L',atma);
      fAtlas.AddImage(org);
    finally
      FreeAndNil(org);
    end;

    org:=TARGBImage.Create(filename);
    try
      org.FlipH;
      org.Rotate(1);
      if pos('COLORKEY',uppercase(pLine))>0 then org.SetColorkey(0,0,0);
      atma:=TAnimationData.Create(32,32);
      atma.Name:=name+'U';
      atma.FrameDelay:=FrameDelay;
      atma.Looped:=pos('LOOPED',uppercase(pLine))>0;
      atma.Paused:=pos('PAUSED',uppercase(pLine))>0;
      atma.RandomStart:=pos('RANDOMSTART',uppercase(pLine))>0;
      atma.PingPong:=pos('PINGPONG',uppercase(pLine))>0;
      for i:=org.Height div 32-1 downto 0 do atma.AddFrame(0,i*32);
      org.Animations.AddObject(name+'U',atma);
      fAtlas.AddImage(org);
    finally
      FreeAndNil(org);
    end;

    org:=TARGBImage.Create(filename);
    try
      org.FlipH;
      org.Rotate(3);
      if pos('COLORKEY',uppercase(pLine))>0 then org.SetColorkey(0,0,0);
      atma:=TAnimationData.Create(32,32);
      atma.Name:=name+'D';
      atma.FrameDelay:=FrameDelay;
      atma.Looped:=pos('LOOPED',uppercase(pLine))>0;
      atma.Paused:=pos('PAUSED',uppercase(pLine))>0;
      atma.RandomStart:=pos('RANDOMSTART',uppercase(pLine))>0;
      atma.PingPong:=pos('PINGPONG',uppercase(pLine))>0;
      for i:=0 to org.Height div 32-1 do atma.AddFrame(0,i*32);
      org.Animations.AddObject(name+'D',atma);
      fAtlas.AddImage(org);
    finally
      FreeAndNil(org);
    end;
  end;
end;

procedure TMain.CreatePresents;
const
  PresentColors:array[0..7,0..2] of integer=
    ((255,0,0),(255,0,0),(255,255,0),(255,255,0),(0,64,255),(0,255,0),(0,255,0),(255,0,255));
  WrapperColors:array[0..7,0..2] of integer=
    ((0,255,0),(0,64,255),(255,0,0),(0,64,255),(255,0,255),(255,0,0),(255,0,255),(255,255,0));
var
  present,wrapper:TARGBImage;
  atma:TAnimationData;
  i:integer;
begin
  wrapper:=TARGBImage.Create(32,32);
  try
    for i:=0 to 7 do begin
      present:=TARGBImage.Create(32,32);
      try
        fSurprise.Copy(32,0,32,32,present);
        fSurprise.Copy(0,0,32,32,wrapper);
        present.RecolorRGB(PresentColors[i,0],PresentColors[i,1],PresentColors[i,2]);
        wrapper.RecolorRGB(WrapperColors[i,0],WrapperColors[i,1],WrapperColors[i,2]);
        wrapper.SetColorkey(0,0,0);
        wrapper.Copy(0,0,32,32,present,true);
        atma:=TAnimationData.Create(32,32);
        atma.Name:=chr(ord('s')+i);
        atma.AddFrame(0,0);
        atma.Looped:=true;
        atma.LoopDelay:=20;
        present.Animations.AddObject(atma.name,atma);
        fAtlas.AddImage(present);
      finally
        FreeAndNil(present);
      end;
    end;
  finally
    FreeAndNil(wrapper);
  end;
end;

procedure TMain.CreateTextWagons;
var
  fWagons,fFontTMP,fTMP:TARGBImage;
  atmA:TAnimationData;
  i:integer;
begin
  fWagons:=TARGBImage.Create(512,128);
  try
    // Add wheels
    for i:=0 to 15 do begin
      fWagonBTM.CopyTo(0,0,32,32,i*32,0,fWagons,false);
    end;

    fWagons.FlipH;
    fFontTMP:=TARGBImage.Create(fCongaFnt.Width,fCongaFnt.Height);
    try
      fCongaFnt.Copy(0,0,fCongaFnt.Width,fCongaFnt.Height,fFontTMP);
      fFontTMP.RecolorRGB(255,0,0);  // Red
      fFontTMP.CopyTo(0,0,12,fFontTMP.Height,2,0,fWagons);  // C
      fFontTMP.CopyTo(12,0,12,fFontTMP.Height,50,0,fWagons);  // O
      fFontTMP.CopyTo(72,0,12,fFontTMP.Height,194,0,fWagons);  // T
      fFontTMP.CopyTo(84,0,12,fFontTMP.Height,242,0,fWagons);  // U
      fFontTMP.CopyTo(12,0,12,fFontTMP.Height,386,0,fWagons);  // O
      fFontTMP.CopyTo(24,0,12,fFontTMP.Height,434,0,fWagons);  // N
      fCongaFnt.Copy(0,0,fCongaFnt.Width,fCongaFnt.Height,fFontTMP);
      fFontTMP.RecolorRGB(0,255,0);  // Green
      fFontTMP.CopyTo(12,0,12,fFontTMP.Height,18,0,fWagons);  // O
      fFontTMP.CopyTo(0,0,12,fFontTMP.Height,34,0,fWagons);  // C
      fFontTMP.CopyTo(84,0,12,fFontTMP.Height,210,0,fWagons);  // U
      fFontTMP.CopyTo(72,0,12,fFontTMP.Height,226,0,fWagons);  // T
      fFontTMP.CopyTo(24,0,12,fFontTMP.Height,402,0,fWagons);  // N
      fFontTMP.CopyTo(12,0,12,fFontTMP.Height,418,0,fWagons);  // O

      fCongaFnt.Copy(0,0,fCongaFnt.Width,fCongaFnt.Height,fFontTMP);
      fFontTMP.RecolorRGB(0,64,255);  // Blue
      fFontTMP.CopyTo(24,0,12,fFontTMP.Height,66,0,fWagons);  // N
      fFontTMP.CopyTo(36,0,12,fFontTMP.Height,114,0,fWagons);  // G
      fFontTMP.CopyTo(96,0,12,fFontTMP.Height,258,0,fWagons);  // L
      fFontTMP.CopyTo(60,0,12,fFontTMP.Height,306,0,fWagons);  // A
      fFontTMP.CopyTo(120,0,12,fFontTMP.Height,450,0,fWagons);  // S
      fFontTMP.CopyTo(132,0,12,fFontTMP.Height,498,0,fWagons);  // !
      fCongaFnt.Copy(0,0,fCongaFnt.Width,fCongaFnt.Height,fFontTMP);
      fFontTMP.RecolorRGB(255,255,0);  // Yellow
      fFontTMP.CopyTo(36,0,12,fFontTMP.Height,82,0,fWagons);  // G
      fFontTMP.CopyTo(24,0,12,fFontTMP.Height,98,0,fWagons);  // N
      fFontTMP.CopyTo(60,0,12,fFontTMP.Height,274,0,fWagons);  // A
      fFontTMP.CopyTo(96,0,12,fFontTMP.Height,290,0,fWagons);  // L
      fFontTMP.CopyTo(132,0,12,fFontTMP.Height,466,0,fWagons);  // !
      fFontTMP.CopyTo(120,0,12,fFontTMP.Height,482,0,fWagons);  // S

      fCongaFnt.Copy(0,0,fCongaFnt.Width,fCongaFnt.Height,fFontTMP);
      fFontTMP.RecolorRGB(255,0,255);  // Magenta
      fFontTMP.CopyTo(48,0,12,fFontTMP.Height,130,0,fWagons);  // R
      fFontTMP.CopyTo(60,0,12,fFontTMP.Height,178,0,fWagons);  // A
      fFontTMP.CopyTo(72,0,12,fFontTMP.Height,322,0,fWagons);  // T
      fFontTMP.CopyTo(108,0,12,fFontTMP.Height,370,0,fWagons);  // I
      fCongaFnt.Copy(0,0,fCongaFnt.Width,fCongaFnt.Height,fFontTMP);
      fFontTMP.RecolorRGB(0,255,255);  // Cyan
      fFontTMP.CopyTo(60,0,12,fFontTMP.Height,146,0,fWagons);  // A
      fFontTMP.CopyTo(48,0,12,fFontTMP.Height,162,0,fWagons);  // R
      fFontTMP.CopyTo(108,0,12,fFontTMP.Height,338,0,fWagons);  // I
      fFontTMP.CopyTo(72,0,12,fFontTMP.Height,354,0,fWagons);  // T

      fTMP:=TARGBImage.Create(32,32);
      try
        for i:=0 to 15 do begin
          fWagons.Copy(i*32,0,32,32,fTMP);
          fTMP.FlipH;
          fTMP.CopyTo(0,0,32,32,i*32,32,fWagons);
          fTMP.Rotate(1);
          fTMP.CopyTo(0,0,32,32,i*32,64,fWagons);
          fTMP.Rotate(2);
          fTMP.CopyTo(0,0,32,32,i*32,96,fWagons);
        end;
      finally
        FreeAndNil(fTMP);
      end;


      for i:=0 to 7 do begin
        atma:=TAnimationData.Create(32,32);
        atma.Name:=chr(ord('S')+i)+'L';
        atma.FrameDelay:=32;
        atma.Looped:=true;
        atma.AddFrame(i*64,0);
        atma.AddFrame(i*64+32,0);
        fWagons.Animations.AddObject(atma.Name,atma);

        atma:=TAnimationData.Create(32,32);
        atma.Name:=chr(ord('S')+i);
        atma.FrameDelay:=32;
        atma.Looped:=true;
        atma.AddFrame(i*64,32);
        atma.AddFrame(i*64+32,32);
        fWagons.Animations.AddObject(atma.Name,atma);

        atma:=TAnimationData.Create(32,32);
        atma.Name:=chr(ord('S')+i)+'U';
        atma.FrameDelay:=32;
        atma.Looped:=true;
        atma.AddFrame(i*64,96);
        atma.AddFrame(i*64+32,96);
        fWagons.Animations.AddObject(atma.Name,atma);

        atma:=TAnimationData.Create(32,32);
        atma.Name:=chr(ord('S')+i)+'D';
        atma.FrameDelay:=32;
        atma.Looped:=true;
        atma.AddFrame(i*64,64);
        atma.AddFrame(i*64+32,64);
        fWagons.Animations.AddObject(atma.Name,atma);
      end;

      fAtlas.AddImage(fWagons);
    finally
      fFontTMP.Free;
    end;
  finally
    FreeAndNil(fWagons);
  end;
end;

var
  Main:TMain;

begin
  Main:=TMain.Create;
  try
    try
      Main.Run;
    except
      on e:exception do begin
        writeln('* Exception: '+e.Message);
        Log.Trace('* Exception: '+e.Message);
      end;
    end;
  finally
    Main.Free;
  end;
end.


