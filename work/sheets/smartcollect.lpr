program smartcollect;

{$mode delphi}{$H+}

uses
  sysutils, Classes, RawPictureUnit, RawPictureTGAUnit, RawPictureCELUnit,
  FastPaletteUnit, Logger, Lists, MKToolbox, CSVFileUnit, LogTicksUnit, FramesUnit;

const
  BLOCKWIDTH=32;
  BLOCKHEIGHT=32;
//  SPRITESHEETMAXWIDTH=640;

type
  TImageData=class
    constructor Create(iImage:TRawPicture;iICName:string;iRotated:boolean);
    destructor Destroy; override;
  public
    _ICname:string;
    _framecount,_framedelay,_loopdelay:integer;
    _looped,_randomstart,_paused,_pingpong:boolean;
    _transform:string;
    _needrotation:boolean;
    _frames:TIntList;
  end;

  TImages=class(TNamedList<TImageData>);

var
  Images:TImages;
  Frames:TFrames;
  FrameOrder:TIntList;
//  SC,ANIM:TStringList;
  Pal:TFastPalette;
  Best,BestCount:integer;
//  rotatedwidth,notrotatedwidth:integer;

constructor TImageData.Create(iImage:TRawPicture;iICName:string;iRotated:boolean);
var i:integer;atm:TRawPicture;
begin
  _framecount:=iImage.Width div BLOCKWIDTH;
  _frames:=TIntList.Create;
  for i:=0 to _framecount-1 do begin
    atm:=TRawPicture.Create(BLOCKWIDTH,BLOCKHEIGHT);
    iImage.CopyTo(i*BLOCKWIDTH,0,BLOCKWIDTH,BLOCKHEIGHT,0,0,atm);
    _frames.Add(Frames.AddFrame(atm,iRotated));
  end;
  _ICName:=iICName;
  _needrotation:=iRotated;
end;

destructor TImageData.Destroy;
begin
  FreeAndNil(_frames);
  inherited ;
end;

function AddColors(Pal:TFastPalette;Source:TRawPicture):boolean;
var i,j:integer;p:pointer;
begin
  Result:=false;
  p:=Source.Rawdata;
  for i:=0 to Source.Width*Source.Height-1 do begin
    j:=Pal.AddColor(byte(p^),byte((p+1)^),byte((p+2)^));
    if j=-1 then exit;
    inc(p,3);
  end;
  Result:=true;
end;

function FindNext:integer;
var atmPal:TFastPalette;
    i,min:integer;w:boolean;
begin
  Result:=-1;
  min:=87423;
  for i:=0 to Frames.Count-1 do if not Frames[i]._used then begin
    atmPal:=Pal.Clone;
    LogTicks.AddTimeTo(1);
    w:=AddColors(atmPal,Frames[i]._image);
    LogTicks.AddTimeTo(2);
//    Log.Trace(Format('    %s=%d',[L[i],atmPal.Count-Pal.Count]));
    if w and (atmPal.Count-Pal.Count<min) then begin
      min:=atmPal.Count-Pal.Count;
      Result:=i;
    end;
    FreeAndNil(atmPal);
    LogTicks.AddTimeTo(3);
  end;
  if Result<>-1 then begin
    AddColors(Pal,Frames[Result]._image);
    LogTicks.AddTimeTo(4);
//    Log.Trace(Format('%d (%d)',[Result, Pal.Count]));
  end;
end;

procedure TryCollect(StartItem:integer);
var i,cnt,full,filecnt:integer;
begin
  writeln(Format('TryCollect(%d)',[StartItem]));
  FrameOrder.Clear;
  Frames.ResetUsage;
  Pal:=TFastPalette.Create;
  full:=0;
  filecnt:=0;
  cnt:=0;
//  Pal.AddColor(0,0,0);
  AddColors(Pal,Frames[StartItem]._image);
  FrameOrder.Add(StartItem);
  Frames[StartItem]._used:=true;
  Frames[StartItem]._sheetoffset:=cnt;
  Frames[StartItem]._fileindex:=filecnt;
  inc(cnt);
//  Log.Trace(Format('%d (%d)',[StartItem, Pal.Count]));
  LogTicks.AddTimeTo(15);
  while Frames.UnusedCount>0 do begin
    i:=FindNext;
    if i>-1 then begin
      FrameOrder.Add(i);
      Frames[i]._used:=true;
      Frames[i]._sheetoffset:=cnt;
      Frames[i]._fileindex:=filecnt;
      inc(cnt);
    end else begin
      cnt:=0;
      Full+=Pal.Count;
      FreeAndNil(Pal);
      Pal:=TFastPalette.Create;
//      Pal.AddColor(0,0,0);
      inc(filecnt);
    end;
    LogTicks.AddTimeTo(0);
  end;
  Full+=Pal.Count;
  FreeAndNil(Pal);
  Log.LogStatus(Format('%d, %d',[StartItem,Full]));
//  writeln(StartItem,', ',Full);
  if Full<BestCount then begin
    Best:=StartItem;
    BestCount:=Full;
  end;
end;

procedure LoadImages(pFilename:String);
var CSV:TCSVFile;rp:TRawPicture;i:integer;atm:TImageData;
begin
  CSV:=TCSVFile.Create(pFilename);
  Images:=TImages.Create;
  for i:=0 to CSV.RowCount-1 do begin
    rp:=TRawPicture.Create;
    rp.ReadFile(CSV.GetData('File',i));
    atm:=TImageData.Create(rp,CSV.GetData('ICName',i),CSV.GetData('NeedRotation',i)='X');
    FreeAndNil(rp);
    if atm._framecount>1 then begin
      atm._framedelay:=strtoint('0'+CSV.GetData('FrameDelay',i));
      atm._loopdelay:=strtoint('0'+CSV.GetData('LoopDelay',i));
      atm._looped:=CSV.GetData('Looped',i)='X';
      atm._randomstart:=CSV.GetData('Randomstart',i)='X';
      atm._pingpong:=CSV.GetData('PingPong',i)='X';
      atm._paused:=CSV.GetData('Paused',i)='X';
    end;
//    atm._needrotation:=CSV.GetData('NeedRotation',i)='X';
    Images.AddObject(CSV.GetData('ICName',i),atm);
  end;
  FreeAndNil(CSV);
end;

procedure OrganizeFrames;
var i:integer;
begin
  BestCount:=99999;
  Best:=-1;
  LogTicks.StartMeasuring;
//  for i:=0 to Frames.Count-1 do TryCollect(i);
//  Log.LogStatus('Best='+inttostr(Best));
//  TryCollect(Best);
  TryCollect(0);
//  for i:=0 to 9 do TryCollect(i);
  for i:=0 to FrameOrder.Count-1 do begin
    Log.LogStatus(Format('%d. %d (%d)',[i,FrameOrder[i],Frames[FrameOrder[i]]._fileindex]));
//    Frames[FrameOrder[i]]._image.WriteFile('tmp\'+st(i,4,'0')+'.tga','TGA');
  end;
  LogTicks.ListRegisters;
end;

procedure SaveSheet(no:integer);
var i,cnt:integer;rp:TRawPicture;
begin
  cnt:=0;
  for i:=0 to Frames.Count-1 do
    if Frames[i]._fileindex=no then inc(cnt);
  rp:=TRawPicture.Create(cnt*BLOCKWIDTH,BLOCKHEIGHT);
  cnt:=0;
  for i:=0 to FrameOrder.Count-1 do begin
    if Frames[FrameOrder[i]]._fileindex=no then begin
      Frames[FrameOrder[i]]._image.CopyTo(0,0,BLOCKWIDTH,BLOCKHEIGHT,cnt*BLOCKWIDTH,0,rp);
      inc(cnt);
    end;
  end;
  rp.WriteFile('spritesraw'+inttostr(no)+'.tga','TGA');
  FreeAndNil(rp);
end;

procedure SaveSheetV(no:integer);
var i,cnt:integer;rp:TRawPicture;
begin
  cnt:=0;
  for i:=0 to Frames.Count-1 do
    if Frames[i]._fileindex=no then inc(cnt);
  rp:=TRawPicture.Create(BLOCKWIDTH,cnt*BLOCKHEIGHT);
  cnt:=0;
  for i:=0 to FrameOrder.Count-1 do begin
    if Frames[FrameOrder[i]]._fileindex=no then begin
      Frames[FrameOrder[i]]._image.CopyTo(0,0,BLOCKWIDTH,BLOCKHEIGHT,0,cnt*BLOCKHEIGHT,rp);
      inc(cnt);
    end;
  end;
  rp.WriteFile('sprites'+inttostr(no)+'.tga','TGA');
  FreeAndNil(rp);
end;

procedure SaveSheets;
var i,Max:integer;
begin
  Max:=-1;
  for i:=0 to Frames.Count-1 do if Frames[i]._fileindex>Max then Max:=Frames[i]._fileindex;
  for i:=0 to Max do SaveSheetV(i);
end;

procedure CopyToSheets(SC:TStringList;pre,srcindex,infileindex,cnt:integer;var notrotatedwidth,rotatedwidth:integer);
begin
  if pre and 1>0 then begin // Notrotated
    SC.Add(Format('CopyPartialImage source "sprites%d.tga" leftb %d top 0 target "notrotated" leftb %d topb 0 widthb %d heightb 1',
                  [srcindex,infileindex,notrotatedwidth,cnt]));
    notrotatedwidth+=cnt;
  end;
  if pre and 2>0 then begin // Rotated
    SC.Add(Format('CopyPartialImage source "sprites%d.tga" leftb %d top 0 target "rotated" leftb %d topb 0 widthb %d heightb 1',
                  [srcindex,infileindex,rotatedwidth,cnt]));
    rotatedwidth+=cnt;
  end;
end;

procedure AddStoppedTrain(SC,ANIM:TStringList;var rotatedwidth:integer);
var ei,i:integer;s:String;
begin
  ei:=Images.IndexOf('%');
  if ei=-1 then raise Exception.Create('Engine image not found!');
  for i:=0 to Images[ei]._frames.Count-1 do begin
    SC.Add(Format('CopyPartialImage source "sprites%d.tga" leftb %d top 0target "rotated" leftb %d top 0 widthb 1 heightb 1',
      [Frames[Images[ei]._frames[i]]._fileindex,Frames[Images[ei]._frames[i]]._sheetoffset,rotatedwidth+i]));
  end;
//  SC.Add(Format('CopyPartialImage source "rotated" leftb %d top 0 target "rotated" leftb %d top 0 widthb 8 heightb 1',
//    [Frames[Images[ei]._frames[0]]._rotatedoffset,rotatedwidth]));
  SC.Add(Format('CopyPartialImage source "rotated" leftb %d top 16 target "rotated" leftb %d top 16 widthb 1 height 16',
    [rotatedwidth,rotatedwidth+1]));
  SC.Add(Format('CopyPartialImage source "rotated" leftb %d top 16 target "rotated" leftb %d top 16 widthb 2 height 16',
    [rotatedwidth,rotatedwidth+2]));
  SC.Add(Format('CopyPartialImage source "rotated" leftb %d top 16 target "rotated" leftb %d top 16 widthb 4 height 16',
    [rotatedwidth,rotatedwidth+4]));
  s:=Format('Animation Name "$" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay %d looped Framesb',
    [Images[ei]._FrameDelay]);
  for i:=0 to 7 do s+=Format(' %d %d',[rotatedwidth+i,0]);
  ANIM.Add(s);
  s:=Format('Animation Name "$L" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay %d looped Framesb',
    [Images[ei]._FrameDelay]);
  for i:=0 to 7 do s+=Format(' %d %d',[rotatedwidth+i,1]);
  ANIM.Add(s);
  s:=Format('Animation Name "$U" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay %d looped Framesb',
    [Images[ei]._FrameDelay]);
  for i:=0 to 7 do s+=Format(' %d %d',[rotatedwidth+i,2]);
  ANIM.Add(s);
  s:=Format('Animation Name "$D" image "rotated" widthb 1 heightb 1 FrameCount 8 FrameDelay %d looped Framesb',
    [Images[ei]._FrameDelay]);
  for i:=0 to 7 do s+=Format(' %d %d',[rotatedwidth+i,3]);
  ANIM.Add(s);
  inc(rotatedwidth,8);
end;

procedure AddCongaPresents(SC,ANIM:TStringList;var notrotatedwidth:integer);
const
  Colors:array[0..4,0..2] of integer=((255,0,0),(0,255,0),(0,64,255),(255,255,0),(255,0,255));
  ColPairs:array[0..7,0..1] of integer=((0,1),(0,2),(3,0),(3,2),(2,4),(1,0),(1,4),(4,3));
var i:integer;
begin
  SC.Add('NewImage name "present" width 32 height 32');
  SC.Add('NewImage name "wrapper" width 32 height 32');
  for i:=0 to 7 do begin
    SC.Add('CopyPartialImage source "conga.tga" left 0 top 0 target "present" left 0 top 0 widthb 1 heightb 1');
    SC.Add('CopyPartialImage source "conga.tga" leftb 1 top 0 target "wrapper" left 0 top 0 widthb 1 heightb 1');
    SC.Add(Format('RecolorRGB name "present" r %d g %d b %d',[Colors[ColPairs[i,0],0],Colors[ColPairs[i,0],1],Colors[ColPairs[i,0],2]]));
    SC.Add(Format('RecolorRGB name "wrapper" r %d g %d b %d',[Colors[ColPairs[i,1],0],Colors[ColPairs[i,1],1],Colors[ColPairs[i,1],2]]));
    SC.Add('CopyImage source "wrapper" target "present" colorkeyrgb 0 0 0');
    SC.Add(Format('CopyPartialImage source "present" left 0 top 0 target "notrotated" leftb %d top 0 widthb 1 heightb 1',[notrotatedwidth]));
    ANIM.Add(Format('Animation name "%s" Image "notrotated" widthb 1 heightb 1 framecount 1 framesb %d 0',[chr(ord('s')+i),notrotatedwidth]));
    inc(notrotatedwidth);
  end;
  SC.Add('FreeImage name "wrapper"');
  SC.Add('FreeImage name "present"');
end;

procedure AddCongaWagons(SC,ANIM:TStringList;var rotatedwidth:integer);
const
  CongaText='CONGRATULATIONS!';
  CongaFont='CONGRATULIS!';
  Colors:array[0..5,0..2] of integer=((255,0,0),(0,255,0),(0,64,255),(255,255,0),(255,0,255),(0,255,255));
var ci,i:integer;
begin
  SC.Add('LoadImage name "congafnt.tga" filename "congafnt.tga"');
  SC.Add('NewImage name "fonttmp" width 144 height 21');
  SC.Add('NewImage name "wagon" widthb 2 heightb 1');

  ci:=0;
  for i:=0 to 7 do begin
    SC.Add('CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" left 0 top 0 widthb 1 heightb 1 transform "H"');
    SC.Add('CopyPartialImage source "conga.tga" leftb 2 top 0 target "wagon" leftb 1 top 0 widthb 1 heightb 1 transform "H"');
    SC.Add('CopyImage source "congafnt.tga" target "fonttmp"');
    SC.Add(Format('RecolorRGB name "fonttmp" r %d g %d b %d',[Colors[ci,0],Colors[ci,1],Colors[ci,2]]));
    SC.Add(Format('CopyPartialImage source "fonttmp" left %d top 0 target "wagon" left 2 top 2 width 12 height 21',[(pos(CongaText[i*2+1],CongaFont)-1)*12]));
    SC.Add(Format('CopyPartialImage source "fonttmp" left %d top 0 target "wagon" left 50 top 2 width 12 height 21',[(pos(CongaText[i*2+2],CongaFont)-1)*12]));
    SC.Add('CopyImage source "congafnt.tga" target "fonttmp"');
    SC.Add(Format('RecolorRGB name "fonttmp" r %d g %d b %d',[Colors[ci+1,0],Colors[ci+1,1],Colors[ci+1,2]]));
    SC.Add(Format('CopyPartialImage source "fonttmp" left %d top 0 target "wagon" left 18 top 2 width 12 height 21',[(pos(CongaText[i*2+2],CongaFont)-1)*12]));
    SC.Add(Format('CopyPartialImage source "fonttmp" left %d top 0 target "wagon" left 34 top 2 width 12 height 21',[(pos(CongaText[i*2+1],CongaFont)-1)*12]));
    SC.Add(Format('CopyPartialImageRepeat count 2 deltaleftb 1 deltatop 0 source "wagon" left 0 top 0 target "rotated" leftb %d topb 0 widthb 1 heightb 1 transform "H"',[rotatedwidth]));
    ANIM.Add(Format('Animation name "%s" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb %d 0 %d 0',[chr(ord('S')+i),rotatedwidth,rotatedwidth+1]));
    ANIM.Add(Format('Animation name "%sL" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb %d 1 %d 1',[chr(ord('S')+i),rotatedwidth,rotatedwidth+1]));
    ANIM.Add(Format('Animation name "%sU" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb %d 2 %d 2',[chr(ord('S')+i),rotatedwidth,rotatedwidth+1]));
    ANIM.Add(Format('Animation name "%sD" Image "rotated" widthb 1 heightb 1 framecount 2 framedelay 32 looped framesb %d 3 %d 3',[chr(ord('S')+i),rotatedwidth,rotatedwidth+1]));
    inc(rotatedwidth,2);
    inc(ci,2);
    if ci=6 then ci:=0;
  end;
  SC.Add('FreeImage name "wagon"');
  SC.Add('FreeImage name "fonttmp"');
  SC.Add('FreeImage name "congafnt.tga"');
end;

procedure AddConga(SC,ANIM:TStringList;var rotatedwidth:integer;var notrotatedwidth:integer);
begin
  SC.Add('LoadImage name "conga.tga" filename "conga.tga"');
  AddCongaPresents(SC,ANIM,notrotatedwidth);
  AddCongaWagons(SC,ANIM,rotatedwidth);
  SC.Add('FreeImage name "conga.tga"');
end;

procedure AddRotatedExplosion(SC,ANIM:TStringList;var notrotatedwidth:integer);
var ie:integer;
begin
  ie:=Images.IndexOf('c1');
  if ie=-1 then raise Exception.Create('Engine image not found!');
  // copy the first two frames H flipped
  SC.Add(Format('CopyPartialImage source "notrotated" leftb %d top 0 target "notrotated" leftb %d top 0 widthb 1 heightb 1 transform "H"',
    [Frames[Images[ie]._frames[0]]._notrotatedoffset,notrotatedwidth]));
  SC.Add(Format('CopyPartialImage source "notrotated" leftb %d top 0 target "notrotated" leftb %d top 0 widthb 1 heightb 1 transform "H"',
    [Frames[Images[ie]._frames[1]]._notrotatedoffset,notrotatedwidth+1]));
  // copy the first two frames rotated
  SC.Add(Format('CopyPartialImage source "notrotated" leftb %d top 0 target "notrotated" leftb %d top 0 widthb 1 heightb 1 transform "HR"',
    [Frames[Images[ie]._frames[0]]._notrotatedoffset,notrotatedwidth+2]));
  SC.Add(Format('CopyPartialImage source "notrotated" leftb %d top 0 target "notrotated" leftb %d top 0 widthb 1 heightb 1 transform "HR"',
    [Frames[Images[ie]._frames[1]]._notrotatedoffset,notrotatedwidth+3]));
  // copy the first two frames Rotated
  SC.Add(Format('CopyPartialImage source "notrotated" leftb %d top 0 target "notrotated" leftb %d top 0 widthb 1 heightb 1 transform "Hr"',
    [Frames[Images[ie]._frames[0]]._notrotatedoffset,notrotatedwidth+4]));
  SC.Add(Format('CopyPartialImage source "notrotated" leftb %d top 0 target "notrotated" leftb %d top 0 widthb 1 heightb 1 transform "Hr"',
    [Frames[Images[ie]._frames[1]]._notrotatedoffset,notrotatedwidth+5]));

  ANIM.Add(Format('Animation name "c1L" image "notrotated" widthb 1 heightb 1 FrameCount 6 Framedelay %d Framesb %d 0 %d 0 %d 0 %d 0 %d 0 %d 0',
    [Images[ie]._framedelay,notrotatedwidth,notrotatedwidth+1,Frames[Images[ie]._frames[2]]._notrotatedoffset,
    Frames[Images[ie]._frames[3]]._notrotatedoffset,Frames[Images[ie]._frames[4]]._notrotatedoffset,Frames[Images[ie]._frames[5]]._notrotatedoffset]));
  ANIM.Add(Format('Animation name "c1U" image "notrotated" widthb 1 heightb 1 FrameCount 6 Framedelay %d Framesb %d 0 %d 0 %d 0 %d 0 %d 0 %d 0',
    [Images[ie]._framedelay,notrotatedwidth+2,notrotatedwidth+3,Frames[Images[ie]._frames[2]]._notrotatedoffset,
    Frames[Images[ie]._frames[3]]._notrotatedoffset,Frames[Images[ie]._frames[4]]._notrotatedoffset,Frames[Images[ie]._frames[5]]._notrotatedoffset]));
  ANIM.Add(Format('Animation name "c1D" image "notrotated" widthb 1 heightb 1 FrameCount 6 Framedelay %d Framesb %d 0 %d 0 %d 0 %d 0 %d 0 %d 0',
    [Images[ie]._framedelay,notrotatedwidth+4,notrotatedwidth+5,Frames[Images[ie]._frames[2]]._notrotatedoffset,
    Frames[Images[ie]._frames[3]]._notrotatedoffset,Frames[Images[ie]._frames[4]]._notrotatedoffset,Frames[Images[ie]._frames[5]]._notrotatedoffset]));
  inc(notrotatedwidth,6);
end;

function CreateFrames(pFrames:TIntList;pRow:integer;rotated:boolean):string;
var i:integer;
begin
  Result:='';
  if rotated then
    for i:=0 to pFrames.Count-1 do
      result+=Format(' %d %d',[Frames[pFrames[i]]._rotatedoffset,pRow])
  else
    for i:=0 to pFrames.Count-1 do
      result+=Format(' %d %d',[Frames[pFrames[i]]._notrotatedoffset,pRow])
end;

procedure AddAnims(ANIM:TStringList);
var i:integer;flags:string;
begin
  for i:=0 to Images.Count-1 do with Images[i] do begin
    flags:='';
    if _looped then flags+=' looped';
    if _randomstart then flags+=' randomstart';
    if _paused then flags+=' paused';
    if _pingpong then flags+=' pingpong';
    if _needrotation then begin
      ANIM.Add(Format('Animation Name "%s" image "rotated" widthb 1 heightb 1 Framecount %d Framedelay %d loopdelay %d%s Framesb%s',
        [_ICName,_framecount,_framedelay,_loopdelay,flags,CreateFrames(_frames,0,true)]));
      ANIM.Add(Format('Animation Name "%sL" image "rotated" widthb 1 heightb 1 Framecount %d Framedelay %d loopdelay %d%s Framesb%s',
        [_ICName,_framecount,_framedelay,_loopdelay,flags,CreateFrames(_frames,1,true)]));
      ANIM.Add(Format('Animation Name "%sU" image "rotated" widthb 1 heightb 1 Framecount %d Framedelay %d loopdelay %d%s Framesb%s',
        [_ICName,_framecount,_framedelay,_loopdelay,flags,CreateFrames(_frames,2,true)]));
      ANIM.Add(Format('Animation Name "%sD" image "rotated" widthb 1 heightb 1 Framecount %d Framedelay %d loopdelay %d%s Framesb%s',
        [_ICName,_framecount,_framedelay,_loopdelay,flags,CreateFrames(_frames,3,true)]));
    end else begin
      ANIM.Add(Format('Animation Name "%s" image "notrotated" widthb 1 heightb 1 Framecount %d Framedelay %d loopdelay %d%s Framesb%s',
        [_ICName,_framecount,_framedelay,_loopdelay,flags,CreateFrames(_frames,0,false)]));
    end;
  end;
end;

procedure CreateCode;
var
  SC,ANIM:TStringList;
  rotatedwidth,notrotatedwidth:integer;
  i,infileindex,srcindex:integer;
  pre,now,cnt:integer;
  LoadedSheets:TStringList;
  s:string;
begin
  SC:=TStringList.Create;
  ANIM:=TStringList.Create;
  ANIM.Add('ASHScript 1.2 revision 4');
  ANIM.Add('BlockSize width 32 height 32');
  ANIM.Add('sub type Animations name "Anims"');

  SC.Add('ASHScript 1.2 revision 4');
  SC.Add('sub type ImageScript name "Images"');
  SC.Add('BlockSize width 32 height 32');
  SC.Add('NewImage name "notrotated" widthb %d heightb 1');
  SC.Add('NewImage name "rotated" widthb %d heightb 4');
  LoadedSheets:=TStringList.Create;
  for i:=0 to FrameOrder.Count-1 do begin
    s:='sprites'+inttostr(Frames[FrameOrder[i]]._fileindex);
    if LoadedSheets.IndexOf(s+'.tga')=-1 then begin
      SC.Add(Format('LoadImage name "%s.tga" filename "%s.tga"',[s,s]));
      SC.Add(Format('RearrangeTilesV2H name "%s.tga" width 32 height 32',[s]));
      LoadedSheets.Add(s+'.tga');
    end;
  end;
  rotatedwidth:=0;
  notrotatedwidth:=0;
  srcindex:=-1;
  pre:=0;cnt:=0;infileindex:=0;
  for i:=0 to FrameOrder.Count-1 do begin
    if Frames[FrameOrder[i]]._fileindex<>srcindex then begin
      CopyToSheets(SC,pre,srcindex,infileindex,cnt,notrotatedwidth,rotatedwidth);
      srcindex:=Frames[FrameOrder[i]]._fileindex;
      infileindex:=0;pre:=0;cnt:=0;
    end;
    now:=0;
    if Frames[FrameOrder[i]]._usedinnotrotated then now:=now or 1;
    if Frames[FrameOrder[i]]._usedinrotated then now:=now or 2;
//    Log.Trace(Format('%d. %d (nr=%d, r=%d)',[i,Frameorder[i],Frames[FrameOrder[i]]._notrotatedoffset,Frames[FrameOrder[i]]._rotatedoffset]));
    if pre=now then
      inc(cnt)
    else begin
      CopyToSheets(SC,pre,srcindex,infileindex,cnt,notrotatedwidth,rotatedwidth);
      infileindex+=cnt;
      cnt:=1;
      pre:=now;
    end;
    if Frames[FrameOrder[i]]._usedinnotrotated then
      Frames[FrameOrder[i]]._notrotatedoffset:=notrotatedwidth+cnt-1;
    if Frames[FrameOrder[i]]._usedinrotated then
      Frames[FrameOrder[i]]._rotatedoffset:=rotatedwidth+cnt-1;
  end;
  CopyToSheets(SC,pre,srcindex,infileindex,cnt,notrotatedwidth,rotatedwidth);

  AddAnims(ANIM);
  AddStoppedTrain(SC,ANIM,rotatedwidth);
  AddConga(SC,ANIM,rotatedwidth,notrotatedwidth);
  AddRotatedExplosion(SC,ANIM,notrotatedwidth);

  for i:=0 to LoadedSheets.Count-1 do
    SC.Add(Format('FreeImage name "%s"',[LoadedSheets[i]]));
  FreeAndNil(LoadedSheets);


  SC.Add(Format('CopyPartialImageRepeat count %d deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 1 widthb 1 heightb 1 transform "H"',[rotatedwidth]));
  SC.Add(Format('CopyPartialImageRepeat count %d deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 2 widthb 1 heightb 1 transform "HR"',[rotatedwidth]));
  SC.Add(Format('CopyPartialImageRepeat count %d deltaleftb 1 deltatop 0 source "rotated" leftb 0 top 0 target "rotated" leftb 0 topb 3 widthb 1 heightb 1 transform "Hr"',[rotatedwidth]));
  SC[3]:=Format(SC[3],[notrotatedwidth]);
  SC[4]:=Format(SC[4],[rotatedwidth]);
  ANIM.SaveToFile('anims.ash');
  FreeAndNil(ANIM);
  SC.SaveToFile('sprites.ash');
  FreeAndNil(SC);
end;

begin
  Log.SetLogLevel(llStatus);
  Frames:=TFrames.Create;
  LogTicks.LogTicks('Start');
  LoadImages('Allsprites.csv');
  LogTicks.LogTicks('LoadImages');
  Log.LogStatus(Format('%d frames loaded.',[Frames.Count]));
  FrameOrder:=TIntList.Create;
  OrganizeFrames;
  SaveSheets;
  CreateCode;
{  SC:=TStringList.Create;
  ANIM:=TStringList.Create;
  rotatedwidth:=0;
  notrotatedwidth:=0;
  BestCount:=99999;
  Best:=-1;
  for i:=0 to Images.Count-1 do TryCollect(i);

  TryCollect(Best,true);
  SC.SaveToFile('sprites.ash');
  FreeAndNil(ANIM);
  FreeAndNil(SC);}
  FreeAndNil(FrameOrder);
  FreeAndNil(Images);
  FreeAndNil(Frames);
end.

