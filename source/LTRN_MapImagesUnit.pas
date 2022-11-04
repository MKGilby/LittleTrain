{$mode delphi}{$smartlink on}

unit LTRN_MapImagesUnit;

interface

uses mk_sdl2, Lists, LTRN_MapListUnit, ARGBImageUnit;

type

  { TMapImages }

  TMapImages=class(TNamedList<TARGBImage>)
  public
    constructor Create(iMapList:TMapList);
    destructor Destroy; override;
    procedure CreateMapImage(pNextMap:integer);
  private
    fThumbs:TARGBImage;
    fMapList:TMapList;
  end;

implementation

uses sysutils, MKToolBox, Logger{, LTRN_SharedUnit};

constructor TMapImages.Create(iMapList:TMapList);
var i:integer;
begin
  inherited Create;
  fMapList:=iMapList;
  fThumbs:=TARGBImage.Create('thumbs.png');
  AddObject('?',TARGBImage.Create('unknown.png'));
  for i:=0 to fMapList.Count-1 do CreateMapImage(i);
end;

destructor TMapImages.Destroy;
begin
  if Assigned(fThumbs) then FreeAndNil(fThumbs);
  inherited Destroy;
end;

procedure TMapImages.CreateMapImage(pNextMap:integer);
const THUMBCHARS='#%!abcdefghijklmnopqr';
var Thumb:TARGBImage;Map:TRawMap;i,j,k:integer;
begin
  Thumb:=TARGBImage.Create(160,96);
  Thumb.Clear;

  Map:=fMapList.Items[pNextMap];
  for i:=0 to 19 do
    for j:=0 to 11 do begin
      k:=pos(chr(Map.Tiles[i,j]),THUMBCHARS);
      if (Map.Tiles[i,j]<>32) and (k>0) then
        fThumbs.CopyTo((k-1)*8,0,8,8,i*8,j*8,Thumb);
//        Thumb.PutImage(i<<3,j<<3,Sprites.FindImage(chr(Map.Tiles[i,j])+'~'));
    end;
  AddObject(st(pNextMap,2,'0'),Thumb);
end;

end.
