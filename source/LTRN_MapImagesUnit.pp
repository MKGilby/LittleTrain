{$mode delphi}{$smartlink on}

unit LTRN_MapImagesUnit;

interface

uses ImageListUnit, LTRN_MapListUnit;

// Map thumbnails in an imagecollection. Can create them 1 by 1 so you can load
// them while showing the intro. But what if the user skips the intro?

type
  TMapImages=class(TImageList)
    constructor Create(iMapList:TMapList);
    procedure CreateNextMapImage;
  private
    fMapList:TMapList;
    fNextMap:integer;
    fFinished:boolean;
  public
    property Finished:boolean read fFinished;
  end;

var MapImages:TMapImages;

implementation

uses sysutils, MKToolBox, ImageUnit, Logger, LTRN_SharedUnit;

constructor TMapImages.Create(iMapList:TMapList);
begin
  inherited Create;
  fMapList:=iMapList;
  fNextMap:=0;
  Add('unknown.tga','?');
  fFinished:=false;
end;

procedure TMapImages.CreateNextMapImage;
var Thumb:TImage;Map:TRawMap;i,j:integer;
begin
  if Finished then exit;
  Thumb:=TImage.Create(160,96);

  Map:=fMapList.Items[fNextMap];
  for i:=0 to 19 do
    for j:=0 to 11 do begin
      if Map.Tiles[i,j]<>32 then
        Thumb.PutImage(i<<3,j<<3,Sprites.FindImage(chr(Map.Tiles[i,j])+'~'));
    end;
  Add(Thumb,st(fNextMap,2,'0'));

  inc(fNextMap);
  fFinished:=(fNextMap=fMapList.Count);
end;

end.
