unit LTRN_MapListUnit;

{$mode delphi}

interface

uses
  Classes, Lists;

type
  TRawMap=class
    constructor Create(iSource:TStream);
    destructor Destroy; override;
    procedure ChangeDoorToEndTrack;
    procedure Reset;
  private
    fOrigTiles,fTiles:pointer;
    fParScore:integer;
    fAutoPlay:boolean;
    fSolutionPresent:boolean;
    fCongratulations:boolean;
    fSolution:string;
    fMapSize:integer;
    function fGetTile(x,y:integer):integer;
    procedure fSetTile(x,y,value:integer);
  public
    property Tiles[x,y:integer]:integer read fGetTile write fSetTile;
    property ParScore:integer read fParScore;
    property Solution:string read fSolution;
    property AutoPlay:boolean read fAutoPlay;
    property Congratulations:boolean read fCongratulations;
  end;

  TMapList=class(TGenericList<TRawMap>)
    constructor Create(iFilename:String); overload;
    constructor Create(iSource:TStream); overload;
  end;


implementation

uses sysutils, MKStream, Logger;

constructor TRawMap.Create(iSource:TStream);
var b:byte;i,j:integer;
begin
  fMapSize:=20*12*sizeof(integer);
  getmem(fTiles,fMapSize);
  getmem(fOrigTiles,fMapSize);

  // Check signature
  i:=0;
  iSource.Read(i,2);
  if i<>$544C then exit;

  // Read flags
  iSource.Read(i,1);
  fSolutionPresent:=(i and 1)<>0;
  fAutoPlay:=(i and 2)<>0;
  fCongratulations:=(i and 4)<>0;

  // Read ParScore
  fParScore:=0;
  iSource.Read(fParScore,1);

  // Read wall data
  b:=0;
  for j:=0 to 29 do begin
    iSource.Read(b,1);
    for i:=0 to 7 do begin
      if b and 128<>0 then
        integer((fOrigTiles+(j*8+i)*sizeof(integer))^):=35
      else
        integer((fOrigTiles+(j*8+i)*sizeof(integer))^):=32;
      b:=(b and $7f)<<1;
    end;
  end;

  // Read start position
  i:=0;
  iSource.Read(i,1);
  integer((fOrigTiles+i*sizeof(integer))^):=37;

  // Read door position
  iSource.Read(i,1);
  integer((fOrigTiles+i*sizeof(integer))^):=33;

  // Read goodies
  j:=0;
  iSource.Read(b,1);
  while (j<26) and (b<>255) do begin
    while b>0 do begin
      iSource.Read(i,1);
      integer((fOrigTiles+i*sizeof(integer))^):=97+j;  // Goodies
      dec(b);
    end;
    iSource.Read(b,1);
    inc(j);
  end;
  if j=26 then iSource.Seek(-1,soFromCurrent);

  // Read solusion, if present
  i:=fParScore;
  fSolution:='';
  if fSolutionPresent then begin
    while i>0 do begin
      iSource.Read(b,1);
//      Log.Trace(hexstr(b,2));
      j:=4;
      while (j>0) and (i>0) do begin
        case (b and $c0)>>6 of
          0:fSolution+='U';
          1:fSolution+='R';
          2:fSolution+='D';
          3:fSolution+='L';
        end;
        b:=(b and $3f)<<2;
        dec(j);
        dec(i);
      end;
    end;
  end;
  Reset;
end;

destructor TRawMap.Destroy;
begin
  freemem(fOrigTiles,fMapSize);
  freemem(fTiles,fMapSize);
end;

function TRawMap.fGetTile(x,y:integer):integer;
begin
  Result:=integer((fTiles+(x+y*20)*sizeof(integer))^);
end;

procedure TRawMap.fSetTile(x,y,value:integer);
begin
  if (y*20+x>=0) and (y*20+x<240) then
    integer((fTiles+(x+y*20)*sizeof(integer))^):=value;
end;

procedure TRawMap.Reset;
begin
  move(fOrigTiles^,fTiles^,fMapSize);
end;

procedure TRawMap.ChangeDoorToEndTrack;
var i:integer;
begin
  for i:=0 to 239 do
    if integer((fOrigTiles+i*sizeof(integer))^)=33 then
      integer((fOrigTiles+i*sizeof(integer))^):=61;
  Reset;
end;

constructor TMapList.Create(iFilename:String);
var Stream:TStream;
begin
  Stream:=MKStreamOpener.OpenStream(iFilename);
  Create(Stream);
  FreeAndNil(Stream);
end;

constructor TMapList.Create(iSource:TStream);
begin
  inherited Create;
  while iSource.Position<iSource.Size do Add(TRawMap.Create(iSource));
  Self[Count-1].ChangeDoorToEndTrack;
  Log.LogStatus(Format('%d map(s) loaded.',[Count]));
end;

end.

