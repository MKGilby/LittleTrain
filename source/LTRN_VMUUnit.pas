{$mode delphi}
{$smartlink on}

unit LTRN_VMUUnit;

interface

uses PlayerRegistryUnit, BASS;

const
  VMUFILENAME:string='LittleTrain.vmu';
  LEVELPACKNAME:string='<internal>';

type

  { TVMU }

  TVMU=class(TPlayerRegistry)
    constructor Create;
    destructor Destroy; override;
    procedure SelectSlot(pSlot:integer);
    function GetMapState(iMapNo:integer):integer;
    procedure SetMapState(iMapNo,iState:integer);
    procedure ClearSlot;
    function GetCompletedMapCount:integer;
    procedure CompleteAllMaps;
  private
    fSoundVolume, fMusicVolume: float;
    fFullScreen:boolean;
    fSpeed:integer;
    fScalingQuality:integer;
    fSlot:integer;
    fLevelPackID:integer;
    procedure fSetSpeed(value:integer);
  public
    property SoundVolume:float read fSoundVolume write fSoundVolume;
    property MusicVolume:float read fMusicVolume write fMusicVolume;
    property FullScreen:boolean read fFullScreen write fFullScreen;
    property Speed:integer read fSpeed write fSetSpeed;
    property Slot:integer read fSlot write SelectSlot;
    property ScalingQuality:integer read fScalingQuality write fScalingQuality;
  end;

var VMU:TVMU;

implementation

uses SysUtils, Logger;

const
  CFG=#0'*CFG';

constructor TVMU.Create;
var i:integer;
begin
  inherited Create;
  Verbose:=false;
  Load(VMUFILENAME);
  AddLevelPack(LEVELPACKNAME);
  for i:=fPlayers.Count to 4 do AddPlayer(chr(i));
  AddLevelPack(CFG);
  AddPlayer(CFG);
  i:=0;
  fSlot:=-1;
  fSpeed:=-1;
  if not ReadData(CFG,CFG,0,sizeof(Float),fSoundVolume) then fSoundVolume:=1;
  if not ReadData(CFG,CFG,sizeof(Float),sizeof(Float),fMusicVolume) then fMusicVolume:=1;
  if not ReadData(CFG,CFG,2*sizeof(Float),1,i) then begin
    fFullScreen:=false;
    fScalingQuality:=0;
  end else begin
    fFullScreen:=(i and 1=1);
    fScalingQuality:=(i and $06)>>1;
  end;
  fLevelPackID:=LevelPacks.IndexOf(LEVELPACKNAME);
end;

destructor TVMU.Destroy;
var b:byte;
begin
  WriteData(CFG,CFG,0,sizeof(Float),fSoundVolume);
  WriteData(CFG,CFG,sizeof(Float),sizeof(Float),fMusicVolume);
  if fFullScreen then b:=1 else b:=0;
  b+=fScalingQuality*2;
  WriteData(CFG,CFG,2*sizeof(Float),1,b);
  Save(VMUFILENAME);
  inherited ;
end;

procedure TVMU.SelectSlot(pSlot:integer);
begin
  // -1 means unselect slot
  if (pSlot>=-1) and (pSlot<=4) then begin
    if fSlot<>-1 then WriteData(fSlot,fLevelPackID,104,1,fSpeed);
    fSlot:=pSlot;
    fSpeed:=0;
    if fSlot<>-1 then begin
      if not ReadData(fSlot,fLevelPackID,104,1,fSpeed) then fSpeed:=3;
    end else
      fSpeed:=-1;
  end else Log.LogWarning('Invalid slot number! (Got: '+inttostr(pSlot)+'; Should be: -1..4)');
end;

function TVMU.GetMapState(iMapNo:integer):integer;
//const Istr=Fstr+'TVMU.GetMapState';
var i:integer;
begin
  if fSlot in [0..4] then
    if (iMapNo in [0..50]) then begin
      i:=0;
      if ReadData(fSlot,fLevelPackID,iMapNo*2,2,i) then
        Result:=i
      else
        Result:=0;
    end else Log.LogWarning('Invalid map number! (Got: '+inttostr(iMapNo)+'; Should be: 0..49)')
  else Result:=0;
end;

procedure TVMU.SetMapState(iMapNo,iState:integer);
begin
  if fSlot in [0..4] then
    if iMapNo in [0..50] then begin
      WriteData(fSlot,fLevelPackID,iMapNo*2,2,iState)
    end
    else Log.LogWarning('Invalid map number! (Got: '+inttostr(iMapNo)+'; Should be: 0..49)')
  else begin
    Log.LogWarning('Invalid slot number! (Got: '+inttostr(fSlot)+'; Should be: 0..4)');
    Log.LogWarning('Missed calling SelectSlot?');
  end;
end;

procedure TVMU.ClearSlot;
begin
  ClearData(fSlot,fLevelPackID);
end;

function TVMU.GetCompletedMapCount:integer;
var i:integer;
begin
  if fSlot in [0..4] then begin
    Result:=0;
    for i:=0 to 49 do
      if GetMapState(i)>0 then inc(Result);
  end else Result:=0;
end;

procedure TVMU.CompleteAllMaps;
var i:integer;
begin
  if fSlot in [0..4] then begin
    for i:=0 to 49 do SetMapState(i,128);
  end else begin
    Log.LogWarning('Invalid slot number! (Got: '+inttostr(fSlot)+'; Should be: 0..4)');
    Log.LogWarning('Missed calling SelectSlot?');
  end;
end;

procedure TVMU.fSetSpeed(value:integer);
begin
  if value in [0..5] then begin
    fSpeed:=value;
  end;
end;

end.
