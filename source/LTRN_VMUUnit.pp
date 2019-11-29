{$mode delphi}
{$smartlink on}

unit LTRN_VMUUnit;

interface

uses PlayerRegistryUnit, BASS, SDL;

const
  VMUFileName:string='LittleTrain.vmu';
  LevelPackName:string='<internal>';
  CFG=#0'*CFG';

type
  TVMU=class(TPlayerRegistry)
    constructor Create;
    destructor Destroy; override;
    function GetName(iSlot:integer):string;
    function GetMapState(iSlot,iMapNo:integer):integer;
    procedure SetMapState(iSlot,iMapNo,iState:integer);
    procedure CompleteAllMaps(iSlot:integer);
  private
    fSoundVolume, fMusicVolume: float;
  public
    property SoundVolume:float read fSoundVolume write fSoundVolume;
    property MusicVolume:float read fMusicVolume write fMusicVolume;
  end;

var VMU:TVMU;

implementation

uses SysUtils, Logger;

const Fstr='LTRN_VMUUnit.pp, ';

constructor TVMU.Create;
var i:integer;
begin
  inherited Create;
  Verbose:=false;
  Load(VMUFileName);
  AddLevelPack(LevelPackName);
  for i:=fPlayers.Count+1 to 5 do AddPlayer('???'+inttostr(i));
  AddLevelPack(CFG);
  AddPlayer(CFG);
  if not ReadData(CFG,CFG,0,sizeof(Float),fSoundVolume) then fSoundVolume:=1;
  if not ReadData(CFG,CFG,sizeof(Float),sizeof(Float),fMusicVolume) then fMusicVolume:=1;
end;

destructor TVMU.Destroy;
begin
  WriteData(CFG,CFG,0,sizeof(Float),fSoundVolume);
  WriteData(CFG,CFG,sizeof(Float),sizeof(Float),fMusicVolume);
  Save(VMUFileName);
  inherited ;
end;

function TVMU.GetName(iSlot:integer):string;
const Istr=Fstr+'TVMU.GetName';
begin
  if iSlot in [1..5] then
    Result:=Players[iSlot-1]._Name
  else Log.LogWarning('Invalid slot number! (Got: '+inttostr(iSlot)+'; Should be: 1..5)',Istr);
end;

function TVMU.GetMapState(iSlot,iMapNo:integer):integer;
const Istr=Fstr+'TVMU.GetMapState';
var i,j:integer;
begin
  if iSlot in [1..5] then
    if (iMapNo in [0..50]) then begin
      i:=0;
      j:=fLevelPacks.IndexOf(LevelPackName);
      if ReadData(iSlot-1,j,iMapNo*2,2,i) then Result:=i
                                                               else Result:=0;
    end else Log.LogWarning('Invalid map number! (Got: '+inttostr(iMapNo)+'; Should be: 0..49)',Istr)
  else Log.LogWarning('Invalid slot number! (Got: '+inttostr(iSlot)+'; Should be: 1..5)',Istr);
end;

procedure TVMU.SetMapState(iSlot,iMapNo,iState:integer);
const Istr=Fstr+'TVMU.SetMapState';
var i:integer;
begin
  if iSlot in [1..5] then
    if iMapNo in [0..50] then begin
      i:=fLevelPacks.IndexOf(LevelPackName);
      WriteData(iSlot-1,i,iMapNo*2,2,iState)
    end
    else Log.LogWarning('Invalid map number! (Got: '+inttostr(iMapNo)+'; Should be: 0..49)',Istr)
  else Log.LogWarning('Invalid slot number! (Got: '+inttostr(iSlot)+'; Should be: 1..5)',Istr);
end;

procedure TVMU.CompleteAllMaps(iSlot:integer);
const Istr=Fstr+'TVMU.CompleteAllMaps';
var i,j:integer;
begin
  if iSlot in [1..5] then begin
    j:=128;
    for i:=0 to 49 do WriteData(iSlot-1,fLevelPacks.IndexOf(LevelPackName),i*2,2,j);
    j:=0;
    WriteData(iSlot-1,fLevelPacks.IndexOf(LevelPackName),0,2,j);
  end else Log.LogWarning('Invalid slot number! (Got: '+inttostr(iSlot)+'; Should be: 1..5)',Istr);
end;

initialization
  Log.LogStatus('Initializing VMU...','LTRN_VMUUnit.pp, Initialize');
  VMU:=TVMU.Create;
//  VMU.CompleteAllMaps(1);
  Log.LogStatus('VMU initialized.','LTRN_VMUUnit.pp, Initialize');

finalization
  Log.LogStatus('Freeing VMU...','LTRN_VMUUnit.pp, Finalize');
//  VMU.Verbose:=false;
  FreeAndNil(VMU);
  Log.LogStatus('VMU freed.','LTRN_VMUUnit.pp, Finalize');

end.
