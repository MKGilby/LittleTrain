{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit LTRN_CurtainUnit;

interface

uses Animation2Unit;

type

  { TCurtain }

  TCurtain=class
    constructor Create;
    destructor Destroy; override;
    procedure StartClose;
    procedure StartOpen;
    procedure Draw;
  public
    State:integer;
  private
    fAnimation:TAnimation;
//    fTexture:TTexture;
    fFase,fDirection:integer;
  end;


implementation

uses SysUtils, LTRN_SharedUnit;

constructor TCurtain.Create;
begin
  fAnimation:=MM.Animations.ItemByName['#'].SpawnAnimation;
//  fTexture:=MM.Animations.ItemByName['#'].SpawnAnimation.p;
  fFase:=-1;
  State:=0;
end;

destructor TCurtain.Destroy;
begin
  if Assigned(fAnimation) then FreeAndNil(fAnimation);
  inherited Destroy;
end;

procedure TCurtain.StartClose;
begin
  fDirection:=1;
  fFase:=0;
  State:=1;
end;

procedure TCurtain.StartOpen;
begin
  fDirection:=-1;
  fFase:=29;
  State:=2;
end;

procedure TCurtain.Draw;
var i,j:integer;
begin
  if State=3 then State:=0;  // Ez azért kell, hogy 1 ciklusig 3-assal jelzi,
                             // hogy éppen akkor fejezte be.
  if fFase>-1 then begin
    for j:=0 to fFase>>1 do
      for i:=0 to 19 do
        fAnimation.PutFrame(i<<5,j<<5,0);
    fFase+=fDirection;
    if fFase=30 then begin
      fFase:=-1;
      State:=3;
    end else
      if fFase=-1 then State:=3;
  end;
end;

end.
