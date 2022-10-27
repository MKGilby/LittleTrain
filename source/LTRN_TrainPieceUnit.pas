unit LTRN_TrainPieceUnit;

{$mode Delphi}

interface

uses AnimatedSprite2Unit, Animation2Unit;

type

  { TTrainPiece }

  TTrainPiece=class(TAnimatedSprite)
    constructor Create(iX,iY:integer;iType:char);
    destructor Destroy; override;
    procedure FaceLeft;
    procedure FaceRight;
    procedure FaceUp;
    procedure FaceDown;
  private
    fAnimLeft,fAnimRight,fAnimUp,fAnimDown:TAnimation;
    fDir:char;
  public
    property Dir:char read fDir;
  end;

implementation

uses SysUtils, LTRN_SharedUnit;

{ TTrainPiece }

constructor TTrainPiece.Create(iX,iY:integer; iType:char);
begin
  fAnimLeft:=MM.Animations.ItemByName[iType+'L'].SpawnAnimation;
  fAnimRight:=MM.Animations.ItemByName[iType].SpawnAnimation;;
  fAnimUp:=MM.Animations.ItemByName[iType+'U'].SpawnAnimation;
  fAnimDown:=MM.Animations.ItemByName[iType+'D'].SpawnAnimation;
  inherited Create(iX,iy,fAnimRight);
  fDir:='R';
end;

destructor TTrainPiece.Destroy;
begin
  if Assigned(fAnimDown) then FreeAndNil(fAnimDown);
  if Assigned(fAnimUp) then FreeAndNil(fAnimUp);
  if Assigned(fAnimRight) then FreeAndNil(fAnimRight);
  if Assigned(fAnimLeft) then FreeAndNil(fAnimLeft);
  inherited Destroy;
end;

procedure TTrainPiece.FaceLeft;
begin
  SetAnimation(fAnimLeft);
  fDir:='L';
end;

procedure TTrainPiece.FaceRight;
begin
  SetAnimation(fAnimRight);
  fDir:='R';
end;

procedure TTrainPiece.FaceUp;
begin
  SetAnimation(fAnimUp);
  fDir:='U';
end;

procedure TTrainPiece.FaceDown;
begin
  SetAnimation(fAnimDown);
  fDir:='D';
end;

end.

