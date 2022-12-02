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
    procedure Draw; override;
  private
    fAnimLeft,fAnimRight,fAnimUp,fAnimDown:TAnimation;
    fDir:char;
    fType:char;
  public
    property Dir:char read fDir;
    property PieceType:char read fType;
  end;

  { TEngine }

  TEngine=class(TTrainPiece)
    constructor Create(iX,iY:integer);
    destructor Destroy; override;
    procedure Draw; override;
  private
    fPuffAnimLeft,fPuffAnimRight,fPuffAnimUp,fPuffAnimDown:TAnimation;
    fPuffing:boolean;
  public
    property Puffing:boolean read fPuffing write fPuffing;
  end;

implementation

uses SysUtils, LTRN_SharedUnit;

{ TEngine }

constructor TEngine.Create(iX,iY:integer);
begin
  inherited Create(iX,iy,'%');
  fPuffAnimLeft:=MM.Animations.ItemByName['~L'].SpawnAnimation;
  fPuffAnimRight:=MM.Animations.ItemByName['~'].SpawnAnimation;;
  fPuffAnimUp:=MM.Animations.ItemByName['~U'].SpawnAnimation;
  fPuffAnimDown:=MM.Animations.ItemByName['~D'].SpawnAnimation;
  fPuffing:=true;
end;

destructor TEngine.Destroy;
begin
  FreeAndNil(fPuffAnimLeft);
  FreeAndNil(fPuffAnimRight);
  FreeAndNil(fPuffAnimUp);
  FreeAndNil(fPuffAnimDown);
  inherited Destroy;
end;

procedure TEngine.Draw;
begin
  inherited Draw;
  if fPuffing then begin
    case fDir of
      'L':fPuffAnimLeft.PutFrame(x,y);
      'R':fPuffAnimRight.PutFrame(x,y);
      'U':fPuffAnimUp.PutFrame(x,y);
      'D':fPuffAnimDown.PutFrame(x,y);
    end;
    fPuffAnimLeft.Animate;
    fPuffAnimRight.Animate;
    fPuffAnimUp.Animate;
    fPuffAnimDown.Animate;
  end;
end;

{ TTrainPiece }

constructor TTrainPiece.Create(iX,iY:integer; iType:char);
begin
  fAnimLeft:=MM.Animations.ItemByName[iType+'L'].SpawnAnimation;
  fAnimRight:=MM.Animations.ItemByName[iType].SpawnAnimation;;
  fAnimUp:=MM.Animations.ItemByName[iType+'U'].SpawnAnimation;
  fAnimDown:=MM.Animations.ItemByName[iType+'D'].SpawnAnimation;
  inherited Create(iX,iy,fAnimRight,false);
  fDir:='R';
  fType:=iType;
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

procedure TTrainPiece.Draw;
begin
  if fType<>'%' then Animation.Animate;
  inherited Draw;
end;

end.

