{$mode delphi}
{$smartlink on}

unit LTRN_ScrollUnit;

interface

uses Scroll2Unit;

type

  { TLTRN_Scroll }

  TLTRN_Scroll=class(TScroll)
    constructor Create;
    procedure Move2(pPixels:integer=1);
  private
    fStepCount:integer;
  public
    property StepCount:integer read fStepCount;
  end;

implementation

uses LTRN_SharedUnit;

constructor TLTRN_Scroll.Create;
begin
  inherited Create(MM.Fonts['4'],0,452,640,20,0);
  LoadText('scroll.txt');
  fStepCount:=0;
end;

procedure TLTRN_Scroll.Move2(pPixels:integer);
begin
  inc(fStepCount,pPixels);
  Move(pPixels);
end;

end.
