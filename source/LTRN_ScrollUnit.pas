{$mode delphi}
{$smartlink on}

unit LTRN_ScrollUnit;

interface

uses Scroll2Unit;

type
  TLTRN_Scroll=class(TScroll)
    constructor Create;
  end;

implementation

uses LTRN_SharedUnit;

constructor TLTRN_Scroll.Create;
begin
  inherited Create(MM.Fonts['4'],0,452,640,20,0);
  LoadText('scroll.txt');
end;

end.
