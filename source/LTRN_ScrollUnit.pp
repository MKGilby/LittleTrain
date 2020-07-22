{$mode delphi}
{$smartlink on}

unit LTRN_ScrollUnit;

interface

uses ScrollUnit;

type
  TLTRN_Scroll=class(TScroll)
    constructor Create;
  end;

var Scroll:TLTRN_Scroll;

implementation

uses LTRN_SharedUnit;

constructor TLTRN_Scroll.Create;
begin
  inherited Create(Fonts.FontByIndex[4],452,0,20);
  LoadText('scroll.txt');
end;

end.
