// This version: LittleTrain (C) 2006-2019 MKSZTSZ  *** Absolutely Freeware! ***
// Original ver: Vlak        (C) 1993 Miroslav Nemecek (PC EGA)

{$mode delphi}
{$smartlink on}
{apptype gui}

{$R LittleTrain.res}

uses SysUtils, LTRN_MainUnit, RawPictureTGAUnit, LTRN_MapPlayUnit, LTRN_MapCongratsUnit;

{!Build} const Build='0509'; BDate='2019.11.29';
         const Version='0.1';

var
  Main:TMain;

begin
  Main:=TMain.Create(Version, Build, BDate);
  Main.Run;
  FreeAndNil(Main);
end.

