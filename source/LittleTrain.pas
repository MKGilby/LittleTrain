// This version: LittleTrain (C) 2006-2022 MKSZTSZ  *** Absolutely Freeware! ***
// Original ver: Vlak        (C) 1993 Miroslav Nemecek (PC EGA)

{$mode delphi}
{$smartlink on}
{$apptype gui}

{$R LittleTrain.res}

uses
  SysUtils,
  LTRN_MainUnit,
  ARGBImagePNGReaderUnit;

const
  Version='1.10';
  BDate='2022.11.21';

var
  Main:TMain;

begin
  Main:=TMain.Create(Version, BDate);
  Main.Run;
  FreeAndNil(Main);
end.

