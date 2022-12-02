// This version: LittleTrain (C) 2006-2022 MKSZTSZ  *** Absolutely Freeware! ***
// Original ver: Vlak        (C) 1993 Miroslav Nemecek (PC EGA)

{$mode delphi}
{$smartlink on}
{$apptype gui}

{$R LittleTrain.res}

uses
  SysUtils, Logger,
  LTRN_MainUnit,
  LTRN_SharedUnit,
  ARGBImagePNGReaderUnit;

const
  Version='1.10';
  BDate={$i %DATE%};

var
  Main:TMain;
  res,firstrun:boolean;

begin
  firstrun:=true;
  ReturnTo:=rNone;
  repeat
    Main:=TMain.Create(Version, BDate);
    res:=Main.Run(firstrun);
    FreeAndNil(Main);
    firstrun:=false;
    if not res then Log.LogStatus('Restarting!');
  until res;
end.

