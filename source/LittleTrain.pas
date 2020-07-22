// This version: LittleTrain (C) 2006-2019 MKSZTSZ  *** Absolutely Freeware! ***
// Original ver: Vlak        (C) 1993 Miroslav Nemecek (PC EGA)

{$mode delphi}
{$smartlink on}
{$apptype gui}

{$R LittleTrain.res}

uses
  SysUtils,
  LTRN_MainUnit,
  RawPictureTGAUnit,  // Expand TRawPicture with TGA reading capability
  ASH_ISInterpreterUnit,  // Expand TASHInterpreter with ImageScript processing capability
  ASH_ANInterpreterUnit,  // Expand TASHInterpreter with Animation definition processing capability
  ASH_MUInterpreterUnit,  // Expand TASHInterpreter with Music processing capability
  ASH_WAInterpreterUnit,  // Expand TASHInterpreter with Wave processing capability
  ASH_FOInterpreterUnit;  // Expand TASHInterpreter with Font processing capability

{!Build} const Build='0001'; BDate='2020.01.15';
         const Version='1.00';

var
  Main:TMain;

begin
  Main:=TMain.Create(Version, Build, BDate);
  Main.Run;
  FreeAndNil(Main);
end.

