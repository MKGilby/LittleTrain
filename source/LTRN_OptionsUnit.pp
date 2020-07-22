{$mode delphi}
{$smartlink on}

unit LTRN_OptionsUnit;

interface

uses vcc_Menu;

type
  TOptions=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fMenu:TMenu;
  end;

var
  Options:TOptions;

implementation

uses SysUtils, SDL, MK_SDL, Logger, Bass, LTRN_VMUUnit, LTRN_SharedUnit,
     ImageUnit, FontUnit;

const
  X=320;Y=240;Width=520;Height=144;

constructor TOptions.Create;
begin
  fMenu:=TMenu.Create;
  fMenu.Left:=X-Width div 2;
  fMenu.Top:=Y-Height div 2;
  fMenu.Width:=Width;
  fMenu.Height:=Height;
  fMenu.CycleOptions:=false;
  fMenu.BaseFont:=Fonts.FontByIndex[8];
  fMenu.HighlightFont:=Fonts.FontByIndex[1];
  fMenu.HeaderFont:=Fonts.FontByIndex[5];
  fMenu.TextAlign:=mjCenter;
  fMenu.TextOffsetY:=8;
  fMenu.RowHeight:=36;
//  fMenu.AddItemExt('"@SETTINGS" "Back=B" "%MV%MUSIC VOLUME: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
//  fMenu.AddItemExt('"%SV%EFFECTS VOLUME {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
  fMenu.AddItemExt('"@SETTINGS" "Back=B" "%MV%Music volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
  fMenu.AddItemExt('"%SV%Effects volume {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
  fMenu.SetValue('MV',inttostr(trunc(VMU.MusicVolume*10)));
  fMenu.SetValue('SV',inttostr(trunc(VMU.SoundVolume*10)));
end;

destructor TOptions.Destroy;
begin
  FreeAndNil(fMenu);
  inherited ;
end;

procedure TOptions.Run;
var mv,sv:float;atm:TImage;
begin
  fMenu.State:=msActive;
  fMenu.SelectedLabel:='B';
  mv:=VMU.MusicVolume;
  sv:=VMU.SoundVolume;
  atm:=TImage.Create(640,480);
  GetImage(0,0,atm);
  atm.SetAlpha(64);
//  SDL_SetAlpha(atm.surface,SDL_SRCALPHA or SDL_RLEACCEL or SDL_HWACCEL,64);
  ClearScreen(0,0,0);
  PutImage(0,0,atm);
  ClearKeys;
  repeat
    fMenu.Move;
    if keys[OptionsKey] then begin
      fMenu.SelectedLabel:='B';
      fMenu.State:=msEscaped;
      keys[OptionsKey]:=false;
    end;
    mv:=strtoint(fMenu.GetValue('MV'))/10;
    if VMU.MusicVolume<>mv then begin
      VMU.MusicVolume:=mv;
      Muzax.GlobalVolume:=mv;
    end;
    sv:=strtoint(fMenu.GetValue('SV'))/10;
    if VMU.SoundVolume<>sv then begin
      VMU.SoundVolume:=sv;
      Waves.GlobalVolume:=sv;
    end;
//    bar(0,316,639,395,0,0,0);
//    putimage(0,336+trunc(sin(ffase*2.25*pi/180)*20),FBar);
//    bar(0,180,639,207,0,0,0);
    barWH(x-width div 2+1,y-height div 2+1,width-2,Height-2,0,0,0);
    rectangleWH(x-width div 2,y-height div 2,width,Height,112,112,112);
    fMenu.Draw;
    Flip;
    HandleMessages;
{    if keys[SDLK_A] then begin
      ScreenShot;
      keys[SDLK_A]:=false;
    end;}
//  until (keys[SDLK_Return] or keys[SDLK_Space]) or (keys[SDLK_Escape] and (fMenu.Selected=3));
{    if fMenu.Finished and ((fMenu.Exitkey=SDLK_Space) or (fMenu.Exitkey=SDLK_Return)) then begin
      if (fMenu.SelectedLabel='SV') or (fMenu.SelectedLabel='MV') then begin
        fMenu.Finished:=false;
      end;
    end;}
  until fMenu.State<>msActive;
  SDL_SetAlpha(atm.surface,SDL_SRCALPHA or SDL_RLEACCEL or SDL_HWACCEL,255);
  PutImage(0,0,atm);
  FreeAndNIL(atm);
end;

end.
