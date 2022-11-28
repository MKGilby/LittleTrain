{$mode delphi}
{$smartlink on}

unit LTRN_OptionsUnit;

interface

uses vcc_Menu2;

type

  { TOptions }

  TOptions=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fMenu:TMenu;
    fFullScreenChanged:boolean;
//    function fGetFullScreenChanged:boolean;
  public
    property FullScreenChanged:boolean read fFullScreenChanged;
  end;

implementation

uses SysUtils, mk_sdl2, Logger, Bass, LTRN_VMUUnit, LTRN_SharedUnit,
  Font2Unit;

const
  X=320;Y=240;Width=520;Height=36*5;

constructor TOptions.Create;
var Base,Sound,Video:string;
begin
  Base:='"@SETTINGS" "Sound settings=S" %s "Video settings=V" %s "Back=B"';
  Sound:='> "@Sound settings" '+
         '"%MV%Music volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}" '+
         '"%SV%Effects volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}" '+
         '"Back=B1" <';
  Video:='> "@Video settings" "%FS%Full screen: {No=N|Yes=Y}" "Apply=A" "Cancel=B2" <';
  fMenu:=TMenu.Create;
  fMenu.Left:=X-Width div 2;
  fMenu.Top:=Y-Height div 2;
  fMenu.Width:=Width;
  fMenu.Height:=Height;
  fMenu.CycleOptions:=false;
  fMenu.BaseFont:=MM.Fonts['8'];
  fMenu.HighlightFont:=MM.Fonts['1'];
  fMenu.HeaderFont:=MM.Fonts['5'];
  fMenu.TextAlign:=mjCenter;
  fMenu.TextOffsetY:=0;
  fMenu.RowHeight:=36;
//  fMenu.AddItemExt('"@SETTINGS" "Back=B" "%MV%MUSIC VOLUME: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
//  fMenu.AddItemExt('"%SV%EFFECTS VOLUME {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
(*  fMenu.AddItemExt('"@SETTINGS"');
  fMenu.AddItemExt('"%FS%Full screen: {Yes=Y|No=N}"');
  fMenu.AddItemExt('"%MV%Music volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
  fMenu.AddItemExt('"%SV%Effects volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"');
  fMenu.AddItemExt('"Back=B"');*)
  fMenu.AddItemExt(Format(Base,[Sound,Video]));
  if VMU.FullScreen then fMenu.SetValue('FS','Y') else fMenu.SetValue('FS','N');
  fMenu.SetValue('MV',inttostr(trunc(VMU.MusicVolume*10)));
  fMenu.SetValue('SV',inttostr(trunc(VMU.SoundVolume*10)));
  fFullScreenChanged:=false;
end;

destructor TOptions.Destroy;
begin
  FreeAndNil(fMenu);
  inherited ;
end;

procedure TOptions.Run;
var mv,sv:float;orgfs,fs:boolean;
begin
  fMenu.State:=msActive;
  fMenu.SelectedLabel:='S';
  mv:=VMU.MusicVolume;
  sv:=VMU.SoundVolume;
  PutTexture(0,0,MM.Textures.ItemByName['Shadow']);
  ClearKeys;
  orgfs:=VMU.FullScreen;
  fFullScreenChanged:=false;
  repeat
    fMenu.Move;
    if keys[OPTIONSKEY] then begin
      fMenu.SelectedLabel:='B';
      fMenu.State:=msEscaped;
      keys[OPTIONSKEY]:=false;
    end;
    mv:=strtoint(fMenu.GetValue('MV'))/10;
    if VMU.MusicVolume<>mv then begin
      VMU.MusicVolume:=mv;
      MM.Musics.GlobalVolume:=mv;
    end;
    sv:=strtoint(fMenu.GetValue('SV'))/10;
    if VMU.SoundVolume<>sv then begin
      VMU.SoundVolume:=sv;
      MM.Waves.GlobalVolume:=sv;
    end;
    fs:=(fMenu.GetValue('FS')='Y');
    if VMU.FullScreen<>fs then begin
      VMU.FullScreen:=fs;
    end;
    if fMenu.State=msSelected then begin
      if fMenu.SelectedLabel='B1' then begin
        fMenu.SelectedLabel:='S'; // Back
        fMenu.State:=msActive;
      end
      else if fMenu.SelectedLabel='B2' then begin
        if orgfs then fMenu.SetValue('FS','Y') else fMenu.SetValue('FS','N');
        VMU.FullScreen:=orgfs;
        fMenu.SelectedLabel:='V'; // Back
        fMenu.State:=msActive;
      end
      else if fMenu.SelectedLabel='A' then begin
        if (fMenu.GetValue('FS')='Y')=orgfs then begin
          fMenu.SelectedLabel:='V';
          fMenu.State:=msActive;
        end;
      end;
    end;
//    bar(0,316,639,395,0,0,0);
//    putimage(0,336+trunc(sin(ffase*2.25*pi/180)*20),FBar);
//    bar(0,180,639,207,0,0,0);
    bar(x-width div 2+1,y-height div 2+1,width-2,Height-2,0,0,0);
    rectangle(x-width div 2,y-height div 2,width,Height,112,112,112);
    fMenu.Draw;
    Flip;
    HandleMessages;
{    if keys[SDL_scancode_A] then begin
      ScreenShot;
      keys[SDL_scancode_A]:=false;
    end;}
//  until (keys[SDL_scancode_Return] or keys[SDL_scancode_Space]) or (keys[SDL_scancode_Escape] and (fMenu.Selected=3));
{    if fMenu.Finished and ((fMenu.Exitkey=SDL_scancode_Space) or (fMenu.Exitkey=SDL_scancode_Return)) then begin
      if (fMenu.SelectedLabel='SV') or (fMenu.SelectedLabel='MV') then begin
        fMenu.Finished:=false;
      end;
    end;}
  until fMenu.State<>msActive;
  if VMU.FullScreen<>orgfs then fFullScreenChanged:=true;
end;

{function TOptions.fGetFullScreenChanged:boolean;
begin
  Result:=fFullScreenChanged;
  fFullScreenChanged:=false;
end;}

end.
