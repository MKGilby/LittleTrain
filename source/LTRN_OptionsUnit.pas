{$mode delphi}
{$smartlink on}

unit LTRN_OptionsUnit;

interface

uses vcc_Menu2;

const
  VIDEO_SETTINGS=1;
  SOUND_SETTINGS=2;
  SPEED_SETTINGS=4;
  ALL_SETTINGS=VIDEO_SETTINGS+SOUND_SETTINGS+SPEED_SETTINGS;

type

  { TOptions }

  TOptions=class
    constructor Create;
    destructor Destroy; override;
    procedure Run(pSections:integer=ALL_SETTINGS);
  private
    fMenu:TMenu;
    fFullScreenChanged:boolean;
//    function fGetFullScreenChanged:boolean;
    procedure SetupMenu(pSections:integer);
  public
    property FullScreenChanged:boolean read fFullScreenChanged;
  end;

implementation

uses SysUtils, mk_sdl2, Logger, Bass, LTRN_VMUUnit, LTRN_SharedUnit,
  Font2Unit;

const
  X=320;Y=240;Width=520;Height=36*5;

constructor TOptions.Create;
begin
  fMenu:=TMenu.Create;
  fFullScreenChanged:=false;
end;

destructor TOptions.Destroy;
begin
  if Assigned(fMenu) then FreeAndNil(fMenu);
  inherited ;
end;

procedure TOptions.Run(pSections:integer);
var mv,sv:float;orgfs,fs:boolean;
begin
  SetupMenu(pSections);
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
    if fMenu.GetValue('MV')<>'' then begin
      mv:=strtoint(fMenu.GetValue('MV'))/10;
      if VMU.MusicVolume<>mv then begin
        VMU.MusicVolume:=mv;
        MM.Musics.GlobalVolume:=mv;
      end;
    end;
    if fMenu.GetValue('SV')<>'' then begin
      sv:=strtoint(fMenu.GetValue('SV'))/10;
      if VMU.SoundVolume<>sv then begin
        VMU.SoundVolume:=sv;
        MM.Waves.GlobalVolume:=sv;
      end;
    end;
    if fMenu.GetValue('FS')<>'' then begin
      fs:=(fMenu.GetValue('FS')='Y');
      if VMU.FullScreen<>fs then begin
        VMU.FullScreen:=fs;
      end;
    end;
    if fMenu.GetValue('GS')<>'' then begin
      VMU.Speed:=strtoint(fMenu.GetValue('GS'));
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

procedure TOptions.SetupMenu(pSections:integer);
var Base{,Speed,Sound,Video}:string;
begin
//  Log.Trace('Speed at options.create = '+inttostr(VMU.Speed));
  if pSections=0 then pSections:=ALL_SETTINGS;
  Base:='"@SETTINGS" ';
  if pSections and SPEED_SETTINGS<>0 then
    Base+='"%GS%Game speed: {Still=0|Slowest=1|Slow=2|Normal=3|Fast=4|Fastest=5}" ';
  if pSections and SOUND_SETTINGS<>0 then
    Base+='"Sound settings=S" > "@Sound settings" '+
          '"%MV%Music volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}" '+
          '"%SV%Effects volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}" '+
          '"Back=B1" < ';
  if pSections and VIDEO_SETTINGS<>0 then
    Base+='"Video settings=V" > "@Video settings" "%FS%Full screen: {No=N|Yes=Y}" "Apply=A" "Cancel=B2" < ';
  Base+='"Back=B"';
//  if VMU.Speed=-1 then
//    Base:='"@SETTINGS" "Sound settings=S" %s "Video settings=V" %s "Back=B"'
//  else
//    Base:='"@SETTINGS" %s "Sound settings=S" %s "Video settings=V" %s "Back=B"';
//  Speed:='"%GS%Game speed: {Still=0|Slowest=1|Slow=2|Normal=3|Fast=4|Fastest=5}" ';
//  Sound:='> "@Sound settings" '+
//         '"%MV%Music volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}" '+
//         '"%SV%Effects volume: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}" '+
//         '"Back=B1" <';
//  Video:='> "@Video settings" "%FS%Full screen: {No=N|Yes=Y}" "Apply=A" "Cancel=B2" <';
  fMenu.Clear;
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
//  if VMU.Speed=-1 then
//    fMenu.AddItemExt(Format(Base,[Sound,Video]))
//  else begin
//    fMenu.AddItemExt(Format(Base,[Speed,Sound,Video]));
//    fMenu.SetValue('GS',inttostr(VMU.Speed));
//  end;
  fMenu.AddItemExt(Base);
  if VMU.FullScreen then fMenu.SetValue('FS','Y') else fMenu.SetValue('FS','N');
  fMenu.SetValue('MV',inttostr(trunc(VMU.MusicVolume*10)));
  fMenu.SetValue('SV',inttostr(trunc(VMU.SoundVolume*10)));
end;

{function TOptions.fGetFullScreenChanged:boolean;
begin
  Result:=fFullScreenChanged;
  fFullScreenChanged:=false;
end;}

end.
