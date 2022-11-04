{$mode delphi}
{$smartlink on}

unit LTRN_MapSelectorUnit;

interface

uses LTRN_MapBaseUnit, PSLineUnit;

type
  TMapSelector=class
    constructor Create(iTop,iVMUSlot:integer);
    destructor Destroy; override;
    function Run:integer;
  private
    fTop:integer;
    fVMUSlot:integer;
    fMaps:array[0..50] of TMapBase;
    fTotalScore:integer;
    trgt:integer;
    slev:integer;
    function ScanTotalScore:integer;
  end;

var MapSelector:TMapSelector;

implementation

uses
  SysUtils, MKToolBox, Logger, BASS, sdl2, mk_sdl2,
  LTRN_SharedUnit, LTRN_OptionsUnit, LTRN_VMUUnit,
  LTRN_MapPlayUnit, LTRN_MapCongratsUnit;

constructor TMapSelector.Create(iTop,iVMUSlot:integer);
var i,j:integer;
begin
  fTop:=iTop;
  fVMUSlot:=iVMUSlot;

  trgt:=-1;
  j:=0;
//  LogTicks.StartMeasuring;
  for i:=0 to 49 do begin
    fMaps[i]:=TMapPlay.Create(236+i*176,fTop,i,fVMUSlot);
    if fMaps[i].BestScore>0 then fMaps[i].SetState(2)
                            else begin
      if trgt=-1 then begin
        trgt:=i*176;
        slev:=i;
      end;
      if j<3 then fMaps[i].SetState(1) else fMaps[i].SetState(0);
      inc(j);
    end;
//    LogTicks.LogTicks(inttostr(i));
  end;
  fMaps[50]:=TMapCongrats.Create(236+50*176,fTop,50,fVMUSlot);
//  fMaps[50].SetState(1);
  if trgt=-1 then begin trgt:=50*176;slev:=50;end;
  if j=0 then fMaps[50].SetState(1);
end;

destructor TMapSelector.Destroy;
var i:integer;
begin
  for i:=0 to 50 do if Assigned(fMaps[i]) then FreeAndNil(fMaps[i]);
  inherited ;
end;

function TMapSelector.ScanTotalScore:integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to 49 do
    if fMaps[i].BestScore<>0 then
      Result:=Result+fMaps[i].ParScore-fMaps[i].BestScore;
end;

function TMapSelector.Run:integer;
var mode,fase,i,j,posi,spd:integer;
    Lines:array[0..5] of TPSLine;
const ad2:array[0..7] of integer=(0,1,2,4,8,16,32,64);

  procedure ChangeLines;
  var s:string;
  begin
    if not fMaps[slev].AutoPlay then begin
      Lines[2].ChangeText(#7'Map: '#6+st(slev+1,2,'0'));
      if fMaps[slev].State=2 then s:=inttostr(fMaps[slev].ParScore) else s:='???';
      Lines[3].ChangeText(#7'Par score: '#6+s);
      Lines[4].ChangeText(#7'Your score: '#6+inttostr(fMaps[slev].BestScore));
      if fMaps[slev].State=2 then begin
        s:=#8' ('+inttostr(fMaps[slev].ParScore-fMaps[slev].BestScore)+')';
        if not (s[4] in ['-','0']) then insert('+',s,4);
      end else s:='';
      Lines[5].ChangeText(#7'Total score: '#6+inttostr(fTotalScore)+s);
    end else begin
      Lines[2].ChangeText(#3'Congratulations!');
      Lines[3].ChangeText(' ');
      Lines[4].ChangeText(' ');
      Lines[5].ChangeText(#7'Total score: '#6+inttostr(fTotalScore));
    end;
  end;

begin
  mode:=0;
  fase:=0;
//  slev:=0;
  posi:=0;
//  trgt:=0;
  spd:=0;
  fTotalScore:=ScanTotalScore;
  Lines[0]:=TPSLine.Create(#0'Welcome '#1+VMU.GetName(fVMUSlot)+#0'!',80,0);
  Lines[1]:=TPSLine.Create(#2'Select map to play!',112,10);
  Lines[2]:=TPSLine.Create('x',304,40);
  Lines[3]:=TPSLine.Create('x',336,50);
  Lines[4]:=TPSLine.Create('x',368,60);
  Lines[5]:=TPSLine.Create('x',400,70);
  ChangeLines;

  repeat
    Scroll.Move(1);
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    Logo.Draw;
    for i:=0 to 5 do Lines[i].Draw;

    case mode of
      0:begin
          for i:=0 to 50 do
            fMaps[i].Draw(fase*4);
          inc(fase);
          if fase=64 then mode:=1;
        end;
      1,4,5:begin
          for i:=0 to 50 do
            fMaps[i].Draw;
        end;
      2:begin
          dec(fase);
          if fase>=0 then
            for i:=0 to 50 do
              fMaps[i].Draw(fase*4);
          if fase=-64 then mode:=3;
        end;
    end;
    Scroll.Draw(0);
    Curtain.Draw;
{$ifdef DEBUG}
    if keys[SDL_SCANCODE_F11] then begin
      SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
      SDL_RenderClear(PrimaryWindow.Renderer);
      MM.Fonts.DemoFonts;
    end;
{$endif}
    Flip;
    HandleMessages;
    case mode of
      1:begin
          if keys[SDL_SCANCODE_ESCAPE] then begin
            mode:=2;
            for i:=0 to 5 do Lines[i].StartOut;
            BASS_ChannelSlideAttribute(MM.Musics['Menu']._music.Handle, BASS_ATTRIB_VOL,0, 2000);
          end;
          if keys[OptionsKey] then Options.Run;
          if keys[SDL_SCANCODE_RIGHT] and (slev<50) then begin
            inc(trgt,176);
            inc(slev);
            ChangeLines;
            keys[SDL_SCANCODE_RIGHT]:=false;
          end;
          if keys[SDL_SCANCODE_LEFT] and (slev>0) then begin
            dec(trgt,176);
            dec(slev);
            ChangeLines;
            keys[SDL_SCANCODE_LEFT]:=false;
          end;
          if (abs(posi-trgt)>ad2[spd+1]) and (spd<6) then inc(spd);
          if (abs(posi-trgt)<ad2[spd+1]) and (spd>0) then dec(spd);
          if posi<trgt then begin
            for i:=0 to 50 do fMaps[i].MoveRelX(-ad2[spd]);
            posi+=ad2[spd];
          end;
          if posi>trgt then begin
            for i:=0 to 50 do fMaps[i].MoveRelX(+ad2[spd]);
            posi-=ad2[spd];
          end;
          if (keys[SDL_SCANCODE_SPACE] or keys[SDL_SCANCODE_RETURN]) and (fMaps[slev].State>0) then begin
            Mode:=4;
            Curtain.StartClose;
            BASS_ChannelSlideAttribute(MM.Musics['Menu']._music.Handle, BASS_ATTRIB_VOL,0, 300);
          end;
        end;
      4:if Curtain.State=3 then begin
          MM.Musics['Menu']._music.Stop;
          MM.Musics['Menu']._music.Volume:=VMU.MusicVolume;
          if fMaps[slev].Congratulations then
            MM.Musics['Ending']._music.Play
          else
            MM.Musics['Ingame']._music.Play;
          repeat
            i:=fMaps[slev].Play;
          until i in [0,1];
          ClearKeys;
          if fMaps[slev].Congratulations then begin
            MM.Musics['Ending']._music.Stop;
            MM.Musics['Ending']._music.Volume:=VMU.MusicVolume;
          end else begin
            MM.Musics['Ingame']._music.Stop;
            MM.Musics['Ingame']._music.Volume:=VMU.MusicVolume;
          end;
          MM.Musics['Menu']._music.Play;
          if i=0 then begin
            j:=0;
            for i:=0 to 49 do begin
              if fMaps[i].BestScore>0 then fMaps[i].SetState(2)
                                      else begin
                if j<3 then fMaps[i].SetState(1) else fMaps[i].SetState(0);
                inc(j);
              end;
            end;
            if j=0 then fMaps[50].SetState(1);
            i:=ScanTotalScore;
            if i<>fTotalScore then begin
              fTotalScore:=i;
            end;
            ChangeLines;
          end;
          Curtain.StartOpen;
          mode:=5;
        end;
      5:if Curtain.State=3 then mode:=1;
    end;
  until mode=3;
  Result:=0;
  for i:=0 to 5 do FreeAndNil(Lines[i]);
end;

end.
