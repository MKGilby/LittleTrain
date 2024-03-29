unit LTRN_SlotSelectUnit;

{$mode Delphi}

interface

uses PSLineUnit, PSCursorUnit, mk_sdl2;

type

  { TSlotVisual }

  TSlotVisual=class(TPSLine)
    constructor Create(iVMUSlot,iPosition,iDelay:integer);
  private
    fVMUSlotNumber:integer;
    fProgressPercent:integer;
    fScore:integer;
    fSelected:boolean;
  public
    property Selected:boolean write fSelected;
  end;

  { TSlotSelector }

  TSlotSelector=class
    constructor Create;
    destructor Destroy; override;
    function Run(pPreselected:integer=0):integer;
  private
    Lines:array[0..7] of TPSLine;
    Cursor:TPSCursor;
    function SelectSlot(pPreselected:integer):integer;
    procedure ResetSlot(iSlot:integer);
  end;

implementation

uses
  SysUtils, LTRN_VMUUnit, LTRN_SharedUnit, Font2Unit, LTRN_OptionsUnit, SDL2,
  ARGBImageUnit;

const
  INTERVAL=10;
  TOP=128;
  HEIGHT=32;

{ TSlotVisual }

constructor TSlotVisual.Create(iVMUSlot,iPosition,iDelay:integer);
var i:integer;atmT:TARGBImage;
begin
  fVMUSlotNumber:=iVMUSlot;
  i:=(MapList.Count-1);
  VMU.SelectSlot(iVMUSlot);
  fProgressPercent:=VMU.GetCompletedMapCount*100 div (MapList.Count-1);
  fScore:=0;
  for i:=0 to MapList.Count-2 do
    if VMU.GetMapState(i)>0 then
      fScore:=fScore+MapList[i].ParScore-VMU.GetMapState(i);
  VMU.SelectSlot(-1);
  atmT:=TARGBImage.Create(640,22);
  atmT.bar(0,0,atmT.Width,atmt.Height,0,0,0,0);
  MM.Fonts.OutText(atmt,Format(#3'%d.  '#6'Progress: '#1'%d%%',[fVMUSlotNumber,fProgressPercent]),64,0,mjLeft);
  MM.Fonts.OutText(atmt,Format(#7'Score: '#0'%d',[fScore]),400,0,mjLeft);
  CreateImages2(atmT);
  FreeAndNil(atmT);
  fPosition:=iPosition;
  fDelay:=iDelay+1;
  fDelaySave:=iDelay+1;
  fFase:=0;
end;

{ TSlotSelector }

constructor TSlotSelector.Create;
begin
  Cursor:=TPSCursor.Create(1*HEIGHT+TOP-4,0);
end;

destructor TSlotSelector.Destroy;
var i:integer;
begin
//  if Assigned(Logo) then FreeAndNil(Logo);
  if Assigned(Cursor) then FreeAndNil(Cursor);
  for i:=0 to 7 do
    if Assigned(Lines[i]) then FreeAndNil(Lines[i]);
  inherited Destroy;
end;

function TSlotSelector.Run(pPreselected: integer):integer;
var i:integer;
begin
  if (pPreselected<0) or (pPreselected>4) then pPreselected:=1;
  i:=pPreselected;
  repeat
    if (i and 8)<>0 then i:=i xor 8;
    i:=SelectSlot(i);
    if i>8 then ResetSlot(i-8);
  until (i=-1) or (i=-2) or (i in [0..4]);
  Result:=i;
end;

function TSlotSelector.SelectSlot(pPreselected: integer):integer;
var cnt,i:integer;
begin
  Lines[0]:=TPSLine.Create(#2'Select save slot',96,0);
  for i:=1 to 5 do
    Lines[i]:=TSlotVisual.Create(i-1,TOP+HEIGHT*i,INTERVAL*i);
  Lines[6]:=TPSLine.Create(#1'Arrows'#0' - Move, '#1'Space'#0' - Select',352,Interval*6);
//  Lines[7]:=TPSLine.Create(#1'Del'#0' - Reset, '#1'Esc'#0' - Quit, '#1'F12'#0' - Options',384,Interval*7);
  Lines[7]:=TPSLine.Create(#1'Del'#0' - Reset, '#1'F12'#0' - Settings, '#1'Esc'#0' - Quit',384,Interval*7);
  SDL_RenderClear(PrimaryWindow.Renderer);
  PutTexture(149,3,MM.Textures.ItemByName['Logo']);
  if ReturnTo=rNone then
    Cursor.Restart
  else begin
    Cursor.InstantIn;
    for i:=0 to 7 do Lines[i].InstantIn;
    ReturnTo:=rNone;
  end;
  Cursor.Position:=(pPreselected+1)*HEIGHT+TOP-4;
  //  bar(0,SLOTSTOP,PrimaryWindow.Width,SLOTHEIGHT*5,0,0,0);
  ClearKeys;
  repeat
    Scroll.Move2(1);
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    Logo.Draw;
    Cursor.Draw;
    for i:=0 to 7 do
      Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
    HandleMessages;
    if keys[SDL_SCANCODE_UP] and (pPreselected>0) then begin
      keys[SDL_SCANCODE_UP]:=false;
      dec(pPreselected);
      Cursor.MoveTo((pPreselected+1)*HEIGHT+TOP-4);
      MM.Waves['MenuMoveTick']._wave.Play;
    end;
    if keys[SDL_SCANCODE_DOWN] and (pPreselected<4) then begin
      keys[SDL_SCANCODE_DOWN]:=false;
      inc(pPreselected);
      Cursor.MoveTo((pPreselected+1)*HEIGHT+TOP-4);
      MM.Waves['MenuMoveTick']._wave.Play;
    end;
    if keys[OPTIONSKEY] then begin
      Options.Run(SOUND_SETTINGS or VIDEO_SETTINGS);
//      Options.Run(SOUND_SETTINGS);
      ClearKeys;
    end;
  until keys[SDL_SCANCODE_ESCAPE] or keys[SDL_SCANCODE_DELETE]
        or keys[SDL_SCANCODE_SPACE] or keys[SDL_SCANCODE_RETURN] or
        Options.FullScreenChanged;
  Result:=pPreselected;
  if keys[SDL_SCANCODE_DELETE] then Result+=8;
  if keys[SDL_SCANCODE_ESCAPE] then Result:=-1;
  if Options.FullScreenChanged then begin
    ReturnTo:=rSlotSelector;
    ReturnData[0]:=pPreselected;
    Result:=-2;
  end else begin
    MM.Waves['MenuSelect']._wave.Play;
    Cursor.StartOut;
    for i:=0 to 7 do Lines[i].StartOut;
    cnt:=0;
    repeat
      Scroll.Move(1);
      SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
      SDL_RenderClear(PrimaryWindow.Renderer);
      if (cnt<48) or not keys[SDL_SCANCODE_ESCAPE] then
        Logo.Draw
      else
        Logo.Draw(48-cnt);
      inc(cnt);
      Cursor.Draw;
      for i:=0 to 7 do
        Lines[i].Draw;
      if (cnt<64) or not keys[SDL_SCANCODE_ESCAPE] then
        Scroll.Draw(0)
      else
        Scroll.Draw(cnt-64);
      Flip;
      HandleMessages;
    until cnt=Interval*8+LineOneStepTime;
  end;
  for i:=0 to 7 do
    if Assigned(Lines[i]) then FreeAndNil(Lines[i]);
end;

procedure TSlotSelector.ResetSlot(iSlot:integer);
var
  cnt:integer;
  i,act:integer;
begin
  Lines[0]:=TPSLine.Create(#2'Really delete progress',96,0);
  Lines[1]:=TPSLine.Create(#1+'from slot '+inttostr(iSlot+1)+'?',128,Interval);
  Lines[2]:=TPSLine.Create(#3+'No, just kidding!',192,Interval*2);
  Lines[3]:=TPSLine.Create(#3+'Yes, blow it away!',224,Interval*3);
  Lines[4]:=TPSLine.Create(#1'Arrows'#0' - Move, '#1'Space'#0' - Select',352,Interval*4);
  act:=0;
  Cursor.Restart;
  Cursor.Position:=188;
  cnt:=1;
  repeat
    Scroll.Move(1);
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    Logo.Draw;
    inc(cnt);
    Cursor.Draw;
    for i:=0 to 4 do Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
    HandleMessages;
    if keys[SDL_SCANCODE_UP] and (act>0) then begin
      keys[SDL_SCANCODE_UP]:=false;
      dec(act);
      Cursor.MoveTo(act*32+188);
      MM.Waves['MenuMoveTick']._wave.Play;
    end;
    if keys[SDL_SCANCODE_DOWN] and (act<1) then begin
      keys[SDL_SCANCODE_DOWN]:=false;
      inc(act);
      Cursor.MoveTo(act*32+188);
      MM.Waves['MenuMoveTick']._wave.Play;
    end;
    if keys[OPTIONSKEY] then begin
      Options.Run(SOUND_SETTINGS or VIDEO_SETTINGS);
      ClearKeys;
    end;
  until (keys[SDL_SCANCODE_ESCAPE] or keys[SDL_SCANCODE_SPACE] or keys[SDL_SCANCODE_RETURN]) and (cnt>Interval*5+64);
  MM.Waves['MenuSelect']._wave.Play;
  Cursor.StartOut;
  for i:=0 to 4 do Lines[i].StartOut;
  cnt:=0;
  repeat
    Scroll.Move(1);
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    Logo.Draw;
    inc(cnt);
    Cursor.Draw;
    for i:=0 to 4 do Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
    HandleMessages;
  until cnt=Interval*5+LineOneStepTime;
  if (act=1) and not keys[SDL_SCANCODE_ESCAPE] then begin
//    VMU.RenamePlayer(iSlot,'???'+inttostr(iSlot));
    VMU.SelectSlot(iSlot);
    VMU.ClearSlot;
    VMU.SelectSlot(-1);
  end;
  for i:=0 to 4 do
    if Assigned(Lines[i]) then FreeAndNil(Lines[i]);
end;

end.

