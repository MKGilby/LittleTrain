unit LTRN_SlotSelectUnit;

{$mode Delphi}

interface

uses PSLineUnit;

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
    procedure Run(pPreselected:integer=1);
  private
    Lines:array[0..7] of TPSLine;
  end;

implementation

uses
  SysUtils, LTRN_VMUUnit, LTRN_SharedUnit, Font2Unit, mk_sdl2, SDL2,
  ARGBImageUnit, PSCursorUnit;

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
  fProgressPercent:=VMU.GetCompletedMapCount(fVMUSlotNumber)*100 div (MapList.Count-1);
  fScore:=0;
  for i:=0 to MapList.Count-2 do
    if VMU.GetMapState(fVMUSlotNumber,i)>0 then
      fScore:=fScore+MapList[i].ParScore-VMU.GetMapState(fVMUSlotNumber,i);
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
var i:integer;
begin
  Lines[0]:=TPSLine.Create(#2'Select save slot',96,0);
  for i:=1 to 5 do
    Lines[i]:=TSlotVisual.Create(i,TOP+HEIGHT*i,INTERVAL*i);
  Lines[6]:=TPSLine.Create(#1'Arrows'#0' - Move, '#1'Space'#0' - Select',352,Interval*6);
  Lines[7]:=TPSLine.Create(#1'Del'#0' - Reset, '#1'Esc'#0' - Quit',384,Interval*7);
end;

destructor TSlotSelector.Destroy;
var i:integer;
begin
  for i:=0 to 7 do
    if Assigned(Lines[i]) then FreeAndNil(Lines[i]);
  inherited Destroy;
end;

procedure TSlotSelector.Run(pPreselected: integer);
var cnt,i:integer;
  Cursor:TPSCursor;
begin
  if (pPreselected<1) or (pPreselected>5) then pPreselected:=1;
  SDL_RenderClear(PrimaryWindow.Renderer);
  PutTexture(149,3,MM.Textures.ItemByName['Logo']);
  Cursor:=TPSCursor.Create(pPreselected*HEIGHT+TOP-4,0);
//  bar(0,SLOTSTOP,PrimaryWindow.Width,SLOTHEIGHT*5,0,0,0);
  ClearKeys;
  repeat
    Scroll.Move(2);
    bar(0,TOP-4,PrimaryWindow.Width,PrimaryWindow.Height-TOP,0,0,0);
    Cursor.Draw;
    for i:=0 to 7 do
      Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
    HandleMessages;
    if keys[SDL_SCANCODE_UP] and (pPreselected>1) then begin
      keys[SDL_SCANCODE_UP]:=false;
      dec(pPreselected);
      Cursor.MoveTo(pPreselected*HEIGHT+TOP-4);
      MM.Waves['MenuMoveTick']._wave.Play;
    end;
    if keys[SDL_SCANCODE_DOWN] and (pPreselected<5) then begin
      keys[SDL_SCANCODE_DOWN]:=false;
      inc(pPreselected);
      Cursor.MoveTo(pPreselected*HEIGHT+TOP-4);
      MM.Waves['MenuMoveTick']._wave.Play;
    end;
  until keys[SDL_SCANCODE_ESCAPE];
  MM.Waves['MenuSelect']._wave.Play;
  Cursor.StartOut;
  for i:=0 to 7 do Lines[i].StartOut;
  cnt:=0;
  repeat
    Scroll.Move(1);
    bar(0,64,639,405,0,0,0);
    bar(0,452,639,471,0,0,0);
    inc(cnt);
    Cursor.Draw;
    for i:=0 to 7 do
      Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
  until cnt=Interval*8+LineOneStepTime;
  FreeAndNil(Cursor);
end;

end.

