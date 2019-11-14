{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit LTRN_PlayerSelectUnit;

interface

uses PlayerRegistryUnit;

type TPlayerSelect=class
       function Run:integer;
     private
       fActPlayer:integer;
       fActName:string;
       procedure DeletePlayer(slot:integer);
       function SelectPlayer(def:integer):integer;
       function AddNewPlayer(slot:integer):boolean;
     end;

var PlayerSelect:TPlayerSelect;

implementation

uses SysUtils, SDL, MK_SDL, MKFonts, MKToolBox, Logger,
     vcc_Editbox, WaveCollectionUnit, MusicCollectionUnit, PSLineUnit,
     PSCursorUnit, LTRN_OptionsUnit, FontCollectionUnit,
     LTRN_ScrollUnit, LTRN_VMUUnit, LTRN_MapImagesUnit, LTRN_SharedUnit;

const Interval=10;

function TPlayerSelect.Run:integer;
begin
  ClearScreen(0,0,0);
  PutImage(140,0,'logo.tga');
//  CheckPlayers;
  repeat
    fActPlayer:=SelectPlayer(fActPlayer);
    if fActPlayer=-1 then break;
    if fActPlayer>4 then begin
      fActPlayer-=5;
      DeletePlayer(fActPlayer);
      continue;
    end;
//    Log.Trace(VMU.GetName(fActPlayer+1));
    if copy(alltrim(VMU.GetName(fActPlayer+1)),1,3)='???' then begin
      if not AddNewPlayer(fActPlayer) then continue;
    end;
    break;
  until false;
  Result:=fActPlayer;
end;

{procedure TPlayerSelect.CheckPlayers;
var i:integer;
begin
//  fVMU.AddLevelPack('LittleTrain.mad');
//  for i:=1 to 5-fVMU.PlayerCount do fVMU.AddPlayer('???'+inttostr(i));
end;}

function TPlayerSelect.SelectPlayer(def:integer):integer;
var act:integer;
    Lines:array[0..7] of TPSLine;
    Cursor:TPSCursor;
    cnt:integer;
    i:integer;
begin
  act:=def;
  Lines[0]:=TPSLine.Create(#2'Select player',96,0);
  Lines[6]:=TPSLine.Create(#1'Arrows'#0' - Move, '#1'Space'#0' - Select',352,Interval*6);
  Lines[7]:=TPSLine.Create(#1'Del'#0' - Delete, '#1'Esc'#0' - Quit',384,Interval*7);
  for i:=1 to 5 do
    if copy(VMU.GetName(i),1,3)='???' then begin
      Lines[i]:=TPSLine.Create(#3'< New Player >',i*32+128,Interval*i);
    end else begin
      Lines[i]:=TPSLine.Create(#3+alltrim(VMU.GetName(i)),i*32+128,Interval*i);
    end;
  Cursor:=TPSCursor.Create(act*32+156,0);
  cnt:=1;
  repeat
    if cnt mod 3=0 then MapImages.CreateNextMapImage;
    Scroll.Move(1);
    bar(0,64,639,405,0,0,0);
    bar(0,452,639,471,0,0,0);
    Cursor.Draw;
    for i:=0 to 7 do
      Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
    HandleMessages;
    if keys[OptionsKey] then Options.Run;
    if keys[SDLK_Up] and (act>0) then begin
      keys[SDLK_Up]:=false;
      dec(act);
      Cursor.MoveTo(act*32+156);
      WC['MenuMoveTick']._wave.Play;
    end;
    if keys[SDLK_Down] and (act<4) then begin
      keys[SDLK_Down]:=false;
      inc(act);
      Cursor.MoveTo(act*32+156);
      WC['MenuMoveTick']._wave.Play;
    end;
    inc(cnt);
  until (keys[SDLK_Escape] or keys[SDLK_Space] or keys[SDLK_Return] or keys[SDLK_Delete]) and (cnt>Interval*8+LineOneStepTime);
  WC['MenuSelect']._wave.Play;
  Cursor.StartOut;
  for i:=0 to 7 do Lines[i].StartOut;
  cnt:=0;
  repeat
    if cnt mod 3=0 then MapImages.CreateNextMapImage;
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
  SelectPlayer:=act;
  if keys[SDLK_Escape] then SelectPlayer:=-1;
  if keys[SDLK_Delete] then SelectPlayer:=act+5;
  FreeAndNil(Cursor);
  for i:=0 to 7 do FreeAndNil(Lines[i]);
end;

// Get Player name
function TPlayerSelect.AddNewPlayer(slot:integer):boolean; // True-entered something, false-escaped
var EditBox:TEditBox;
    Lines:array[0..2] of TPSLine;
    Cursor:TPSCursor;
    cnt:integer;
    i:integer;
begin
  Lines[0]:=TPSLine.Create(#2'Enter new player name',96,0);
  Lines[1]:=TPSLine.Create('',160,Interval);
  Lines[2]:=TPSLine.Create(#1'ENTER'#0' - OK, '#1'Esc'#0' - Cancel',352,Interval*2);
  EditBox:=TEditBox.Create;
  EditBox.SetupCommand('X=320|Y=156|Width=640|Height=26|AlignX=Center|AlignY=Center');
  EditBox.SetupCommand('Font=3|TextAlign=Center|TextOffsetY=2');
  EditBox.SetupCommand('TextCase=normal|MaxLength=32|Background=800000|Transparent=Yes');
  ClearKeyBuffer;
  ClearKeys;
  Cursor:=TPSCursor.Create(142,0);
  repeat
    HandleMessages;
    EditBox.Move;
    Scroll.Move(1);
    bar(0,64,639,405,0,0,0);
    bar(0,452,639,471,0,0,0);
    Cursor.Draw;
    EditBox.Draw;
    for i:=0 to 2 do
      Lines[i].Draw;
    cnt:=0;
    for i:=1 to 5 do
      if EditBox.Text=VMU.GetName(i) then begin
        FC.FontByIndex[5].OutText('Name already exists!',320,320,mjCenter);
        cnt:=1;
      end;
    Scroll.Draw(0);
    Flip;
  until (keys[SDLK_Return] and (length(EditBox.Text)>0) and (cnt=0)) or keys[SDLK_Escape];
  WC['MenuSelect']._wave.Play;
  Cursor.StartOut;
  FreeAndNil(Lines[1]);
  Lines[1]:=TPSLine.Create(#3+EditBox.Text,147,Interval);
  for i:=0 to 2 do Lines[i].StartOut;
  cnt:=0;
  repeat
    Scroll.Move(1);
    bar(0,64,639,405,0,0,0);
    bar(0,452,639,471,0,0,0);
    inc(cnt);
    Cursor.Draw;
    for i:=0 to 2 do
      Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
  until cnt=Interval*8+LineOneStepTime;
  FreeAndNil(Cursor);
  for i:=0 to 2 do FreeAndNil(Lines[i]);
  if keys[SDLK_Return] then begin
    fActName:=EditBox.Text;
    VMU.RenamePlayer(slot,fActName);
    Result:=true;
  end else Result:=false;
  FreeAndNil(EditBox);
end;

procedure TPlayerSelect.DeletePlayer(slot:integer);
var Lines:array[0..4] of TPSLine;
    Cursor:TPSCursor;
    cnt:integer;
    i,act:integer;
begin
  Lines[0]:=TPSLine.Create(#2'Really delete player?',96,0);
  Lines[1]:=TPSLine.Create(#1+alltrim(VMU.GetName(slot+1)),128,Interval);
  Lines[2]:=TPSLine.Create(#3+'No, just kidding!',192,Interval*2);
  Lines[3]:=TPSLine.Create(#3+'Yes, blow it away!',224,Interval*3);
  Lines[4]:=TPSLine.Create(#1'Arrows'#0' - Move, '#1'Space'#0' - Select',352,Interval*4);
  act:=0;
  Cursor:=TPSCursor.Create(190,0);
  cnt:=1;
  repeat
    Scroll.Move(1);
    bar(0,64,639,405,0,0,0);
    bar(0,452,639,471,0,0,0);
    inc(cnt);
    Cursor.Draw;
    for i:=0 to 4 do Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
    HandleMessages;
    if keys[SDLK_Up] and (act>0) then begin
      keys[SDLK_Up]:=false;
      dec(act);
      Cursor.MoveTo(act*32+188);
      WC['MenuMoveTick']._wave.Play;
    end;
    if keys[SDLK_Down] and (act<1) then begin
      keys[SDLK_Down]:=false;
      inc(act);
      Cursor.MoveTo(act*32+188);
      WC['MenuMoveTick']._wave.Play;
    end;
  until (keys[SDLK_Escape] or keys[SDLK_Space] or keys[SDLK_Return]) and (cnt>Interval*5+64);
  WC['MenuSelect']._wave.Play;
  Cursor.StartOut;
  for i:=0 to 4 do Lines[i].StartOut;
  cnt:=0;
  repeat
    Scroll.Move(1);
    bar(0,64,639,405,0,0,0);
    bar(0,452,639,471,0,0,0);
//    if (cnt mod Interval=0) and (cnt div Interval<5) then Lines[cnt div Interval]^.Start;
    inc(cnt);
    Cursor.Draw;
    for i:=0 to 4 do Lines[i].Draw;
    Scroll.Draw(0);
    Flip;
  until cnt=Interval*5+LineOneStepTime;
  if (act=1) and not keys[SDLK_Escape] then begin
    VMU.RenamePlayer(slot,'???'+inttostr(slot));
    VMU.ClearData('???'+inttostr(slot),'LittleTrain.mkvd');
  end;
  FreeAndNil(Cursor);
  for i:=0 to 4 do FreeAndNil(Lines[i]);
end;


{
function TPlayerReg.NetError:boolean;
begin
  bar(0,420,639,447,50,10,20);
  bar(0,448,639,475,10,20,50);
  MultiFontOutText(#4+upper(OHS^.GetLastError),320,424,mjCenter);
  MultiFontOutText(#4'A - ABORT / R - RETRY',320,452,mjCenter);
  Flip;
  repeat
    SDL_Delay(TimeLeft);
    HandleMessages;
  until keys[SDLK_A] or keys[SDLK_R];
  if keys[SDLK_A] then begin
    gOnline:=2;
    NetError:=true;
  end;
  if keys[SDLK_R] then NetError:=false;
  keys[SDLK_R]:=false;
  keys[SDLK_A]:=false;
  bar(0,420,639,475,0,0,0);
end;
}

{i TPlayerReg.SyncToOnline.pii}

end.
