// MKSZTSZ SDL Wrapper 
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 2.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2021
// ------------------------------------------------------------------

// Version info:
//   V1.00 - 2021.01.30 - Gilby
//     * Initial creation from MKMouse
//   V1.01 - 2022.10.15 - Gilby
//     * Replaced Lists.TGenericList with fgl.TFPGObjectList

{$ifdef fpc}
  {$mode delphi}
{$endif}

unit MKMouse2;

interface

uses
  Classes, SDL2, StackUnit, fgl;

type
  TMouseEvent=function(Sender:TObject;x,y,buttons:integer):boolean of object;
  TKeyEvent=function(Sender:TObject;key:integer):boolean of object;

  TMouseObject=class
    constructor Create;
    procedure SetBounds(x1,y1,x2,y2:integer);
    procedure SetBoundsWH(x,y,width,height:integer);
    function HandleEvent(Event:PSDL_Event):boolean; virtual;
    procedure Draw; virtual; abstract;
    function IsOver(x,y:integer):boolean;
  public
    OnMouseDown:TMouseEvent;
    OnMouseUp:TMouseEvent;
    OnMouseMove:TMouseEvent;
    OnMouseEnter:TMouseEvent;
    OnMouseLeave:TMouseEvent;
    OnClick:TMouseEvent;
    OnKeyDown:TKeyEvent;
    OnKeyUp:TKeyEvent;
  protected
    fLeft,fTop,fWidth,fHeight{,fRight,fBottom}:integer;
    over:boolean;
    keyhandled:boolean;
    fName:string;
    fSelected, fClicked, fVisible:boolean;
  private
    fTag:integer;
    procedure fSetWidth(value:integer);
    procedure fSetHeight(value:integer);
  public
    property Clicked:boolean read fClicked;
    // Not all object will use this, but needed for radiogroup.
    property Selected:boolean read fSelected write fSelected;
    property Name:string read fName write fName;
    property Left:integer read fLeft write fLeft;
    property Top:integer read fTop write fTop;
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
    property Tag:integer read fTag write fTag;
    property Visible:boolean read fVisible write fVisible;
  end;

  { *** You must add your mouse objects to this to handle events... *** }
  TMouseObjects=class(TFPGObjectList<TMouseObject>)
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Delete(index:integer);
    procedure Remove(Item:pointer);
    function HandleEvent(Event:PSDL_Event):boolean;
    procedure NewSession;
    procedure EndSession;
    procedure List;
  private
    fStack:TStack;
    fTop:integer;
    fSoftDelete:boolean;
  end;

var
  MouseObjects : TMouseObjects;

implementation

uses SysUtils, Logger, MK_SDL2;

const 
  Fstr={$I %FILE%}+', ';
  Version='1.01';

procedure Initialize;
begin
  MouseObjects:=TMouseObjects.Create;
  RegisterEventHandler(MouseObjects.HandleEvent);
end;

procedure Finalize;
begin
  FreeAndNil(MouseObjects);
end;

constructor TMouseObjects.Create;
begin
  inherited ;
  fStack:=TStack.Create;
  fTop:=0;
  fSoftDelete:=false;
end;

destructor TMouseObjects.Destroy;
begin
  FreeAndNil(fStack);
  inherited ;
end;

procedure TMouseObjects.Delete(index:integer);
begin
  if fSoftDelete then begin
//    Log.Trace('SoftDelete('+inttostr(index)+')');
    Items[index]:=nil;
  end else begin
//    Log.Trace('HardDelete('+inttostr(index)+')');
    inherited ;
  end;
end;

procedure TMouseObjects.Remove(Item:pointer);
begin
//  Log.Trace(longint(item));
//  Log.Trace(IndexOf(item));
  if IndexOf(Item)>-1 then Delete(IndexOf(Item));
end;

function TMouseObjects.HandleEvent(Event:PSDL_Event):boolean;
var i:integer;
begin
  fSoftDelete:=true;
  Result:=false;
{  case Event^.Type_ of
    SDL_MOUSEBUTTONDOWN:Log.Trace('MouseDown');
    SDL_MOUSEBUTTONUP:Log.Trace('MouseUp');
    SDL_MOUSEMOTION:Log.Trace('MouseMotion');
    SDL_KEYDOWN:Log.Trace('KeyDown');
    SDL_KEYUP:Log.Trace('KeyUp');
  end;}
  if Count>0 then begin
//    Log.Trace('s.HandleEvent');
    i:=Count-1;
//    i:=fTop;
//    Log.Trace(Format('i=%d, fTop=%d',[i,fTop]));
    while (i>=fTop) and (i<Count) and not(Result) do begin
      Log.Trace('Passing event to object number '+inttostr(i)+' ('+Self[i].Name+')');
      if Self[i]<>nil then
        Result:=Self[i].HandleEvent(Event);
      dec(i);
    end;
    if Result then begin
      if Self[i]<>nil then
        Log.Trace('Event handled by object number '+inttostr(i+1)+' ('+Self[i+1].Name+')')
      else
        Log.Trace('Event handled by an object that invalidated itself.');
    end else Log.Trace('Event not handled by any objects.');
  end;
  fSoftDelete:=false;
  for i:=Count-1 downto fTop do
    if Self[i]=nil then Delete(i);
//  Log.Trace('s.HandleEvent end');
end;

procedure TMouseObjects.Draw;
var i:integer;
begin
  for i:=fTop to Count-1 do Self[i].Draw;
end;

procedure TMouseObjects.NewSession;
begin
  fStack.Push2(fTop);
  fTop:=Count;
end;

procedure TMouseObjects.EndSession;
var i:integer;
begin
  for i:=Count-1 downto fTop do Delete(i);
  fTop:=fStack.Pop2;
end;

procedure TMouseObjects.List;
const Istr=Fstr+'TMouseObjects.List';
var i:integer;
begin
  Log.LogDebug(Format('Mouse objects listing starts... (fTop=%d, Count=%d)',[fTop,Count]),Istr);
  for i:=fTop to Count-1 do begin
    if Self[i]<>nil then
      Log.LogDebug(inttostr(i)+'. '+Self[i].fName,Istr)
    else
      Log.LogDebug(inttostr(i)+'. <nil>',Istr);
  end;
  Log.LogDebug('Mouse objects listing ends.',Istr);
end;

constructor TMouseObject.Create;
begin
  fLeft:=-1; // To show that object coordinates are not set.
  over:=false;
  fVisible:=true;
end;

procedure TMouseObject.SetBounds(x1,y1,x2,y2:integer);
var i:integer;
begin
  if x1>x2 then begin
    i:=x1;x1:=x2;x2:=i;
  end;
  if y1>y2 then begin
    i:=y1;y1:=y2;y2:=i;
  end;
  fLeft:=x1;
  fTop:=y1;
//  fRight:=x2;
//  fBottom:=y2;
  fWidth:=x2-x1+1;
  fHeight:=y2-y1+1;
end;

procedure TMouseObject.SetBoundsWH(x,y,width,height:integer);
begin
  if Width<0 then Width:=32;
  if Height<0 then Height:=32;
  fLeft:=x;
  fTop:=y;
  fWidth:=width;
  fHeight:=height;
//  fRight:=x+width-1;
//  fBottom:=y+height-1;
end;

function TMouseObject.IsOver(x,y:integer):boolean;
begin
  Result:=(x>=fLeft) and (x<fLeft+fWidth) and (y>=fTop) and (y<fTop+fHeight);
end;

function TMouseObject.HandleEvent(Event:PSDL_Event):boolean;
var nx,ny:integer;
begin
  Result:=false;
  if not fVisible then exit;
  with Event^.Button do begin
    nx:=x;
    ny:=y;
  end;
  case Event^.Type_ of
    SDL_MOUSEBUTTONDOWN:with Event^.Button do begin
      if IsOver(nx,ny) and Assigned(OnMouseDown) then begin
        Result:=OnMouseDown(Self,nx,ny,Button);
      end;
    end;
    SDL_MOUSEBUTTONUP:with Event^.Button do begin
      if IsOver(nx,ny) then begin
        if Assigned(OnMouseUp) then Result:=OnMouseUp(Self,nx,ny,Button);
        if Assigned(OnClick) then
          if OnClick(Self,nx,ny,Button) then Result:=true;
      end;
    end;
    SDL_MOUSEMOTION:with Event^.Button do begin
      if IsOver(nx,ny) then begin
        if Assigned(OnMouseMove) then Result:=OnMouseMove(Self,nx,ny,Button);
        if not over then begin
          Result:=false;
          if Assigned(OnMouseEnter) then OnMouseEnter(Self,nx,ny,Button);
          over:=true;
        end;
      end else
        if over then begin
          Result:=false;
          if Assigned(OnMouseLeave) then OnMouseLeave(Self,nx,ny,Button);
          over:=false;
        end;
    end;
    SDL_KEYDOWN:begin
      if Assigned(OnKeyDown) then Result:=OnKeyDown(Self,Event.Key.keysym.sym);
    end;
    SDL_KEYUP:begin
      if Assigned(OnKeyUp) then Result:=OnKeyUp(Self,Event.Key.keysym.sym);
    end;
  end;
end;

procedure TMouseObject.fSetWidth(value:integer);
begin
  if value<0 then value:=32;
  if fLeft+value>=PrimaryWindow.Window.w then value:=PrimaryWindow.Window.w-fLeft;
  fWidth:=value;
end;

procedure TMouseObject.fSetHeight(value:integer);
begin
  if value<0 then value:=32;
  if fTop+value>=PrimaryWindow.Window.h then value:=PrimaryWindow.Window.h-fTop;
  fHeight:=value;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  Initialize;

finalization
  Finalize;

end.

