{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit PSCursorUnit;

interface

uses mk_sdl2;

type

{ TPSCursor }

 TPSCursor=class
       constructor Create(iPosition,iDelay:integer);
       destructor Destroy; override;
       procedure StartOut;
       procedure Draw;
       procedure MoveTo(iPosition:integer);
       procedure Restart;
       procedure InstantIn;
     private
       fDelay:integer;
       fSaveDelay:integer;
       fImage:TTexture;
       fPosition:integer;
       fTPosition:integer;
       fSpeed:integer;
       fInOutState:(ioHidden,ioComingInDelay,ioComingIn,ioVisible,ioGoingOutDelay,ioGoingOut);
       fFase:integer;
       fLight,fLight2:integer;
       procedure fSetPosition(pValue:integer);
     public
       property Position:integer read fPosition write fSetPosition;
     end;
     
implementation

uses SysUtils, ARGBImageUnit;

const CursorSteps:integer=64;  // How many steps to fly the cursor in and out
      CursorMul:integer=10; // 640 div CursorSteps;

constructor TPSCursor.Create(iPosition,iDelay:integer);
const r=30;g=162;b=240;
var i,v,d:integer;tmp:TARGBImage;
begin
  fDelay:=iDelay+1;
  fSaveDelay:=fDelay;
  fPosition:=iPosition;
  fTPosition:=fPosition;
  fInOutState:=ioComingInDelay;
  tmp:=TARGBImage.Create(512+640,2);
  v:=0;d:=1;
  for i:=0 to 512+640-1 do begin
    tmp.bar(i,0,1,2,(r*v) div 256,(g*v) div 256,(b*v) div 256);
    v+=d;
    if (v=0) or (v=255) then d:=-d;
  end;
  fImage:=TStaticTexture.Create(tmp);
  FreeAndNil(tmp);

{  fImage:=TImage.Create(512+640,2);
  v:=0;d:=1;
  for i:=0 to 512+640-1 do begin
    fImage.bar(i,0,1,2,r*v>>8,g*v>>8,b*v>>8);
    v+=d;
    if (v=0) or (v=255) then d:=-d;
  end;}
  
  fLight:=0;
  fLight2:=512;
  fFase:=0;
  fSpeed:=0;
end;

destructor TPSCursor.Destroy;
begin
  if Assigned(fImage) then FreeAndNIL(fImage);
  inherited ;
end;

procedure TPSCursor.MoveTo(iPosition:integer);
begin
  fTPosition:=iPosition;
end;

procedure TPSCursor.StartOut;
begin
  fDelay:=fSaveDelay;
  fInOutState:=ioGoingOutDelay;
end;

procedure TPSCursor.Draw;
begin
  if fDelay>0 then begin
    dec(fDelay);
    if fDelay=0 then begin
      case fInOutState of
        ioComingInDelay:fInOutState:=ioComingIn;
        ioGoingOutDelay:fInOutState:=ioGoingOut;
      end;
      fFase:=0;
    end;
  end;
  inc(fLight,2);
  if fLight=512 then fLight:=0;
  dec(fLight2,2);
  if fLight2=0 then fLight2:=512;

  if fTPosition<fPosition then begin
    if (fPosition-fTPosition>(1<<(fSpeed+1))) and (fSpeed<3) then inc(fSpeed);
    if (fPosition-fTPosition<(1<<fSpeed)) and (fSpeed>0) then dec(fSpeed);
    dec(fPosition,1<<fSpeed);
  end;
  if fTPosition>fPosition then begin
    if (fTPosition-fPosition>(1<<(fSpeed+1))) and (fSpeed<3) then inc(fSpeed);
    if (fTPosition-fPosition<(1<<fSpeed)) and (fSpeed>0) then dec(fSpeed);
    inc(fPosition,1<<fSpeed);
  end;

  case fInOutState of
    ioComingIn:begin
      inc(fFase);

      PutTexturePart(0,fPosition   ,640-fFase*CursorMul+fLight,0,fFase*CursorMul,2,fImage);
      PutTexturePart(0,fPosition+28,640-fFase*CursorMul+fLight2,0,fFase*CursorMul,2,fImage);
      bar(0,fPosition+2,fFase*CursorMul,26,5,27,40);
      if fFase=CursorSteps then fInOutState:=ioVisible;
    end;
    ioVisible:begin
      PutTexturePart(0,fPosition   ,fLight,0,640,2,fImage);
      PutTexturePart(0,fPosition+28,fLight2,0,640,2,fImage);
      Bar(0,fPosition+2,640,26,5,27,40);
    end;
    ioGoingOut:begin
      PutTexturePart(fFase*CursorMul,fPosition   ,fLight,0,640-fFase*CursorMul,2,fImage);
      PutTexturePart(fFase*CursorMul,fPosition+28,fLight2,0,640-fFase*CursorMul,2,fImage);
      Bar(fFase*CursorMul,fPosition+2,640-fFase*CursorMul,26,5,27,40);
      inc(fFase);
      if fFase=CursorSteps then fInOutState:=ioHidden;
    end;
  end;
end;

procedure TPSCursor.Restart;
begin
  fDelay:=fSaveDelay;
  fInOutState:=ioComingInDelay;
  fFase:=0
end;

procedure TPSCursor.InstantIn;
begin
  fInOutState:=ioVisible;
end;

procedure TPSCursor.fSetPosition(pValue:integer);
begin
  fPosition:=pValue;
  fTPosition:=pvalue;
end;

end.
