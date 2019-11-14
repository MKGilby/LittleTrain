{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit PSCursorUnit;

interface

uses MK_SDL, ImageUnit;

type TPSCursor=class
       constructor Create(iPosition,iDelay:integer);
       destructor Destroy; override;
       procedure StartOut;
       procedure Draw;
       procedure MoveTo(iPosition:integer);
       function GetPosition:integer;
       procedure Restart;
     private
       fDelay:integer;
       fSaveDelay:integer;
       fImage:TImage;
       fPosition:integer;
       fTPosition:integer;
       fSpeed:integer;
       fFase:integer;
       fLight,fLight2:integer;
     end;
     
implementation

uses SysUtils;

const CursorSteps:integer=64;
      CursorMul:integer=10; // 640 div CursorSteps;

constructor TPSCursor.Create(iPosition,iDelay:integer);
const r=30;g=162;b=240;
var i,v,d:integer;
begin
  fDelay:=iDelay+1;
  fSaveDelay:=fDelay;
  fPosition:=iPosition;
  fTPosition:=fPosition;
  fImage:=TImage.Create(512+640,2);
  v:=0;d:=1;
  for i:=0 to 512+640-1 do begin
    fImage.bar(i,0,1,2,r*v>>8,g*v>>8,b*v>>8);
    v+=d;
    if (v=0) or (v=255) then d:=-d;
  end;
  
  fLight:=0;
  fLight2:=512;
  fFase:=0;
  fSpeed:=0;
end;

destructor TPSCursor.Destroy;
begin
  FreeAndNIL(fImage);
  inherited ;
end;

procedure TPSCursor.MoveTo(iPosition:integer);
begin
  fTPosition:=iPosition;
end;

procedure TPSCursor.StartOut;
begin
  fDelay:=fSaveDelay;
  fFase:=CursorSteps+1
end;

procedure TPSCursor.Draw;
begin
  if fDelay>0 then begin
    dec(fDelay);
    if fDelay=0 then inc(fFase);
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

  if (fFase>0) and (fFase<=CursorSteps) then begin
    PutImagePart(0,fPosition   ,640-fFase*CursorMul+fLight,0,639+fLight,1,fImage);
    PutImagePart(0,fPosition+28,640-fFase*CursorMul+fLight2,0,639+fLight2,1,fImage);
    bar(0,fPosition+2,fFase*CursorMul-1,fPosition+27,5,27,40);
    inc(fFase);
  end;
  if (fFase=CursorSteps+1) then begin
    PutImagePart(0,fPosition   ,fLight,0,fLight+639,1,fImage);
    PutImagePart(0,fPosition+28,fLight2,0,fLight2+639,1,fImage);
    bar(0,fPosition+2,639,fPosition+27,5,27,40);
  end;
  if (fFase>CursorSteps+1) and (fFase<=CursorSteps<<1+1) then begin
    PutImagePart((fFase-CursorSteps-1)*CursorMul,fPosition   ,fLight,0,fLight+639-(fFase-CursorSteps-1)*CursorMul,1,fImage);
    PutImagePart((fFase-CursorSteps-1)*CursorMul,fPosition+28,fLight2,0,fLight2+639-(fFase-CursorSteps-1)*CursorMul,1,fImage);
    bar((fFase-CursorSteps-1)*CursorMul,fPosition+2,639,fPosition+27,5,27,40);
    inc(fFase);
  end;
end;

function TPSCursor.GetPosition:integer;
begin
  Result:=fPosition;
end;

procedure TPSCursor.Restart;
begin
  fDelay:=fSaveDelay;
  fFase:=0
end;

end.
