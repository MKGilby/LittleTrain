{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit LTRN_CurtainUnit;

interface

uses ImageUnit;

type TCurtain=class
       constructor Create(iImage:TImage);
       procedure StartClose;
       procedure StartOpen;
       procedure Draw;
     public
       State:integer;
     private
       fImage:TImage;
       fFase,fDirection:integer;
     end;


implementation

uses MK_SDL;

constructor TCurtain.Create(iImage:TImage);
begin
  fImage:=iImage;
  fFase:=-1;
  State:=0;
end;

procedure TCurtain.StartClose;
begin
  fDirection:=1;
  fFase:=0;
  State:=1;
end;

procedure TCurtain.StartOpen;
begin
  fDirection:=-1;
  fFase:=29;
  State:=2;
end;

procedure TCurtain.Draw;
var i,j:integer;
begin
  if State=3 then State:=0;  // Ez azért kell, hogy 1 ciklusig 3-assal jelzi,
                             // hogy éppen akkor fejezte be.
  if fFase>-1 then begin
    for j:=0 to fFase>>1 do
      for i:=0 to 19 do
        PutImage(i<<5,j<<5,fImage);
    fFase+=fDirection;
    if fFase=30 then begin
      fFase:=-1;
      State:=3;
    end else
      if fFase=-1 then State:=3;
  end;
end;

end.
