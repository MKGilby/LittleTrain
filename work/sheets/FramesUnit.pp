unit FramesUnit;

{$mode delphi}

interface

uses
  RawPictureUnit, Lists;

type
  TFrameData=class
    destructor Destroy; override;
  public
    _used:boolean;
    _image:TRawPicture;
    _rotatedoffset,
    _notrotatedoffset:integer;  // Place in ingame sheet, -1 unknown yet
    _sheetoffset:integer;
    _fileindex:integer;   // Sheet number
    _usedinrotated,_usedinnotrotated:boolean;
  end;

  TFrames=class(TGenericList<TFrameData>)
    function AddFrame(pFrame:TRawPicture;pRotated:boolean):integer;
    procedure ResetUsage;
    function UnusedCount:integer;
  private
    function CompareFrames(pFrame1,pFrame2:TRawPicture):boolean;
  end;

implementation

uses SysUtils;

destructor TFrameData.Destroy;
begin
  FreeAndNil(_image);
  inherited ;
end;

function TFrames.AddFrame(pFrame:TRawPicture;pRotated:boolean):integer;
var atm:TFrameData;i:integer;
begin
  for i:=0 to Count-1 do
    if CompareFrames(Self[i]._image,pFrame) then begin
      Result:=i;
      if pRotated then
        Self[i]._usedinrotated:=true
      else
        Self[i]._usedinnotrotated:=true;
      FreeAndNil(pFrame);    // Not needed, already having it.
      exit;
    end;
  Result:=Count;
  atm:=TFrameData.Create;
  atm._image:=pFrame;
  atm._rotatedoffset:=-1;
  atm._notrotatedoffset:=-1;
  if pRotated then
    atm._usedinrotated:=true
  else
    atm._usedinnotrotated:=true;
  Add(atm);
end;

function TFrames.CompareFrames(pFrame1,pFrame2:TRawPicture):boolean;
var p1,p2:pointer;i:integer;
begin
  Result:=false;
  if (pFrame1.Width<>pFrame2.Width) or (pFrame1.Height<>pFrame2.Height) then exit;
  p1:=pFrame1.Rawdata;
  p2:=pFrame2.Rawdata;
  i:=pFrame1.Width*pFrame1.Height*3;
  while (i>0) and (byte(p1^)=byte(p2^)) do begin
    inc(p1);
    inc(p2);
    dec(i);
  end;
  Result:=(i=0);
end;

procedure TFrames.ResetUsage;
var i:Integer;
begin
  for i:=0 to Count-1 do Self[i]._used:=false;
end;

function TFrames.UnusedCount:integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if not Self[i]._used then Result+=1;
end;

end.

