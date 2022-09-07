{ Scrolling Text class for FreePascal and SDL2 }
{ Written by Gilby            (C) 2021 MKSZTSZ }

{ Version info:
    V1.00 - 2021.09.02 - Gilby:
       - Initial creation for SDL2 based on ScrollUnit
    V1.01 - 2021.09.17 - Gilby:
       - Allow scrolling backwards by passing a negative parameter to Move
}

unit Scroll2Unit;

{$mode delphi}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}

interface

uses Classes, Font2Unit;

type
  TScroll=class
    constructor Create(iFont:TFont;iLeft,iTop,iWidth,iHeight,iOffs:integer);
    procedure LoadText(aFilename:string); overload;
    procedure LoadText(aStream:TStream); overload;
    procedure Move(pixels:integer=1);
    procedure Draw(Offset:integer);
  private
    fFont:TFont;
    fLeft,fTop,fWidth,fHeight,fOffs:integer;
    fText:string;
    fLeadIn:integer;
    // Text position in fText string
    fTextPos:integer;
    // Pixel position in current first letter
    fPosInLetter:integer;
    // Current first letter width
    fCurrentLetterWidth:integer;
    procedure fSetText(pValue:string);
  public
    property Text:string read fText write fSetText;
  end;

implementation

uses SDL, Logger, MKStream, MKToolBox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.01';

constructor TScroll.Create(iFont:TFont;iLeft,iTop,iWidth,iHeight,iOffs:integer);
begin
  fFont:=iFont;
  fTop:=iTop;
  fLeft:=iLeft;
  fWidth:=iWidth;
  fHeight:=iHeight;
  fOffs:=iOffs;
  fTextPos:=1;
  fPosInLetter:=0;
  fCurrentLetterWidth:=0;
  fText:='';
end;

procedure TScroll.LoadText(aStream:TStream);
var tmp:string;i:integer;
begin
  if Assigned(aStream) then begin
    SetLength(tmp,aStream.Size);
    aStream.Read(tmp[1],aStream.Size);
    fText:='';
    for i:=1 to length(tmp) do
      if not (tmp[i] in [#13,#10,#27]) then fText+=tmp[i]
      else if tmp[i]=#10 then fText+=' ';
  end;
  fLeadin:=(fWidth div fFont.SpaceSpace)+1;
  Log.Trace(fWidth);
  Log.Trace(fLeadIn);
end;

procedure TScroll.LoadText(aFilename:string);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(aFilename);
  if Assigned(Xs) then begin
    LoadText(Xs);
    FreeAndNil(Xs);
  end;
end;

procedure TScroll.Move(pixels:integer);
begin
  while Pixels>0 do begin
    inc(fPosInLetter);
    if fPosInLetter>fCurrentLetterWidth then begin
      if fLeadIn<=0 then begin
        inc(fTextPos);
        if fTextPos>length(fText) then fTextPos:=1;
        fPosInLetter:=0;
        fCurrentLetterWidth:=fFont.TextWidth(fText[fTextPos]);
        while fCurrentLetterWidth=0 do begin
          inc(fTextPos);
          if fTextPos>length(fText) then fTextPos:=1;
          fCurrentLetterWidth:=fFont.TextWidth(fText[fTextPos]);
        end;
      end else begin
        dec(fLeadIn);
        fCurrentLetterWidth:=fFont.SpaceSpace;
        fPosInLetter:=0;
      end;
    end;
    dec(Pixels);
  end;
  while Pixels<0 do begin
    fLeadIn:=0;  // drop LeadIn if moved backwards.
    dec(fPosInLetter);
    if fPosInLetter<0 then begin
      dec(fTextPos);
      if fTextPos<1 then fTextPos:=length(fText);
      fCurrentLetterWidth:=fFont.TextWidth(fText[fTextPos]);
      while fCurrentLetterWidth=0 do begin
        dec(fTextPos);
        if fTextPos<1 then fTextPos:=length(fText);
        fCurrentLetterWidth:=fFont.TextWidth(fText[fTextPos]);
      end;
      fPosInLetter:=fCurrentLetterWidth;
    end;
    inc(Pixels);
  end;
end;

procedure TScroll.Draw(Offset:integer);
var s:String;i,w:integer;
begin
  s:='';
  w:=0;
  if fLeadIn>0 then begin s:=spc(fLeadIn);w:=fLeadin*fFont.SpaceSpace;end;
  i:=fTextPos;
  while w-fPosInLetter<fWidth do begin
    s+=fText[i];
    w+=fFont.TextWidth(fText[i]);
    inc(i);
    if i>length(fText) then i:=1;
  end;
  fFont.OutText(s,fLeft-fPosInLetter,fTop+Offset+fOffs,0);
end;

procedure TScroll.fSetText(pValue:string);
begin
  fText:=pValue;
  fLeadin:=(fWidth div fFont.SpaceSpace)+1;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
