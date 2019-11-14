program mapconv;

{$mode delphi}{$H+}

uses
  sysutils, Classes, XMapPackUnit, XMapUnit, MKToolbox, Logger;

var
  MP:TXMapPack;
  Target:TMemoryStream;

function EncodeSolution(sol:string):string;
var b:byte;i:integer;
begin
  Result:='';
  b:=0;
  while length(sol) mod 4<>0 do sol+='U';
  for i:=0 to length(sol)-1 do begin
    b:=b<<2;
    case sol[i+1] of
      'U':b:=b+0;
      'R':b:=b+1;
      'D':b:=b+2;
      'L':b:=b+3;
    end;
    if i mod 4=3 then begin
      Result+=chr(b);
      Log.Trace(hexstr(b,2));
      b:=0;
    end;
  end;
end;

function ProcessMap(mapno:integer):TMemoryStream;
const Head:string='LT';
var Map:TXMap;i,j,prepos:integer;b:byte;s:string;

  procedure ScanForItems(Target:TStream;value:integer;savecount:boolean);
  var cp,cnt,i:integer;
  begin
    cp:=Target.Position;
    if savecount then Target.Write(cp,1);
    cnt:=0;
    for i:=0 to 239 do
      if Map.OrigTiles[i mod 20,i div 20]=value then begin
        inc(cnt);
        Target.Write(i,1);
      end;
    if savecount then begin
      if (cnt=0) and (prepos=0) then prepos:=cp;
      if cnt<>0 then prepos:=0;
      Target.Position:=cp;
      Target.Write(cnt,1);
      Target.Position:=Target.Size;
    end;
  end;

begin
  prepos:=0;
  Result:=TMemoryStream.Create;
  Result.Write(Head[1],2);
  Map:=MP.GetMap(st(mapno,2,'0'));
//  Map.LogContent;
  // 1. Walls
  if mapno<51 then b:=0 else b:=7;
  Result.Write(b,1);
  b:=length(Map.Extras[0]);
  Result.Write(b,1);
  for i:=0 to 29 do begin
    b:=0;
    for j:=0 to 7 do begin
      b:=(b<<1) and $ff;
      if Map.OrigTiles[(i*8+j) mod 20,(i*8+j) div 20]=35 then b:=b or 1;
    end;
    Result.Write(b,1);
  end;
  ScanForItems(Result,37,false);
  ScanForItems(Result,33,false);
  for i:=97 to 122 do ScanForItems(Result,i,true);
  if prepos<>0 then begin
    Result.Position:=prepos;
    b:=255;
    Result.Write(b,1);
    Result.Size:=Result.position;
  end;
  if mapno=51 then begin
    s:=EncodeSolution(Map.Extras[0]);
    Result.Write(s[1],length(s));
  end;
  FreeAndNil(Map);
end;

var
  Xs:TMemoryStream;
  i:integer;

begin
  MP:=TXMapPack.Create;
  MP.Load('ltrain.mpk');
  Target:=TMemoryStream.Create;
  for i:=1 to 51 do begin
    writeln(i,#13);
    Xs:=ProcessMap(i);
    Xs.Position:=0;
    Target.CopyFrom(Xs,Xs.Size);
//    Xs.SaveToFile(st(i,2,'0')+'.binmap');
    FreeAndNil(Xs);
  end;
  Target.SaveToFile('maps.bin');
  FreeAndNil(Target);
  FreeAndNil(MP);
end.

{
  Start Len  Description
    0    2    Signature: 'LT'
    2    1    Flags.
                bit0 - Solution exists (There's a stream of directions exists in
                       the level stream, which can be used to autoplay the level.)
                bit1 - Self playing map (Game will control the engine moving it
                       according to the solution stored with the map. If no
                       solution exists (see previous bit), the engine will stay
                       still at the start position.)
                bit2 - Congratulations map (game shows a hard-coded message on
                       the lower part of the screen when this flag is set.)
    3    1    Par Score
    4   30    Wall data (30*8=240, 12*20=240)
                1 bit - 1 block, if bit is set, there's wall
                Order is first byte highest bit -> last byte lowest bit
                (going right then down on the map)
   34    1    Engine start position (0 means upper-left, 19 means upper-right,
              239 means lower right corner)
   35    1    Door position

   36    <max. 26 times>
         1    Count of nth block type in the map (x)
         x    Map positions of the blocks (0..239)
   when x=255 then no more blocks are present in the map file.

   ?   (([3]-1) div 4)+1  Solution of the map, if the bit0 of the flag byte is set.
   1 byte contains 4 moves in bit 7-6, 5-4, 3-2, 1-0 pairs. If a bit pair is
      00 - move up
      01 - move right
      10 - move down
      11 - move left
}

