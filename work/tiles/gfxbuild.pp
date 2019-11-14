uses objects,mkwincrt,crt;

{!Build} const Build='0002';
         const Version='1.10';

{$I r_tga.ii}
{$I w_tga.ii}

var Xs:PStream;
    s:String;

const Dir:string='New';

procedure AddGFX(fn,name:string);
var p:pointer;wi,he:integer;
begin
  write('  File: '+fn+'.tga...');
  getmem(p,32768);
  ReadTGA(dir+'/'+fn+'.tga',wi,he,p^);
  if (he<>32) or (wi mod 32<>0) then begin
    writeln('Invalid image dimensions! ('+st(wi)+'x'+st(he)+')'#13#10'*** Height must be 32, Width must be dividable by 32!');
    halt;
  end;
  if wi div 32=1 then begin
    WriteTGA('..\data\'+fn+'.tga',wi,he,p^,16384);
  end else begin
    if (upper(fn)='DOOR') or (name='c1') or (name='c2') then
      WriteTGA('..\data\'+fn+'.tga',wi,he,p^,wi div 32)
    else
      WriteTGA('..\data\'+fn+'.tga',wi,he,p^,wi div 32+12288);
  end;
  fn+='.tga';
  fn+=','+name+#13#10;
  Xs^.Write(fn[1],length(fn));
  freemem(p,32768);
  writeln('OK ');
end;

var i:integer;

begin
  writeln('Little Train GFX Pack Builder V'+Version+'.'+Build+' - (C) 2006 MKSZTSZ'#13#10);
  if paramcount=1 then dir:=paramstr(1);
  writeln('Processing folder: '+dir);
  Xs:=new(PBufStream,Init('..\data\gfx.lst',stCreate,4096));
  AddGFX('wall','#');
  AddGFX('door','!');
  AddGFX('crash1','c1');
  AddGFX('crash2','c2');
  AddGFX('engine','%');
  s:='engine.tga:H,%L'#13#10;
  Xs^.Write(s[1],length(s));
  s:='engine.tga:HR,%U'#13#10;
  Xs^.Write(s[1],length(s));
  s:='engine.tga:Hr,%D'#13#10;
  Xs^.Write(s[1],length(s));
  for i:=0 to 17 do begin
    AddGFX('g'+st(i+1,2,'0'),chr(97+i));
    AddGFX('t'+st(i+1,2,'0'),chr(65+i));
    s:='t'+st(i+1,2,'0')+'.tga:H,'+chr(65+i)+'L'#13#10;
    Xs^.Write(s[1],length(s));
    s:='t'+st(i+1,2,'0')+'.tga:HR,'+chr(65+i)+'U'#13#10;
    Xs^.Write(s[1],length(s));
    s:='t'+st(i+1,2,'0')+'.tga:Hr,'+chr(65+i)+'D'#13#10;
    Xs^.Write(s[1],length(s));
  end;
  write('Finalizing...');
  s:='!END'#13#10;
  Xs^.Write(s[1],length(s));
  dispose(Xs,Done);
  writeln('Done.');
  readkey;
end.
