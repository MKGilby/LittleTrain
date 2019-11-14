uses mkwincrt;

var f:file;

procedure WriteHead;
var s:String;
begin
  s:='KEYS';
  blockwrite(f,s[1],length(s));
end;

procedure WriteAliases;
var s:String;
begin
  s:=#4'U'#17#1'D'#18#1'R'#19#1'L'#20#1;
  blockwrite(f,s[1],length(s));
end;

procedure WriteIndexes;
var i:integer;w:word;
begin
  i:=50;
  blockwrite(f,i,2);
  w:=0;
  for i:=1 to 50 do begin
    blockwrite(f,w,2);
    w+=sizeoffile(st(i,2,'0')+'.sol');
  end;
end;

procedure WriteData;
var i:integer;g:file;l:longint;
    buf:array[0..1023] of byte;
begin
  for i:=1 to 50 do begin
    assign(g,st(i,2,'0')+'.sol');
    reset(g,1);
    l:=filesize(g);
    blockread(g,buf[0],l);
    blockwrite(f,buf[0],l);
    close(g);
  end;
end;

begin
  assign(f,'solution.ksp');
  rewrite(f,1);
  WriteHead;
  WriteAliases;
  WriteIndexes;
  WriteData;
  close(f);
end.

{

  Format:
  ["KEYS"][b][alias1]..[aliasn][w][index1]...[indexn][data]
     4     1    3    ..   3     2    2          2      ?
     
  ["KEYS"]              - FourCC
  [b]                   - How many aliases are there
  [alias1]..[aliasn]    - Aliases: 1 byte alias, 1 word SDLK key value
  [w]                   - How many streams are there
  [index1]...[indexn]   - Stream indexes show each stream's start in data
  [data]                - Stream of aliases
  

}
