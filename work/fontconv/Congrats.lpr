program Congrats;

{$mode delphi}{$H+}

const
  Chars='CONGRATULIS!';

var f:file;i,j:integer;

begin
  assignfile(f,'congafnt.bmf');
  rewrite(f,1);
  for i:=0 to 11 do begin
    blockwrite(f,chars[i+1],1);
    j:=i*12;
    blockwrite(f,j,2);
    j:=0;
    blockwrite(f,j,2);
    j:=i*12+11;
    blockwrite(f,j,2);
    j:=20;
    blockwrite(f,j,2);
  end;
  closefile(f);
end.

