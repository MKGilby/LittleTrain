unit LTRN_LogoUnit;

{$mode Delphi}

interface

uses mk_sdl2;

type

{ TLogo }

  TLogo=class
    constructor Create(iLeft,iTop:integer);
    procedure Draw(fOffset:integer=0);
  private
    fLeft,fTop:integer;
    fTexture:TTexture;
  end;

implementation

uses LTRN_SharedUnit;

{ TLogo }

constructor TLogo.Create(iLeft,iTop:integer);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  fTexture:=MM.Textures.ItemByName['Logo'];
end;

procedure TLogo.Draw(fOffset:integer);
begin
  PutTexture(fLeft,fTop+fOffset,fTexture);
end;

end.

