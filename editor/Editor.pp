// LittleTrain Editor (C) 2007 MKSZTSZ  *** Absolutely Freeware! ***

{$smartlink on}

uses crt,
     objects,
     windows,
     sysutils, // FPC RTL units...
     
     logger2,  // If you want to use the Log object
     contmem,  // For some visual components.
     sdl,      // SimpleDirectmediaLayer
     mk_sdl,   // Our wrapper for SDL
     mkfonts,  // MultiFont outtext and text justification constants
     sprites,  // Sprite object (PSprite)
     SpriteEngineUnit,  // Sprite Engine (PSpriteEngine)
     BMPFont,  // PBMPFont
     GSDFont,  // PGSDFont
     MKRFont,  // PMKRFont
     ImageCollection,  // PImageCollection
//     WaveCollection,   // PWaveCollection
//     MusicCollection,  // PMusicCollection
     mkwincrt,   // Our toolbox (various functions to make the life easier)
//     mkinifile,  // INI file handling (PINIFile)
     madread,    // MAD file reading (PMAD)
     madwrite,   // MAD file writing (PMADWrite)
//     players,    // Player registry (PPlayers)
//     onlinehighscore,  // To access the internet (OHS)
//     rankings,   // I don't really remember... :-(
//     code1732,   // To code the lowest 17 bit of a longint into 8 hexdigits (and back)
     xmap,       // Tile map object (PXMAP)
     xmappack,   // MapPack object (PXMapPack)
     vcc_button, // Visual Component Library - Button
     md5;        // To create MD5 hash of a file or a string

{!Build} const Build='0093';
         const Version='0.00';
         const FullName='LittleTrain Editor';
         const ShortName='Editor';

var
    MAD:PMAD;
    MapPack:PXMapPack;
    IC:PImageCollection;
    SysFont:PMKRFont;

const BackR=64;BackG=32;BackB=128;

const ssno:word=0;

procedure ScreenShot;
var p:pchar;
begin
  p:=strAlloc(16);
  StrPCopy(p,ShortName+'_'+st(ssno,4,'0')+'.bmp');
  SDL_SaveBMP(PrimarySurface,p);
  StrDispose(p);
  inc(ssno);
end;

{$I mapedit.pii}
{$I mapselect.pii}
{$I blocks.pii}
{$I main.pii}

var i:integer;

begin
  randomize;
  Log.LogAppHead(FullName+' V'+Version+' Build '+Build,'');
  SDLInit(800,600,32,false,true);
  SDL_WM_SetCaption(FullName+' V'+Version+' Build '+Build+' (C) 2007 MKSZTSZ', nil);
//  SDL_ShowCursor(SDL_Disable);
//  Init_Audio;
  SetFPS(60);

  ClearScreen(0,0,0);
  new(MAD,Init('LittleTrain.mad',false));
  new(SysFont,Init('normal.mkr',MAD));
  SysFont^.SetColorKey(0,0,0);
  ControlMemory^.Store(0,SysFont,4);
  new(IC,Init);
  IC^.AddByList('editgfx.lst',MAD);
  for i:=0 to IC^.ItemCount-1 do
    IC^.Add(ShrinkImage4(IC^.Find(i)),IC^.GetName(i)+'/4');

  new(Main,Init);
  Main^.Run;
  dispose(Main,Done);

  dispose(IC,Done);
  dispose(SysFont,Done);
  dispose(MAD,Done);
  SDLDone;
end.

