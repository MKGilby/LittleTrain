(* -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                                               Menu

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2021

  --------------------------------------------------

  -[Description]------------------------------------

   Simple menu with the possibility of options.
   To set up use the properties or use an INI file.

   Example INI file:

   [Menu]
   ; Positioning
   Left=120
   Width=400
   Top=168
   Height=144
   ; One menu row height
   RowHeight=36
   ; Horizontal alignment and vertical offset if centering is not accurate
   TextAlign=Center
   TextOffsetY=2
   ; Full menu vertical align within the given height.
   MenuAlign=Center
   ; Font names in FC (FontCollectionUnit)
   BaseFont=BaseFont
   HighlightFont=HLFont
   HeaderFont=HeaderFont
   ; If there are options, pressing left on the first item or pressing right on
   ; the last item will jump to the last/first item.
   CycleOptions=false
   ; INI section of menu items
   Items=MenuItems

   ; Menu items, one item in ""s.
   ; You can create sub-menus using > and <.
   ; > The previous item has a sub menu, the following items will be the
   ;   sub menu's items. (until <)
   ; < One level up, the sub menu is finished.
   ; To create a simple menu line, just add it.
   ; To create an option menu line, add the following:
   ;     %label%Text {text1=value1|text2=value2...}
   ; ex. %fs%Fullscreen {on=1|off=0}
   ;     You can query the state of the option with GetValue method.
   ;     ex. Menu.GetValue('fs') will give back 1 or 0.
   ; To create a header line (not selectable) use @ at the start of
   ; the item (@Are you sure?)

   [*MenuItems]
   "CONTINUE=C"
   "NEW GAME=N"
   > "@YOU WILL LOSE ALL PROGRESS!"
     "@REALLY START OVER?"
     "NO=0"
     "YES=1" <
   "OPTIONS=O"
   > "@OPTIONS"
     "%MV%MUSIC VOLUME: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}"
     "%SV%EFFECTS VOLUME: {0%=0|10%=1|20%=2|30%=3|40%=4|50%=5|60%=6|70%=7|80%=8|90%=9|100%=10}" <
   "QUIT=Q"

  --------------------------------------------------

*)
// Version info:
//
//  V1.00: Gilby - 2021.05.28
//     - Initial creation from vcc_Menu
//  V1.01: Gilby - 2021.06.03
//     - Added MenuAlign property (whole menu vertical align within the given height)

{$mode delphi}
{$smartlink on}

unit vcc_Menu2;

interface

uses MKMouse2, Font2Unit, StackUnit, MKINIFile, FontList2Unit;

type TMenuCallback=procedure(key:byte);
// Keys: Up=1, Down=2, Left=4, Right=8, Upleft=1+4, etc...

type
  TLineData=record
    _text:string;      // Menu text
    _type:(ltNormal,ltOption,ltHeader);  // Menu type
    _submenu:boolean;  // Has submenu?
    _parent:integer;   // Parent line index (-1 = root)
    _label:string;    // Menu item label or Option label to query
    // if _type is ltOption
    _selectedoptionindex:integer;   // Index of selected option
    _optionstrings:array of string; // Option text to show
    _optionvalues:array of string;  // Option values to return when queried
  end;

  TMenuState=(msActive,msEscaped,msSelected);

  TMenu=class(TMouseObject)
    constructor Create; overload;
    constructor Create(iINI:TINIFile;iSection:string;iFonts:TFontList); overload;
    destructor Destroy; override;

    procedure Clear;
//    procedure SetupData;
    procedure Draw; override;
    procedure Move;
    function GetValue(iLabel:string):string;
    procedure SetValue(iLabel:string;iValue:string);
    procedure ExecuteSelf;
    procedure AddItem(s:String);
    procedure AddItemExt(s:String);
//    function OnMouseMove({%H-}x,y,{%H-}buttons:integer):boolean; override;
//    function OnMouseDown({%H-}x,{%H-}y,{%H-}buttons:integer):boolean; override;
  private
    fBaseFont,
    fHighlightFont,
    fHeaderFont:TFont;
    fTextAlign,
    fTextOffsetY,
    fRowHeight,
    fTextAlignPointX,
    fMenuAlign,
    fMenuTop:integer;
//    fKeys:array[0..5] of word;
//    fBackgroundColor,
//    fHighLightBackgroundColor,
//    fHeaderBackgroundColor:integer;

    fLines:array of TLineData;
    fVisibleItems:array of integer;
//    fVisibleLineCount:integer;

    fCycleOptions:boolean;
      
    fStack:TStack;
    fSelected:integer;
    fState:TMenuState;
//    fExitKey:integer;
    fStart:integer;
    function ProcessMenuItem(s:string):TLineData;
    procedure FillItems(parent:integer);
    function GetWord(var s:String):string;
    function GetSelected:integer;
    procedure SetSelected(iIndex:integer);
    function GetSelectedLabel:string;
    procedure SetSelectedLabel(iLabel:string);
    procedure fSetLeft(value:integer);
    procedure fSetWidth(value:integer);
    procedure fSetTextAlign(value:integer);
  public
    property Left:integer read fLeft write fSetLeft;
    property Top:integer read fTop write fTop;
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fHeight;
    property RowHeight:integer read fRowHeight write fRowHeight;
    property TextAlign:integer read fTextAlign write fSetTextAlign;
    property TextOffsetY:integer read fTextOffsetY write fTextOffsetY;
    property MenuAlign:integer read fMenuAlign write fMenuAlign;
    property BaseFont:TFont read fBaseFont write fBaseFont;
    property HighlightFont:TFont read fHighlightFont write fHighlightFont;
    property HeaderFont:TFont read fHeaderFont write fHeaderFont;
//    property BackgroundColor:integer read fBackgroundColor write fBackgroundColor;
//    property HighlightBackgroundColor:integer read fHighLightBackgroundColor write fHighLightBackgroundColor;
//    property HeaderBackgroundColor:integer read fHeaderBackgroundColor write fHeaderBackgroundColor;
    property CycleOptions:boolean read fCycleOptions write fCycleOptions;

    property Selected:integer read GetSelected write SetSelected;
    property SelectedLabel:string read GetSelectedLabel write SetSelectedLabel;
    property State:TMenuState read fState write fState;
//    property Finished:boolean read fFinished write fFinished;
//    property ExitKey:integer read fExitKey;
  end;

var Menu:TMenu;

implementation

uses Classes, SysUtils, SDL2, MK_SDL2, MKToolBox, Logger;

const Fstr='vcc_Menu2.pas, ';
      Version='1.01';

constructor TMenu.Create;
begin
  inherited Create;
  Clear;

  fLeft:=0;
  fTop:=0;
  fWidth:=64;
  fHeight:=64;
  fTextAlign:=mjLeft;
  fTextAlignPointX:=fLeft;
  fTextOffsetY:=0;
  fRowHeight:=16;
  fBaseFont:=nil;
  fHighlightFont:=nil;
  fHeaderFont:=nil;
  fMenuAlign:=1;
//  fBackgroundColor:=-1;
//  fHeaderBackgroundColor:=-1;
//  fHighLightBackgroundColor:=-1;
  fState:=msActive;
  fCycleOptions:=false;

  fStack:=TStack.Create;
end;

constructor TMenu.Create(iINI:TINIFile;iSection:string;iFonts:TFontList);
var s,s2:string;i:integer;
begin
  Create;

  Left:=iINI.ReadInteger(iSection,'Left',0);
  Top:=iINI.ReadInteger(iSection,'Top',0);
  Width:=iINI.ReadInteger(iSection,'Width',64);
  RowHeight:=iINI.ReadInteger(iSection,'RowHeight',16);
  Height:=iINI.ReadInteger(iSection,'Height',64);
  TextAlign:=strtoint(decode(iINI.ReadString(iSection,'TextAlign','Left'),'Center,1,Right,2,0'));
  TextOffsetY:=iINI.ReadInteger(iSection,'TextOffsetY',0);
  MenuAlign:=strtoint(decode(iINI.ReadString(iSection,'MenuAlign','Top'),'Center,1,Bottom,2,0'));
  fMenuTop:=Top;
  CycleOptions:=iINI.ReadBool(iSection,'CycleOptions',false);
  s:=iINI.ReadString(iSection,'BaseFont','not defined');
  BaseFont:=iFonts[s];
  if BaseFont=nil then raise Exception.Create(Format('BaseFont not found! (%s)',[s]));
  s:=iINI.ReadString(iSection,'HighlightFont','not defined');
  HighlightFont:=iFonts[s];
  if HighlightFont=nil then raise Exception.Create(Format('HighlightFont not found! (%s)',[s]));
  s:=iINI.ReadString(iSection,'HeaderFont','not defined');
  HeaderFont:=iFonts[s];
  if HeaderFont=nil then raise Exception.Create(Format('HeaderFont not found! (%s)',[s]));

//  BackgroundColor:=iINI.ReadInteger(iSection,'BackgroundColor',0);
//  HighlightBackgroundColor:=iINI.ReadInteger(iSection,'HighlighBackgroundColor',0);
//  HeaderBackgroundColor:=iINI.ReadInteger(iSection,'HeaderBackgroundColor',0);

  s:=iINI.ReadString(iSection,'Items','not defined');
  if iINI.GetSectionType(s)<>stList then raise Exception.Create(Format('Missing or invalid section! (%s)',[s]));
  s2:='';
  for i:=0 to iINI.ListItemCount(s)-1 do s2+=iINI.ReadListItem(s,i)+' ';
  AddItemExt(s2);
end;

destructor TMenu.Destroy;
begin
  FreeAndNil(fStack);
  Clear;
  inherited ;
end;

procedure TMenu.fSetLeft(value:integer);
begin
  fLeft:=value;
  case fTextAlign of
    mjLeft:fTextAlignPointX:=fLeft;
    mjCenter:fTextAlignPointX:=fLeft+fWidth div 2;
    mjRight:fTextAlignPointX:=fLeft+fWidth;
  end;
end;

procedure TMenu.fSetWidth(value:integer);
begin
  fWidth:=value;
  case fTextAlign of
    mjLeft:fTextAlignPointX:=fLeft;
    mjCenter:fTextAlignPointX:=fLeft+fWidth div 2;
    mjRight:fTextAlignPointX:=fLeft+fWidth;
  end;
end;

procedure TMenu.fSetTextAlign(value:integer);
begin
  fTextAlign:=value;
  case fTextAlign of
    mjLeft:fTextAlignPointX:=fLeft;
    mjCenter:fTextAlignPointX:=fLeft+fWidth div 2;
    mjRight:fTextAlignPointX:=fLeft+fWidth;
  end;
end;

procedure TMenu.Clear;
var i:integer;
begin
//  inherited Clear;
  for i:=0 to length(fLines)-1 do begin
    SetLength(fLines[i]._optionstrings,0);
    SetLength(fLines[i]._optionvalues,0);
  end;
  SetLength(fLines,0);
  SetLength(fVisibleItems,0);
  fSelected:=-1;
//  fModified:=true;
end;

function TMenu.ProcessMenuItem(s:string):TLineData;
var s2:string;
begin
//  Log.Trace(s);
  if s[1]='@' then with Result do begin
    _type:=ltHeader;
//    _type:=ltNormal;
    _text:=copy(s,2,length(s)-1);
  end
  else if s[1]='%' then with Result do begin
    _type:=ltOption;
    delete(s,1,1);
    _label:=copy(s,1,pos('%',s)-1);
//    Log.Trace('Label: '+_label);
    delete(s,1,pos('%',s));
    _text:=copy(s,1,pos('{',s)-1);
//    Log.Trace('Text: '+_text);
    delete(s,1,pos('{',s));
    s:=copy(s,1,pos('}',s)-1)+'|';
    _SelectedOptionIndex:=0;
    SetLength(_optionstrings,0);
    SetLength(_optionvalues,0);
    repeat
      s2:=copy(s,1,pos('|',s)-1);
      delete(s,1,pos('|',s));
      SetLength(_optionstrings,length(_optionstrings)+1);
      _optionstrings[length(_optionstrings)-1]:=copy(s2,1,pos('=',s2)-1);
      delete(s2,1,pos('=',s2));
      SetLength(_optionvalues,length(_optionvalues)+1);
      _optionvalues[length(_optionvalues)-1]:=s2;
    until length(s)=0;
  end else with Result do begin
    _type:=ltNormal;
    if pos('=',s)>0 then begin
      _text:=copy(s,1,pos('=',s)-1);
      delete(s,1,length(_text)+1);
      _label:=s;
    end else begin
      _text:=s;
      _label:=s;
    end;
  end;
end;

procedure TMenu.FillItems(parent:integer);
const Istr=Fstr+'TMenu.FillItems';
var i,j,k:integer;
begin
  Log.LogDebug('Parent='+inttostr(parent),Istr);
  SetLength(fVisibleItems,0);
  fStart:=0;
  for i:=0 to length(fLines)-1 do
    if fLines[i]._parent=parent then begin
      Setlength(fVisibleItems,length(fVisibleItems)+1);
      fVisibleItems[length(fVisibleItems)-1]:=i;
      if fLines[i]._type=ltHeader then inc(fStart);
      Log.LogDebug('Item added: '+inttostr(i),Istr);
    end;
    
  if fstart>0 then   // There are header lines in the menu
    for i:=length(fVisibleItems)-2 downto 0 do  // Sort the headers to the top
      for j:=0 to i do
        if (fLines[fVisibleItems[j]]._type<>ltHeader) and
           (fLines[fVisibleItems[j+1]]._type=ltHeader) then begin
             k:=fVisibleItems[j];
             fVisibleItems[j]:=fVisibleItems[j+1];
             fVisibleItems[j+1]:=k;
           end;

  fMenuTop:=fTop+(fRowHeight-fBaseFont.Height) div 2;
  case fMenuAlign of
    1:fMenuTop+=(fHeight-length(fVisibleItems)*fRowHeight) div 2;
    2:fMenuTop+=fHeight-length(fVisibleItems)*fRowHeight;
  end;
end;

function TMenu.GetWord(var s:String):string;
var mode:(mLTrim,mRTrim,mNormal,mQuote1,mQuote2);i:integer;
begin
  Result:='';
  mode:=mLTrim;
  i:=0;
  s+=' ';
  while i<length(s) do begin
    inc(i);
    case mode of
      mLTrim:if s[i]<>' ' then begin mode:=mNormal;dec(i);end;
      mRTrim:if s[i]<>' ' then begin dec(i);break;end;
      mNormal:begin
        if s[i]='"' then mode:=mQuote2
        else if s[i]=#39 then mode:=mQuote1
        else if s[i]=' ' then mode:=mRTrim
        else Result+=s[i];  
      end;
      mQuote1:if s[i]=#39 then mode:=mNormal
                          else Result+=s[i];
      mQuote2:if s[i]='"' then mode:=mNormal
                          else Result+=s[i];
    end;
  end;
  delete(s,1,i);
end;

procedure TMenu.AddItem(s:String);
begin
  SetLength(fLines,length(fLines)+1);
  fLines[length(fLines)-1]:=ProcessMenuItem(s);
  fLines[length(fLines)-1]._submenu:=false;
  fLines[length(fLines)-1]._parent:=-1;
  FillItems(-1);
end;

procedure TMenu.AddItemExt(s:String);
var s2:string;parent:integer;
begin
  parent:=-1;
  s:=alltrim(s);
  while length(s)>0 do begin
    s2:=GetWord(s);
    if (s2='>') then begin
      fStack.Push4(parent);
      parent:=length(fLines)-1;
      if length(fLines)>0 then fLines[length(fLines)-1]._submenu:=true;
    end else
    if (s2='<') then begin
      if (fStack.Size>3) then parent:=fStack.Pop4;
    end else begin
      SetLength(fLines,length(fLines)+1);
      fLines[length(fLines)-1]:=ProcessMenuItem(s2);
      fLines[length(fLines)-1]._submenu:=false;
      fLines[length(fLines)-1]._parent:=parent;
    end;
  end;
  FillItems(-1);
end;

procedure TMenu.Draw;
var i,j:integer;font:TFont;
begin
//  if fModified then SetupData;
  j:=fMenuTop;
  for i:=0 to length(fVisibleItems)-1 do with fLines[fVisibleItems[i]] do begin
//    Log.Trace(inttostr(i)+', '+inttostr(fVisibleItems[i])+', '+inttostr(fSelected));
    if i=fSelected then begin
      font:=fHighlightFont; 
//      if fHighLightBackgroundColor>-1 then 
//        Bar(fLeft,j,fWidth,fRowHeight,fHighLightBackgroundColor);
    end else begin
      font:=fBaseFont;
//      if fBackgroundColor>-1 then 
//        Bar(fLeft,j,fWidth,fRowHeight,fBackgroundColor);
    end;   
    if _type=ltHeader then begin
      font:=fHeaderFont;
//      if fHeaderBackgroundColor>-1 then 
//        Bar(fLeft,j,fWidth,fRowHeight,fHeaderBackgroundColor);
    end;  
    if _type=ltOption then
      font.OutText(_text+_optionstrings[_selectedoptionindex],fTextAlignPointX,j+fTextOffsetY,fTextAlign)
    else
      font.OutText(_text,fTextAlignPointX,j+fTextOffsetY,fTextAlign);
    j+=fRowHeight;  
  end;
end;

procedure TMenu.Move;
var i,j:integer;
begin
//  if fModified then SetupData;
  if fState=msActive then begin
    if keys[SDL_SCANCODE_UP] and (fSelected>fStart) then begin
      dec(fSelected);
      keys[SDL_SCANCODE_UP]:=false;
    end;
    if keys[SDL_SCANCODE_Down] and (fSelected<length(fVisibleItems)-1) then begin
      inc(fSelected);
      keys[SDL_SCANCODE_DOWN]:=false;
    end;
    if keys[SDL_SCANCODE_LEFT] and (fLines[fVisibleItems[fSelected]]._type=ltOption) then begin
      with fLines[fVisibleItems[fSelected]] do begin
        dec(_selectedoptionindex);
        if _selectedoptionindex=-1 then
          if fCycleOptions then _selectedoptionindex:=length(_optionstrings)-1
                           else _selectedoptionindex:=0;
      end;
      keys[SDL_SCANCODE_LEFT]:=false;
    end;
    if keys[SDL_SCANCODE_RIGHT] and (fLines[fVisibleItems[fSelected]]._type=ltOption) then begin
      with fLines[fVisibleItems[fSelected]] do begin
        inc(_selectedoptionindex);
        if _selectedoptionindex=length(_optionstrings) then
          if fCycleOptions then _selectedoptionindex:=0
                           else _selectedoptionindex:=length(_optionstrings)-1;
      end;
      keys[SDL_SCANCODE_RIGHT]:=false;
    end;
    if Keys[SDL_SCANCODE_RETURN] then begin
      if (fLines[fVisibleItems[fSelected]]._submenu) then begin
        FillItems(fVisibleItems[fSelected]);
        fSelected:=fStart;
      end else begin
        fState:=msSelected;
      end;
      keys[SDL_SCANCODE_RETURN]:=false;
    end;
    if Keys[SDL_SCANCODE_ESCAPE] then begin
      i:=fLines[fVisibleItems[fSelected]]._parent;
      if (i<>-1) then begin
        FillItems(fLines[i]._parent);
        fSelected:=fStart;
        for j:=0 to length(fVisibleItems)-1 do
          if fVisibleItems[j]=i then begin fSelected:=j;break;end;
      end else begin
        fState:=msEscaped;
      end;
      keys[SDL_SCANCODE_ESCAPE]:=false;
    end;
  end;
end;

function TMenu.GetValue(iLabel:string):string;
var i:integer;
begin
  for i:=0 to length(fLines)-1 do with fLines[i] do
    if (_type=ltOption) and (_label=iLabel) then begin
      Result:=_optionvalues[_selectedoptionindex];
      exit;
    end;
  GetValue:='';
end;

procedure TMenu.SetValue(iLabel,iValue:string);
var i,j:integer;
begin
//  if fModified then SetupData;
  for i:=0 to length(fLines)-1 do with fLines[i] do
    if (_type=ltOption) and (_label=iLabel) then
      for j:=0 to length(_optionvalues)-1 do
        if _optionvalues[j]=iValue then begin
          _selectedoptionindex:=j;
          exit;
        end;
end;

procedure TMenu.ExecuteSelf;
begin
//  if fModified then SetupData;
  fState:=msActive;
  MouseObjects.NewSession;
  MouseObjects.Add(self);
  repeat
    HandleMessages;
    Move;
    MouseObjects.Draw;
    Flip;
  until fState<>msActive;
  MouseObjects.EndSession;
end;

function TMenu.GetSelected:integer;
begin
  Result:=fVisibleItems[fSelected];
end;

procedure TMenu.SetSelected(iIndex:integer);
var i:integer;
begin
  FillItems(fLines[iIndex]._parent);
  for i:=0 to length(fVisibleItems)-1 do
    if fVisibleItems[i]=iIndex then fSelected:=i;
end;

function TMenu.GetSelectedLabel:string;
begin
  Result:=fLines[fVisibleItems[fSelected]]._label;
end;

procedure TMenu.SetSelectedLabel(iLabel:string);
var i,j:integer;
begin
  j:=0;
  for i:=0 to length(fLines)-1 do
    if fLines[i]._label=iLabel then begin j:=i;break;end;
  FillItems(fLines[j]._parent);
  for i:=0 to length(fVisibleItems)-1 do
    if fLines[fVisibleItems[i]]._label=iLabel then fSelected:=i;
end;

{function TMenu.OnMouseMove(x,y,buttons:integer):boolean;
var i:integer;
begin
  i:=(y-fTop) div fRowHeight;
  if i in [0..length(fVisibleItems)-1] then fSelected:=i;
  Result:=true;
end;

function TMenu.OnMouseDown(x,y,buttons:integer):boolean;
begin
  fExitKey:=SDLK_Return;
  Result:=true;
end;}

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  Menu:=TMenu.Create;

finalization
  Menu.Free;

end.
