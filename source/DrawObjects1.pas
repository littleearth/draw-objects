unit DrawObjects1;

(* *****************************************************************************
Module:          DrawObjects1
Description:     Base (abstract) DrawObject Classes
Version:         3.1.1
Date:            31-MAY-2008
Compiler:        Delphi 7
Author:          Angus Johnson,   angusj-AT-myrealbox-DOT-com
Copyright:       © 2006-2008 Angus Johnson
********************************************************************************
The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
********************************************************************************

Changes:
  ver 3.1.1      * Changed to draw rounded line ends, dots and dashes when
  29 May 08        Pen.Style <> psSolid
  ver 3.1        * Improved drawing of lines when Pen.Style <> psSolid
  29 May 08        (with thanks to Jim Bytheway for clue to use ExtCreatePen().)
  ver 3.0        * HitTest functionality has now been extended to all
  25 Jan 07      TDrawObject controls, not just TBaseLine descendants.
                 * The Mousedown() event no longer combines setting design focus
                 and initiating size/design events.
                 * CanMove property is now Protected (ie no longer Published)
  ver 2.9        * Exposed Margin property
  24 Jan 07      * Bug fix: TConnector descendants are now redrawn when their
                   Orientation property is changed.
  ver 2.8        * Added MoveBtnTo() method.
  23 Jan 07      * Left click with shift key pressed now toggles 'focus'.
  ver 2.7        * Bugfix in TConnector move logic (now blocked when connected
  18 Jan 07        to a TSolid object).
  ver 2.6        * Added UseHitTest property to TBaseLine so clicking within a
  16 Jan 07        line's boundsRect doesn't necessarily trigger mouse events.
                 * Fixed a bug which trimmed spaces from front and end of text.
                 * Minor improvement to TTextBezier drawing.
                 * TZline's center buttons are now hidden if AutoCenter = true.
                 * Bugfix: TRectangle's attached connectors are now redrawn when
                   Rounded property is changed.
  ver 2.5        * Improved TZLine AutoOrientation logic.
  11 Jan 07
  ver 2.4        * Improved shadowing of TTextBezier when Outline = foColored.
  10 Jan 07      * Fixed flicker in demo application whenever adjusting objects.
  ver 2.3        * Added TTextBezier component - displays text along a
  7 Jan 07         bezier path.
  ver 2.2        * Finally fixed transparency wben printing TPic objects. Also,
  5 Jan 07         stretched transparent TPic objects are now properly rendered
                   on screen (ie no longer with palette issues).
  ver 2.1        * Added TightConnections boolean property to TPic
  3 Jan 07       * Other minor tidy ups and further improved demo.
  ver 2.0        * Added 360 deg. rotation for TPic objects
  2 Jan 07       * Major improvements to accompanying demo appication.
  ver 1.9        * Bug fix: Occasionally, was moving on MouseMove without
  30 Dec 06        mouse capture.
  ver 1.8        * Added Rounded property to TRectangle.
  26 Dec 06      * Added Draw() method to significantly improve printing.
  ver 1.7        * Text in objects will now rotate with the object.
  16 Dec 06        This applies to (TRectangle, TDiamond, TEllipse & TText).
  ver 1.6        * Bug fix: TConnectors were not always being resized properly
  15 Dec 06        when a dis-connected end was moved while the other connected.
  ver 1.5        * Size() method renamed Zoom().
  5 Dec 06       * Changed BeginRotate() & EndRotate() to BeginTransform() and
                   EndTransform() since these methods are required for both
                   Zoom() & Rotate() to preserve proportional scaling and
                   orientation relative to a starting state.
                 * Rectangles and diamonds can now also be rotated. (However,
                   any text within these objects will remain unrotated.)
  ver 1.4        * Fixed broken 'CanMove' logic.
  ver 1.3        * Added 'CanFocus' property to simplify managing design focus.
  24 Nov 06      * Renamed 'MoveLocked' property to 'CanMove' property.
                 * Minor change to SetColor logic for TBaseline objects
  ver 1.2        * Subclassed TPen to default to width 2 and consequently
  21 Nov 06        removed the pen.width workaround from custom streaming.
***************************************************************************** *)

interface

uses
  Windows, SysUtils, Classes, Messages, Controls, Graphics, Math,
  Forms, TypInfo, ZLib;

const
  //:Respond to a click within 2 pixels of a TBaseLine (if UseHitTest = true)
  HITTESTSENSITIVITY = 2;

  MAX_SHADOW_SIZE = 50;
  FOCUSED_DESIGN_COLOR = clRed;
  FOCUSED_DESIGN_COLOR2 = clGreen;

  PI_Div4 = pi/4;
  PI_Div2 = pi/2;
  PI_Mul3_Div4 = pi *3/4;
  PI_Mul5_Div4 = pi *5/4;
  PI_Mul3_Div2 = pi *3/2;
  PI_Mul7_Div4 = pi *7/4;
  PI_Mul2 = pi * 2;

  CRLF: array [0..1] of char = #13#10;

type
  TBalloonPoint = (bpNone, bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight);
  TOrientation = (oHorizontal, oVertical);
  TQuadConnection = (qcLeft, qcTop, qcRight, qcBottom);

  {:When scaling objects using width & height it's vital to
  keep a copy of the points for proportional references.}
  TSavedSizeInfo = record
    SavedLeft: integer;
    SavedTop: integer;
    SavedWidth: integer;
    SavedHeight: integer;
    SavedPts: array of TPoint;

    isTransforming: boolean; //see Zoom & Rotate
    AngleInDegrees: integer;
    ZoomAsPercent: integer;  //also amalgamate this with TArc's fCurrRotAngle.
    RotateScreenPt: TPoint;
  end;

  TDrawObjectClass = class of TDrawObject;
  TDrawObject = class;

  PObjPropInfo = ^TObjPropInfo;
  TObjPropInfo = record
    fOwner: TDrawObject;
    fProperty: string;
    fPropVal: string;
  end;

  TPenEx = class(TPen)
    property Width default 2;
  end;

  {:Each object is it's own TGraphicControl to avoid repainting the whole
  drawing surface whenever any object is changed.}
  TDrawObject = class(TGraphicControl)
  private
    fBitmap: TBitmap;
    fPen: TPenEx;
    fPropStrings: TStrings;
    fBtnCount: integer;
    //:fMargin: Shadow margin around objects (also needed for line arrowheads)
    fMargin: integer;
    fBtnSize: integer;
    fFocused: boolean;
    fCanFocus: boolean;
    fShadowSize: integer;
    fColorShadow: TColor;
    fDistinctiveLastBtn: boolean;
    fPressedBtnIdx: integer;
    fUseHitTest: boolean;
    fDblClickPressed: boolean;
    fBlockResize: boolean;
    fUpdateNeeded, fReSizeNeeded: boolean;
    fMoving: boolean;
    fMovingPt: TPoint;
    fStreamID: string;
    fDataStream: TMemoryStream;
    fFocusChangedEvent: TNotifyEvent;
    procedure WriteBtnData(S : TStream);
    procedure WriteData(S : TStream);
    procedure ReadBtnData(S : TStream);
    procedure ReadData(S : TStream);
    procedure GetBinaryData(var lineIdx: integer); //loads binary data
    procedure SetPen(Value: TPenEx);
    procedure PenOnChange(Sender: TObject);
    procedure SetCanFocus(CanFocus: boolean);
    procedure SetFocused(Focused: boolean);
    procedure SetBtnSize(size: integer);
    procedure SetShadowSize(size: integer);
    procedure SetColorShadow(Color: TColor);
    function GetColor: TColor;
    function GetBitmap: TBitmap;
    procedure PrepareBitmap; virtual;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    BtnPoints: array of TPoint;
    //:SavedInfo: enables proportional zooming and rotating
    SavedInfo: TSavedSizeInfo;
    procedure SetColor(Color: TColor); virtual;
    procedure OffsetBtns(x,y: integer);
    procedure InternalSetCount(count: integer);
    procedure DefineProperties(Filer : TFiler); override;
    procedure ResizeNeeded; virtual;
    procedure UpdateNeeded; virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure CalcMargin; virtual;
    procedure DoSaveInfo; virtual;
    function GetCanMove: boolean; virtual;
    procedure SetUseHitTest(value: boolean); virtual;

    procedure DrawBtn(BtnPt: TPoint; index: integer; Pressed, LastBtn: boolean); virtual;
    procedure InternalResize;

    Procedure CMHittest(var msg: TCMHittest); message CM_HITTEST;
    procedure DrawControlLines; virtual;
    procedure DrawObject(Canvas: TCanvas; IsShadow: boolean); virtual;
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function IsValidBtnDown(BtnIdx: integer): boolean; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function IsValidBtnMove(BtnIdx: integer; NewPt: TPoint): boolean; virtual;
    procedure InternalBtnMove(BtnIdx: integer; NewPt: TPoint); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    {:Implement custom property streaming to text files ... see also
    SaveDrawObjectsToTextStream() and LoadDrawObjectsFromTextStream()}
    procedure LoadFromPropStrings;
    procedure BinaryDataLoaded; virtual;
    procedure SaveToPropStrings;  virtual;
    procedure AddToPropStrings(const PropName, PropVal: string);
    property BlockResize: boolean read fBlockResize write fBlockResize;
    property DataStream: TMemoryStream read fDataStream write fDataStream;
    property DistinctiveLastBtn: boolean
      read fDistinctiveLastBtn write fDistinctiveLastBtn;
    property PropStrings: TStrings read fPropStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(targetCanvas: TCanvas; offsetX, offsetY: integer);
    function Clone: TDrawObject;

    //:BtnIdxFromPt: returns false and BtnIdx := -1 if not a button
    function BtnIdxFromPt(pt: TPoint;
      ignoreDisabled: boolean; out BtnIdx: integer): boolean;
    function ObjectMidPoint: TPoint;
    //:PointOverObject: True if not over the transparent border of the object
    function PointOverObject(ScreenPt: TPoint): boolean; virtual;

    function MoveBtnTo(BtnIdx: integer; screenPt: TPoint): boolean;

    //:BeginTransform: Must be used in combination with
    procedure BeginTransform; virtual;
    //:EndTransform: Used with either Rotate() or Zoom().
    procedure EndTransform; virtual;
    procedure Rotate(degrees: integer); virtual; //(0..360)
    procedure Zoom(percent: integer); virtual;

    property ButtonCount: integer read fBtnCount;
    //:PressedBtnIdx: returns -1 if no button pressed
    property PressedBtnIdx: integer read fPressedBtnIdx;
    //:Bitmap: the image of the draw object
    property Bitmap: TBitmap read GetBitmap;

    //:CanMove: prevents the object from moving unless also focused (designing)
    property CanMove: boolean read GetCanMove;

    property Margin: integer read fMargin write fMargin;

    {:Moving: differentiates between a move and a resize operation
    and is typically useful when coding an OnMouseMove event.}
    property Moving: boolean read fMoving;
  published
    property ButtonSize: integer read fBtnSize write SetBtnSize;
    property Color read GetColor write SetColor;
    property ColorShadow: TColor read fColorShadow write SetColorShadow;

    //:CanFocus: when False, blocks the object gaining focus when clicked
    property CanFocus: boolean read fCanFocus write SetCanFocus;
    {:Focused: Displays the TDrawObjects's design buttons and a dotted bounding
    rectangle. Also triggers a FocusChangedEvent(). Note: TDrawObjects, being
    TGraphicControl descendants, never gain direct keyboard focus.
    Note - multiple TDrawObjects can have design focus concurrently.}
    property Focused: boolean read fFocused write SetFocused;

    property Pen: TPenEx read fPen write SetPen;
    property ShadowSize: integer read fShadowSize write SetShadowSize;
    property PopupMenu;
    property Tag;
    property UseHitTest: boolean read fUseHitTest write SetUseHitTest;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDblClick;
    property OnContextPopup;
    property FocusChangedEvent: TNotifyEvent read
      fFocusChangedEvent write fFocusChangedEvent;
  end;

  TBaseLine = class(TDrawObject)
  private
    fArrow1: boolean;
    fArrow2: boolean;
    function GetButtonCount: integer;
  protected
    procedure CalcMargin; override;
    procedure SetButtonCount(count: integer); virtual;
    procedure SetArrow1(Arrow: boolean); virtual;
    procedure SetArrow2(Arrow: boolean); virtual;
    procedure SaveToPropStrings;  override;
  public
    constructor Create(AOwner: TComponent); override;
    //:Grow: extend the line by adding more control points
    function Grow(TopEnd: boolean = false): boolean; virtual;
    function Shrink(TopEnd: boolean = false): boolean; virtual;
  published
    property Arrow1: boolean read fArrow1 write SetArrow1;
    property Arrow2: boolean read fArrow2 write SetArrow2;
    property ButtonCount: integer read GetButtonCount write SetButtonCount;
  end;

  TSolid = class; //forward declaration

  TConnector = class(TBaseLine)
  private
    fQuadPtConnect: boolean;
    fOrientation: TOrientation;
    fAutoOrientation: boolean;
    fConnection1: TSolid;
    fConnection2: TSolid;
    procedure SetOrientation(orientation: TOrientation);
    procedure SetConnection1(Connection: TSolid);
    procedure SetConnection2(Connection: TSolid);
    procedure SetAutoOrientation(value: boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateConnectionPoints(MovingConnection: TSolid); virtual;
    procedure Resize; override;
    function GetCanMove: boolean; override;
    procedure DoQuadPtConnection; virtual;
    procedure UpdateNonEndButtonsAfterBtnMove; virtual;
    function IsValidBtnDown(BtnIdx: integer): boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SaveToPropStrings;  override;
    //QuadPtConnect: only allow connections at NSEW points on TSolid objects
    property QuadPtConnect: boolean read fQuadPtConnect write fQuadPtConnect;
    property Orientation: TOrientation read fOrientation write SetOrientation;
    property AutoOrientation: boolean read fAutoOrientation write SetAutoOrientation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rotate(degrees: integer); override;
    function IsConnected: boolean;
  published
    {:TConnector descendants can connect to one or two TSolids
    and they will automatically follow the Solids' movements.
    (Semantics: lines == connectors, TSolid == connection objects)}
    property Connection1: TSolid read fConnection1 write SetConnection1;
    property Connection2: TSolid read fConnection2 write SetConnection2;
  end;

  TSolid = class(TDrawObject)
  private
    fCanConnect: boolean;
    fConnectorList: TList;
    procedure RemoveConnector(Connector: TConnector);
  protected
    procedure AddConnector(Connector: TConnector); virtual;
    procedure Resize; override;
    property CanConnect: boolean read fCanConnect write fCanConnect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {:ClosestScreenPt: return the closest point on the object's circumference
    (in screen coordinates) to a given point. (Used by TConnectors.)}
    function ClosestScreenPt(FromScreenPt: TPoint): TPoint; virtual;
    //:QuadScreenPt: return the N,S,E or W point depending on QuadConnect param.
    function QuadScreenPt(QuadConnect: TQuadConnection): TPoint; virtual;
    //:ConnectorList: list of attached TConnectors
    property ConnectorList: TList read fConnectorList;
  end;

  {NOTE: TDrawObject, TSolid and TConnector should be considered as
  co-dependant base objects.}
  TSolidWithText = class(TSolid)
  private
    fPadding: integer;
    fStrings: TStrings;
    fAction: String;
    procedure SetPadding(padding: integer);
    procedure SetStrings(strings: TStrings);
    procedure StringsOnChange(Sender: TObject);
    function GetAngle: integer;
  protected
    procedure SetAngle(angle: integer); virtual;
    procedure RotatedTextAtPt(Canvas: TCanvas; x,y: integer; const s: string);
    procedure DrawBtn(BtnPt: TPoint; index: integer; Pressed, LastBtn: boolean); override;
    function IsValidBtnDown(BtnIdx: integer): boolean; override;
    procedure InternalBtnMove(BtnIdx: integer; NewPt: TPoint); override;
    procedure SaveToPropStrings;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Rotate(degrees: integer); override;
    function ResizeObjectToFitText: boolean; virtual;
  published
    {:Action: a user defined label for an 'action' that can be implemented
    for an event, typically an OnClick event}
    property Action: String read fAction write fAction;
    //:Angle: rotation angle. See also the Rotation() method.
    property Angle: integer read GetAngle write SetAngle;
    //:Strings: displayed text on TSolid object
    property Strings: TStrings read fStrings write SetStrings;
    //:Padding: minimum space around text
    property Padding: integer read fPadding write SetPadding;
    property Font;
    property ParentFont;
  end;

function PtInPolygon(const pts: array of TPoint; pt: TPoint): Boolean;
function MidPoint(pt1, pt2: TPoint): TPoint;
procedure OffsetPt(var pt: TPoint; dx, dy: integer);
function RotatePt(pt, origin: TPoint; radians: single): TPoint;
procedure RotatePts(var pts: array of TPoint;
  startArrayIdx: integer; origin: TPoint; radians: single);
function SquaredDistBetweenPoints(pt1, pt2: TPoint): cardinal;
function IntersectionPoint(Line1a, line1b, Line2a, line2b: TPoint;
  out IntersectionPt: TPoint): boolean;
function GetAnglePt2FromPt1(pt1, pt2: TPoint): single;
function SlopeLessThanOne(pt1,pt2: TPoint): boolean;
procedure SaveDrawObjectsToStrings(DoList: TList; Strings: TStrings);
procedure LoadDrawObjectsFromStrings(strings: TStrings; Owner: TComponent;
  Parent: TWinControl; EachDrawObjLoadedEvent: TNotifyEvent);
function MakeNameForControl(ctrl: TControl): boolean;
function CreatePenHandle(TemplatePen: TPen): HPen;
procedure DrawDottedPolyline(Canvas: TCanvas; pts: array of TPoint);

implementation

uses Types;

var ObjPropertyList: TList;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function ClickPosNearPaintedObject(bmp: TBitmap; X,Y, dist: integer): boolean;
var
  i,j: integer;
  clr: TColor;
begin
  //tests if pos(X,Y) is within 'dist' pixels of the painted object (line) ...
  result := true;
  with bmp do
  begin
    clr := (TransparentColor and $FFFFFF);
    if canvas.Pixels[X,Y] <> clr  then exit;
    if dist > 0 then
    begin
      for i := max(0, X - dist) to min(X + dist, width-1) do
        for j := max(0, Y - dist) to min(Y + dist, height-1) do
          if canvas.Pixels[i,j] <> clr then exit;
    end;
  end;
  result := false;
end;
//------------------------------------------------------------------------------

function TrimNonPrintChrs(const S: string): string;
var
  i,j: Integer;
begin
  i := 1;
  j := Length(S);
  while (j > 0) and (S[j] < ' ') do Dec(j);
  while (i < j) and (S[i] < ' ') do Inc(i);
  Result := Copy(S, i, j-i+1);
end;
//------------------------------------------------------------------------------

function PtInPolygon(const pts: array of TPoint; pt: TPoint): Boolean;
var
  i,j : integer;
begin
  Result := False;
  j := high(pts);
  for i := low(pts) to high(pts) do
  begin
   if ((pts[i].Y <= pt.Y) and (pt.Y < pts[j].Y)) or
      ((pts[j].Y <= pt.Y) and (pt.Y < pts[i].Y)) then
        if (pt.X < (pts[j].X - pts[i].X) * (pt.Y - pts[i].Y) /
            (pts[j].Y - pts[i].Y) + pts[i].X) then Result := not Result;
    j := i;
  end;
end;
//------------------------------------------------------------------------------

function InvertColor(color: TColor): TColor;
var
  r,g,b: byte;
begin
  Color := ColorToRGB(color);
  b := (Color shr 16) and $FF;
  g := (Color shr 8) and $FF;
  r := (Color and $FF);
  b := 255 - b;
  g := 255 - g;
  r := 255 - r;
  result := (b shl 16) or (g shl 8) or r;
end;
//------------------------------------------------------------------------------

function SlopeLessThanOne(pt1,pt2: TPoint): boolean;
begin
  if pt2.X = pt1.X then result := false
  else result := abs((pt2.y - pt1.y)/(pt2.x - pt1.x)) < 1;
end;
//------------------------------------------------------------------------------

function BtnRectFromPt(pt: TPoint; size: integer): TRect;
begin
  result := Rect(pt.X-size, pt.Y-size, pt.X+size,pt.Y+size);
end;
//------------------------------------------------------------------------------

procedure OffsetPt(var pt: TPoint; dx, dy: integer);
begin
  inc(pt.X , dx);
  inc(pt.Y , dy);
end;
//------------------------------------------------------------------------------

function MidPoint(pt1, pt2: TPoint): TPoint;
begin
  result.X := (pt1.X + pt2.X) div 2;
  result.Y := (pt1.Y + pt2.Y) div 2;
end;
//------------------------------------------------------------------------------

function RotatePt(pt, origin: TPoint; radians: single): TPoint;
var
  x,y: integer;
  cosAng, sinAng: single;
begin
  cosAng := cos(radians);
  sinAng := sin(radians);
  x := pt.X - origin.X;
  y := pt.Y - origin.Y;
  result.X := round((x * cosAng) - (y * sinAng)) + origin.X;
  result.Y := round((x * sinAng) + (y * cosAng)) + origin.Y;
end;
//------------------------------------------------------------------------------

procedure RotatePts(var pts: array of TPoint;
  startArrayIdx: integer; origin: TPoint; radians: single);
var
  i,x,y: integer;
  cosAng, sinAng: single;
begin
  cosAng := cos(radians);
  sinAng := sin(radians);

  for i := startArrayIdx to high(pts) do
  begin
    x := pts[i].X - origin.X;
    y := pts[i].Y - origin.Y;
    pts[i].X := round((x * cosAng) - (y * sinAng)) + origin.X;
    pts[i].Y := round((x * sinAng) + (y * cosAng)) + origin.Y;
  end;
end;
//------------------------------------------------------------------------------

function SquaredDistBetweenPoints(pt1, pt2: TPoint): cardinal;
begin
  result := sqr(pt2.X - pt1.X) + sqr(pt2.Y - pt1.Y);
end;
//------------------------------------------------------------------------------

function IntersectionPoint(Line1a, line1b, Line2a, line2b: TPoint;
  out IntersectionPt: TPoint): boolean;
var
  m1,m2,b1,b2: single;
begin
  //nb: Y axis is positive down ...
  //y = mx + b  ;  b := y - mx
  IntersectionPt := Point(0,0);
  result := false;
  if (line1b.X = line1a.X) then                            //vertical line
  begin
    if (line2b.X = line2a.X) then exit;                    //parallel lines
    m2 := (line2b.Y - line2a.Y)/(line2b.X - line2a.X);
    b2 := Line2a.Y - m2 * Line2a.X;
    IntersectionPt.X := line1a.X;
    IntersectionPt.Y := round((line1a.X*m2) + b2);
  end else
  if (line2b.X = line2a.X) then                            //vertical line
  begin
    if (line1b.X = line1a.X) then exit;                    //parallel lines
    m1 := (line1b.Y - line1a.Y)/(line1b.X - line1a.X);
    b1 := Line1a.Y - m1 * Line1a.X;
    IntersectionPt.X := line2a.X;
    IntersectionPt.Y := round((line2a.X*m1) + b1);
  end else
  begin
    m1 := (line1b.Y - line1a.Y)/(line1b.X - line1a.X);
    m2 := (line2b.Y - line2a.Y)/(line2b.X - line2a.X);
    if m1 = m2 then exit;                                  //parellel lines
    b1 := Line1a.Y - m1 * Line1a.X;
    b2 := Line2a.Y - m2 * Line2a.X;
    //via similtaneous equations ...
    IntersectionPt.X := round((b1 - b2)/(m2 - m1));
    IntersectionPt.Y := round(m1*(b1 - b2)/(m2 - m1) + b1);
  end;
  result := true;
end;
//------------------------------------------------------------------------------

function GetAnglePt2FromPt1(pt1, pt2: TPoint): single;
begin
  with pt1 do OffsetPt(pt2,-X,-Y);
  with pt2 do
    if X = 0 then
    begin
      result := PI_Div2;
      if Y > 0 then result := PI_Mul3_Div2;
    end else
    begin
      result := arctan2(-Y,X);
      if result < 0 then result := result + PI_Mul2;
    end;
end;
//------------------------------------------------------------------------------

procedure SaveDrawObjectsToStrings(DoList: TList; Strings: TStrings);
var
  i: integer;
begin
  //use the object pointer as a unique ID ...
  for i := 0 to DoList.Count -1 do
    TDrawObject(DoList[i]).fStreamID := inttohex(longint(DoList[i]),8);

  for i := 0 to DoList.Count -1 do
    with TDrawObject(DoList[i]) do
    begin
      SaveToPropStrings;
      Strings.AddStrings(PropStrings);
      Strings.Add(''); //add a blank line between each component
      fStreamID := ''; //finally, cleanup temp streaming name
    end;
end;
//------------------------------------------------------------------------------

function FindNextObjectLineIdx(Lines: TStrings; StartAtLine: integer): integer;
var
  EndAtLine: integer;
begin
  EndAtLine := Lines.Count;
  result := StartAtLine;
  while (result < EndAtLine) do
    if (Lines[result] <> '') and (Lines[result][1] = '[') then break
    else inc(result);
end;
//------------------------------------------------------------------------------

procedure GetClassAndName(const ClassInfoLine: string;
  out objClass, objName: string);
var
  i: integer;
begin
  i := pos(':',ClassInfoLine);
  objClass := copy(ClassInfoLine,2,i-2);
  objName := copy(ClassInfoLine,i+1,length(ClassInfoLine)-i -1);
end;
//------------------------------------------------------------------------------

procedure ClearAndNilObjPropertyList;
var
  i: integer;
begin
  if not assigned(ObjPropertyList) then exit;
  for i := 0 to ObjPropertyList.Count -1 do
    dispose(PObjPropInfo(ObjPropertyList[i]));
  FreeAndNil(ObjPropertyList);
end;
//------------------------------------------------------------------------------

procedure LoadDrawObjectsFromStrings(strings: TStrings; Owner: TComponent;
  Parent: TWinControl; EachDrawObjLoadedEvent: TNotifyEvent);
var
  i,j: integer;
  ObjList: TList;

{steps:
1. Create a list of instantiated DrawObjects
2. DrawObjects fill their own properties and add any DrawObj links to a fixup
   list (containing instance, property, name value)
3. Parse fixup list - using name values to find and assign link instances }

  function NameInUse(const newName: string): boolean;
  var
    i: integer;
  begin
    result := true;
    for i := 0 to Owner.ComponentCount -1 do
      if SameText(Owner.Components[i].Name, newName) then exit;
    result := false;
  end;

  procedure BuildObjList(Lines: TStrings; ObjList: TList);
  var
    i, ClassInfoLineIdx, StartAtLine, EndAtLine: integer;
    ObjInstance: TDrawObject;
    objClass, objName: string;
    PersistentClass: TPersistentClass;
  begin
    EndAtLine := Lines.Count;
    ClassInfoLineIdx := FindNextObjectLineIdx(Lines, 0);
    while ClassInfoLineIdx < EndAtLine do
    begin
      //for each item ...
      StartAtLine := ClassInfoLineIdx;
      GetClassAndName(Lines[ClassInfoLineIdx], objClass, objName);
      ObjInstance := nil;
      try
        PersistentClass := FindClass(objClass);
        if assigned(PersistentClass) and
          PersistentClass.InheritsFrom(TDrawObject) then
            ObjInstance := TDrawObjectClass(PersistentClass).Create(Owner);
      except
      end;
      //find offset of next item so we know which lines pertain to this one ...
      ClassInfoLineIdx := FindNextObjectLineIdx(Lines, ClassInfoLineIdx+1);

      if ObjInstance = nil then continue;

      ObjInstance.Parent := Parent;
      
      if NameInUse(objName) then
        MakeNameForControl(ObjInstance) else
        ObjInstance.Name := objName;

      ObjList.Add(ObjInstance);
      //copy property strings to the new object ...
      for i := StartAtLine to ClassInfoLineIdx -1 do
        ObjInstance.PropStrings.Add(Lines[i]);
      ObjInstance.LoadFromPropStrings;
      ObjInstance.PropStrings.Clear;
    end;
  end;

begin
  if not assigned(owner) or not assigned(parent) then exit;

  ObjPropertyList := TList.Create;
  ObjList := TList.create;
  try
    //fill ObjList with new top level items ...
    BuildObjList(Strings, ObjList);

    //fixup any entries in ObjPropertyList...
    for i := 0 to ObjPropertyList.Count -1 do
      with PObjPropInfo(ObjPropertyList[i])^ do
        for j := 0 to ObjList.Count -1 do
          if (TDrawObject(ObjList[j]).fStreamID = fPropVal) then
          begin
            SetObjectProp(fOwner, fProperty, TObject(ObjList[j]));
            break; //ie this property has been fixed up
          end;
    for i := 0 to ObjList.count -1 do
    begin
      TDrawObject(ObjList[i]).fStreamID := '';
      //notify of object creation ...
      if assigned(EachDrawObjLoadedEvent) then
        EachDrawObjLoadedEvent(TDrawObject(ObjList[i]));
    end;
  finally
    ObjList.free;
    ClearAndNilObjPropertyList;
  end;
end;
//------------------------------------------------------------------------------

procedure GetStrings(FromStrings, ToStrings: TStrings; var lineIdx: integer);
var
  i: integer;
begin
  //format (nb: contained lines all indented 2 spaces) -
  //stringsPropName={
  //  contained text line 1
  //  contained text line 2
  //}
  if not assigned(FromStrings) or not assigned(toStrings) then exit;
  i := pos('={', FromStrings[lineIdx]);
  if i = 0 then exit;
  inc(lineIdx);
  while (lineIdx < FromStrings.Count) and (FromStrings[lineIdx] <> '}') do
  begin
    //strip 2 spaces at beginning of every line ...
    ToStrings.Add(copy(FromStrings[lineIdx],3,512));
    inc(lineIdx);
  end;
end;
//------------------------------------------------------------------------------

function MakeNameForControl(ctrl: TControl): boolean;
var
  i,j: integer;
  s: string;
  ownerCtrl: TComponent;
begin
  result := false;
  if not assigned(ctrl) or not assigned(ctrl.Owner) or (ctrl.Name <> '') then exit;
  ownerCtrl := ctrl.Owner;
  s := copy(ctrl.ClassName, 2, 256);
  j := 0;
  while not result do
  begin
    inc(j);
    result := true;
    with ownerCtrl do
      for i := 0 to ComponentCount -1 do
        if SameText( Components[i].Name, s + inttostr(j)) then
        begin
          result := false;
          break;
        end;
    if result then ctrl.Name := s + inttostr(j);
  end;
end;


//------------------------------------------------------------------------------
// TDrawObject methods
//------------------------------------------------------------------------------

constructor TDrawObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBitmap := TBitmap.Create;
  fPen := TPenEx.create;
  fPen.Width := 2;
  fPen.OnChange := PenOnChange;

  fPropStrings := TStringList.Create;

  InternalSetCount(2);
  SavedInfo.ZoomAsPercent := 100;
  fBtnSize := 4;
  fMargin := 2;
  fPressedBtnIdx := -1;
  fDistinctiveLastBtn := false;
  fUseHitTest := true;

  fCanFocus := true;
  ParentColor := false;
  inherited Color := clWhite;
  fBitmap.Transparent := true;
  if Owner is TCustomForm then
    fBitmap.TransparentColor := TCustomForm(Owner).Color else
    fBitmap.TransparentColor := clBtnFace;
  fColorShadow := clWhite;
  fShadowSize := -2;
  fBitmap.Width := 100;
  fBitmap.Height := 100;
  SetBounds(0,0,100,100);
  CalcMargin; //4-dec-2005 moved up 3 lines
  BtnPoints[0] := Point(fMargin, fMargin);
  BtnPoints[1] := Point(width-fMargin, height-fMargin);
  fUpdateNeeded := true;
end;
//------------------------------------------------------------------------------

destructor TDrawObject.Destroy;
begin
  fBitmap.free;
  fPen.Free;
  fPropStrings.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('BinaryBtnData', ReadBtnData,
    WriteBtnData, fBtnCount > 0);
  Filer.DefineBinaryProperty('BinaryData', ReadData,
    WriteData, assigned(fDataStream) and (fDataStream.Size > 0));
end;
//------------------------------------------------------------------------------

procedure TDrawObject.ReadBtnData(S: TStream);
var
  i: integer;
begin
  for i := 0 to fBtnCount-1 do S.Read(BtnPoints[i], SizeOf(TPoint));
end;
//------------------------------------------------------------------------------

procedure TDrawObject.ReadData(S: TStream);
begin
  fDataStream.LoadFromStream(S);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.WriteBtnData(S: TStream);
var
  i: integer;
begin
  for i := 0 to fBtnCount-1 do S.Write(BtnPoints[i],SizeOf(TPoint));
end;
//------------------------------------------------------------------------------

procedure TDrawObject.WriteData(S: TStream);
begin
  fDataStream.Position := 0;
  S.CopyFrom(fDataStream, fDataStream.Size);
end;
//------------------------------------------------------------------------------

function TDrawObject.GetCanMove: boolean;
begin
  result := focused;
end;
//------------------------------------------------------------------------------

function TDrawObject.Clone: TDrawObject;
begin
  result := nil;
  if not assigned(owner) then exit;

  //load the sl with property values ...
  SaveToPropStrings;
  result := TDrawObjectClass(ClassType).Create(Owner);
  //copy property values in sl to new object ...
  result.PropStrings.Assign(PropStrings);
  result.LoadFromPropStrings;
  result.left := result.left + 10;
  result.top := result.top + 10;
  result.Parent := parent;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.InternalSetCount(count: integer);
begin
  if count < 1 then raise Exception.Create('Not enough buttons');
  fBtnCount := count;
  setLength(BtnPoints, fBtnCount);
  setLength(SavedInfo.SavedPts, fBtnCount);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.ResizeNeeded;
begin
  if fBlockResize then exit;
  fResizeNeeded := true;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.UpdateNeeded;
begin
  fUpdateNeeded := true;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.OffsetBtns(x,y: integer);
var
  i: integer;
begin
  for i := 0 to fBtnCount -1 do
  begin
    inc(BtnPoints[i].X, x);
    inc(BtnPoints[i].Y, y);
  end;
end;
//------------------------------------------------------------------------------

function TDrawObject.ObjectMidPoint: TPoint;
begin
  result := Point(width div 2, height div 2);
end;
//------------------------------------------------------------------------------

function TDrawObject.PointOverObject(ScreenPt: TPoint): boolean;
var
  pt: TPoint;
begin
  pt := ScreenToClient(ScreenPt);
  //nb: returns false if Pt in object is transparent (same color as its parent)
  with fBitmap.Canvas do
    result := PtInRect(ClientRect,pt) and
    (Pixels[pt.X, pt.Y] <> Pixels[width-1, height-1]);
end;
//------------------------------------------------------------------------------

function TDrawObject.MoveBtnTo(BtnIdx: integer; screenPt: TPoint): boolean;
var
  clientPt: TPoint;
begin
  clientPt := ScreenToClient(screenPt);
  result := IsValidBtnMove(BtnIdx, clientPt);
  if result then InternalBtnMove(BtnIdx, clientPt);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.DrawObject(Canvas: TCanvas; IsShadow: boolean);
begin
  //override this method in descendant classes
  Canvas.Polygon(BtnPoints);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.DrawControlLines;
begin
  //implement this method in descendant classes
end;
//------------------------------------------------------------------------------

procedure TDrawObject.Paint;
var
  i: integer;
begin
  if fResizeNeeded then InternalResize;
  if fUpdateNeeded then PrepareBitmap;
  Canvas.Draw(0,0, fBitmap);

  if (Focused or (csDesigning in ComponentState)) then
    with Canvas do
    begin
      //draw control lines ...
      Pen.Color := FOCUSED_DESIGN_COLOR;
      Pen.Width := 1;
      Pen.Style := psDot;
      brush.Style := bsClear;
      canvas.Rectangle(clientRect);
      DrawControlLines;

      //finally, draw buttons ...
      Pen.Style := psSolid;
      for i := 0 to ButtonCount-1 do
        DrawBtn(BtnPoints[i], i, i = fPressedBtnIdx,
          (i = ButtonCount-1) and fDistinctiveLastBtn);
    end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.Loaded;
begin
  inherited;
  fBitmap.Width := Width;
  fBitmap.Height := Height;
  fResizeNeeded := false;
  DoSaveInfo;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.Resize;
var
  i, margX2: integer;
  ratioW, ratioH: single;
begin
  inherited Resize;
  if fBlockResize then exit;

  //whenever the drawobject is resized externally, SCALE the resulting object ...
  if ((width <> fBitmap.Width) or (height <> fBitmap.Height)) then
  begin
    if (width <= fMargin) or (height <= fMargin) then exit;
    margX2 := fMargin *2;
    //scale all button positions to the new dimensions ...
    if (SavedInfo.SavedWidth - margX2 = 0) then
      ratioW := 1 else
      ratioW := (width- margX2)/(SavedInfo.SavedWidth - margX2);
    if (SavedInfo.SavedHeight - margX2 = 0) then
      ratioH := 1 else
      ratioH := (height- margX2)/(SavedInfo.SavedHeight - margX2);
    fBitmap.Width := width;
    fBitmap.Height := height;
    for i := 0 to ButtonCount -1 do
    begin
      BtnPoints[i].X := round((SavedInfo.SavedPts[i].X-fMargin) * ratioW) +fMargin;
      BtnPoints[i].Y := round((SavedInfo.SavedPts[i].Y-fMargin) * ratioH) +fMargin;
    end;
    UpdateNeeded;
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.DoSaveInfo;
var
  i: integer;
begin
  with SavedInfo do
  begin
    if isTransforming then exit;
    //nb: fBitmap is the size of the object dimensions prior to resizing ...
    SavedLeft := left;
    SavedTop := top;
    SavedWidth := fBitmap.width;
    SavedHeight := fBitmap.height;
    for i := 0 to ButtonCount-1 do
    begin
      SavedPts[i].X := BtnPoints[i].X;
      SavedPts[i].Y := BtnPoints[i].Y;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.DrawBtn(BtnPt: TPoint; index: integer; Pressed, LastBtn: boolean);
var
  BtnRect: TRect;
begin
  BtnRect := BtnRectFromPt(BtnPt, fBtnSize);
  with canvas do
  begin
    if Focused or (fPressedBtnIdx >= 0) or Moving then
    begin
      if LastBtn then Pen.Color := FOCUSED_DESIGN_COLOR2
      else Pen.Color := FOCUSED_DESIGN_COLOR
    end else Pen.Color := clSkyBlue;
    Rectangle(BtnRect);
    InflateRect(BtnRect,-1,-1);
    if Pressed then
      Pen.Color := clBtnShadow else
      Pen.Color := clBtnHighlight;
    MoveTo(BtnRect.Left, BtnRect.Bottom-1);
    LineTo(BtnRect.Left, BtnRect.Top);
    LineTo(BtnRect.Right-1, BtnRect.Top);
    if Pressed then
      Pen.Color := clBtnHighlight else
      Pen.Color := clBtnShadow;
    LineTo(BtnRect.Right-1, BtnRect.Bottom-1);
    LineTo(BtnRect.Left-1, BtnRect.Bottom-1);
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.InternalResize;
var
  i: integer;
  NewBounds: TRect;
begin
  with BtnPoints[0] do NewBounds := Rect(X,Y,X,Y);
  for i := 1 to ButtonCount-1 do
  begin
    if BtnPoints[i].X < NewBounds.Left  then NewBounds.Left := BtnPoints[i].X;
    if BtnPoints[i].X > NewBounds.Right  then NewBounds.Right := BtnPoints[i].X;
    if BtnPoints[i].Y < NewBounds.Top  then NewBounds.Top := BtnPoints[i].Y;
    if BtnPoints[i].Y > NewBounds.Bottom then NewBounds.Bottom := BtnPoints[i].Y;
  end;
  OffsetBtns(fMargin - NewBounds.Left, fMargin - NewBounds.Top);
  OffsetRect(NewBounds, Left, Top);
  InflateRect(NewBounds, fMargin, fMargin);
  with NewBounds do
  begin
    Right := Right - Left; //ie right = width
    Bottom := Bottom - Top; //ie bottom = height
    if (Left <> self.Left) or (Top <> self.Top) or
     (Right <> self.width) or (Bottom <> self.Height) then
    begin
      fBitmap.Width := Right;
      fBitmap.Height := Bottom;
      fBlockResize := true; //blocks scaling while resizing Bitmap
      try
        //todo - reorder InternalResize logic to come AFTER
        //UpdateConnectionPoints which is called indirectly via setBounds ...
        self.setBounds(Left, Top, Right, Bottom);
      finally
        fBlockResize := false;
      end;
    end
    else if self is TSolid and assigned(TSolid(self).ConnectorList) then
    begin
      with TSolid(self) do for i := 0 to fConnectorList.Count -1 do
        TConnector(fConnectorList[i]).UpdateConnectionPoints(TSolid(self));
    end;
  end;
  DoSaveInfo;
  fResizeNeeded := false;
  fUpdateNeeded := true;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.Draw(targetCanvas: TCanvas; offsetX, offsetY: integer);
begin
  with targetCanvas do
  begin
    Font.Assign(self.font);
    Pen.Assign(self.fPen);
    //draw the object shadow ...
    if fShadowSize <> 0 then
    begin
      Pen.Color := fColorShadow;
      brush.Color := fColorShadow;
      OffsetBtns(fShadowSize+offsetX, fShadowSize+offsetY);
      try
        DrawObject(targetCanvas, true);
      finally
        OffsetBtns(-fShadowSize-offsetX,-fShadowSize-offsetY);
      end;
    end;

    //draw the object ...
    Pen.Color := self.Pen.Color;
    brush.Color := self.Color;
    OffsetBtns(offsetX, offsetY);
    try
      DrawObject(targetCanvas, false);
    finally
      OffsetBtns(-offsetX, -offsetY);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.PrepareBitmap;
begin
  fBitmap.canvas.brush.Color := fBitmap.TransparentColor and $FFFFFF;
  fBitmap.canvas.FillRect(ClientRect);
  Draw(fBitmap.canvas, 0, 0);
  invalidate;
  fUpdateNeeded := false;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetPen(Value: TPenEx);
begin
  fPen.Assign(Value);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.PenOnChange(Sender: TObject);
begin
  CalcMargin;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetFocused(Focused: boolean);
begin
  if (fFocused = Focused) or (not fCanFocus and Focused) then exit;
  fFocused := Focused;
  if assigned(fFocusChangedEvent) then fFocusChangedEvent(self);
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetCanFocus(CanFocus: boolean);
begin
  if fCanFocus = CanFocus then exit;
  fCanFocus := CanFocus;
  if not CanFocus then SetFocused(false);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.CalcMargin;
var
  pwDiv2: integer;
begin
  //may need to override this method (eg if line arrowheads are made sizable)
  if odd(pen.Width) then
    pwDiv2 := (pen.Width div 2) +1 else
    pwDiv2 := (pen.Width div 2);
  //make sure there's at least 2 pxls outside buttons for designer size btns...
  fmargin := max(fBtnSize + 2, pwDiv2 + max(2, abs(fShadowSize)));
  ResizeNeeded;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetBtnSize(size: integer);
begin
  if (size < 3) or (size > 10) then exit;
  if fBtnSize = size then exit;
  fBtnSize := size;
  CalcMargin;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetShadowSize(size: integer);
begin
  if size > MAX_SHADOW_SIZE then size := MAX_SHADOW_SIZE
  else if size < -MAX_SHADOW_SIZE then size := -MAX_SHADOW_SIZE;
  if fShadowSize = size then exit;
  fShadowSize := size;
  CalcMargin;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetColorShadow(Color: TColor);
begin
  if fColorShadow = Color then exit;
  fColorShadow := Color;
  UpdateNeeded;
end;
//------------------------------------------------------------------------------

function TDrawObject.GetColor: TColor;
begin
  result := inherited Color;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetColor(Color: TColor);
begin
  if inherited Color = Color then exit;
  inherited Color := Color;
  UpdateNeeded;
end;
//------------------------------------------------------------------------------

function TDrawObject.GetBitmap: TBitmap;
begin
  if fUpdateNeeded then PrepareBitmap;
  result := fBitmap;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SetUseHitTest(value: boolean);
begin
  fUseHitTest := value;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.CMHittest(var msg: TCMHIttest);
begin
  if Focused or not fUseHitTest then
    msg.result := HTCLIENT
  else if ClickPosNearPaintedObject(Bitmap,
    msg.XPos, msg.YPos, HITTESTSENSITIVITY) then
      msg.Result := HTCLIENT
  else
    msg.result := HTNOWHERE;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.DblClick;
begin
  inherited;
  //the object can lose 'capture' via DblClick (eg a popup window) so
  //flag DblClick to avoid issues in MouseDown() & MouseMove() ...
  fDblClickPressed := true;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if not (csDesigning in ComponentState) and not fDblClickPressed then
  begin
    //nb: left click while shift key pressed will toggle selection ...
    if (ssShift in Shift) and focused and (Button = mbLeft) then
    begin
      Focused := false;
      exit;
    end else if not focused and CanFocus then
    begin
      Focused := true;
      exit;
    end;

    if Button = mbLeft then
    begin
      if not focused then fPressedBtnIdx := -1
      else if BtnIdxFromPt(Point(X,Y), true, fPressedBtnIdx) then invalidate;
      if (fPressedBtnIdx < 0) and CanMove then
      begin
        fMovingPt := Point(X,Y);
        fMoving := true;
      end;
    end;
  end;
  fDblClickPressed := false;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPt: TPoint;
  idx, dx,dy: integer;
  r: TRect;
begin
  if (csDesigning in ComponentState) then
  begin
    inherited
  end else
  begin
    dx := left; dy := top;
    NewPt := Point(X,Y);
    if BtnIdxFromPt(NewPt, true, idx) then
      cursor := crHandPoint else cursor := crDefault;
    if (fPressedBtnIdx >= 0) then
    begin
      //button move (ie resizing object)
      if IsValidBtnMove(fPressedBtnIdx, NewPt) then
        InternalBtnMove(fPressedBtnIdx, NewPt);
      //bugfix (2-May-08) ...
      //control disappeared when it had its last remaining button temporarily
      //dragged off parent canvas but wouldn't reappear on returning ...
      if not IntersectRect(r, self.BoundsRect, parent.ClientRect) then
        InternalResize; //needed because Paint() isn't called
    end else if fMoving then
      SetBounds(left+ X - fMovingPt.X ,top + Y - fMovingPt.Y, width, height);

    dx := left - dx; dy := top -dy; //bugfix (17-Jan-07)
    inherited MouseMove(Shift, X -dx, Y -dy);
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    if fPressedBtnIdx >= 0 then
    begin
      fPressedBtnIdx := -1;
      invalidate;
    end;
    fMoving := false;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

function TDrawObject.IsValidBtnDown(BtnIdx: integer): boolean;
begin
  //override in descendant classes
  result := true;
end;
//------------------------------------------------------------------------------

function TDrawObject.IsValidBtnMove(BtnIdx: integer; NewPt: TPoint): boolean;
begin
  result := (BtnIdx >= 0) and (BtnIdx < ButtonCount);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.InternalBtnMove(BtnIdx: integer; NewPt: TPoint);
begin
  BtnPoints[BtnIdx] := NewPt;
  ResizeNeeded;
end;
//------------------------------------------------------------------------------

function TDrawObject.BtnIdxFromPt(pt: TPoint;
  ignoreDisabled: boolean; out BtnIdx: integer): boolean;
var
  i: integer;
begin
  result := false;
  BtnIdx := -1;
  if (csDesigning in ComponentState) or Focused then
    for i := ButtonCount-1 downto 0 do //nb: get higher indexed btn before lower
      if PtInRect(BtnRectFromPt(BtnPoints[i], fBtnSize),pt) and
        (not ignoreDisabled or IsValidBtnDown(i)) then
      begin
        BtnIdx := i;
        result := true;
        exit;
      end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.CMFontChanged(var Message: TMessage);
begin
  inherited;
  fBitmap.Canvas.Font.Assign(font);
  UpdateNeeded;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 1;
end;
//------------------------------------------------------------------------------

var
  ClickedInDesigner: boolean;

procedure TDrawObject.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  pt: TPoint;

  procedure NotifyModificationToDesigner;
  begin
    if Owner is TCustomForm then
      with TCustomForm(Owner) do
        if assigned(Designer) then Designer.Modified;
  end;

begin
  //respond to mouse events while in the form designer
  pt := SmallPointToPoint(Message.Pos);
  if (Message.Keys and MK_LBUTTON <> 0) then
  begin
    if not ClickedInDesigner and
      (fPressedBtnIdx < 0) and BtnIdxFromPt(pt, true, fPressedBtnIdx) then
    begin
      //button down
      invalidate;
      Message.Result := 1;
    end
    else if (fPressedBtnIdx >= 0) and (fPressedBtnIdx < ButtonCount) then
    begin
      //button moved
      if IsValidBtnMove(fPressedBtnIdx, pt) then
      InternalBtnMove(fPressedBtnIdx, pt);
      Message.Result := 1;
    end;
    ClickedInDesigner := true;
  end else
  begin
    //button up
    if fPressedBtnIdx >= 0 then
    begin
      NotifyModificationToDesigner;
      fPressedBtnIdx := -1;
      invalidate;
    end;
    ClickedInDesigner := false;
  end;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.SaveToPropStrings;
var
  i: integer;
  BtnsStr: string;

    procedure ConvertBinaryToText;
    const
      BytesPerLine = 32;
    var
      i: Integer;
      cnt: Longint;
      Buffer: array[0..BytesPerLine - 1] of Char;
      Text: array[0..BytesPerLine * 2 - 1] of Char;
      outStream: TMemoryStream;
    begin
      outStream := TMemoryStream.Create;
      try
        //compress the data ...
        fDataStream.Position := 0;
        with TCompressionStream.Create(clDefault, outStream) do
        try
          CopyFrom(fDataStream, 0);
        finally
          Free; //must be freed to flush outStream
        end;
        //now write the data to PropStrings ...
        outStream.Position := 0;
        AddToPropStrings('Data', '{');
        cnt := outStream.Size;
        while cnt > 0 do
        begin
          if cnt >= 32 then i := 32 else i := cnt;
          outStream.ReadBuffer(Buffer, i);
          BinToHex(Buffer, Text, i);
          AddToPropStrings('', '  ' + copy( Text, 1,i *2));
          Dec(cnt, i);
        end;
        AddToPropStrings('', '}');
      finally
        outStream.Free;
      end;
    end;

begin
  PropStrings.Clear; //start afresh
  PropStrings.Add('['+ClassName + ':' + name+']');
  for i := 0 to ButtonCount - 1 do
    with BtnPoints[i] do BtnsStr := BtnsStr + format('(%d,%d) ',[x,y]);

  AddToPropStrings('ButtonCount', inttostr(ButtonCount));
  AddToPropStrings('ButtonSize', inttostr(ButtonSize));
  AddToPropStrings('BtnPoints', BtnsStr);
  AddToPropStrings('Color', '$'+inttohex(ColorToRGB(Color),8));
  AddToPropStrings('ColorShadow', '$'+inttohex(ColorToRGB(ColorShadow),8));
  AddToPropStrings('Height', inttostr(Height));
  AddToPropStrings('Left', inttostr(Left));
  AddToPropStrings('Pen.Color','$'+inttohex(ColorToRGB(Pen.Color),8));
  AddToPropStrings('Pen.Style', GetEnumProp(Pen, 'Style'));
  AddToPropStrings('Pen.Width', inttostr(Pen.Width));
  AddToPropStrings('ShadowSize', inttostr(ShadowSize));
  AddToPropStrings('Top', inttostr(Top));
  AddToPropStrings('Width', inttostr(Width));
  AddToPropStrings('UseHitTest', GetEnumProp(self, 'UseHitTest'));
  if fStreamID <> '' then AddToPropStrings('ObjId', fStreamID);
  if assigned(fDataStream) then ConvertBinaryToText;
  //append to this in descendant classes
end;
//------------------------------------------------------------------------------

procedure TDrawObject.AddToPropStrings(const PropName, PropVal: string);
begin
  if PropName = '' then //eg when adding multiline properties
    PropStrings.Add(PropVal) else
    PropStrings.Add(format('%s=%s', [PropName, PropVal]))
end;
//------------------------------------------------------------------------------

procedure TDrawObject.GetBinaryData(var lineIdx: integer);
const
  BufferSize = 4096;
var
  cnt: integer;
  Buffer: array[0..BufferSize-1] of Char;
  hexSrc: pchar;
  inStream: TMemoryStream;
  decompStream: TDecompressionStream;
begin
  //format (nb: contained lines all indented 2 spaces) -
  //Data={
  //  compressed-ascii-hex
  //  compressed-ascii-hex
  //}
  inc(lineIdx);
  if assigned(fDataStream) then //ie: if there's somewhere to store the data ...
  begin
    inStream := TMemoryStream.Create;
    try
      fDataStream.Clear;
      //copy binary data into temp 'inStream' ...
      while (lineIdx < PropStrings.Count) and (length(PropStrings[lineIdx]) > 4) do
      begin
        hexSrc := @PropStrings[lineIdx][3]; //ignore 2 prepended blanks
        cnt := HexToBin(hexSrc, Buffer, BufferSize);
        if cnt = 0 then exit; //buffer too small
        inStream.Write(Buffer, cnt);
        inc(lineIdx);
      end;
      if inStream.Size > 0 then
      begin
        //now decompress data to fDataStream ...
        inStream.Position := 0;
        decompStream := TDecompressionStream.Create(inStream);
        try
          cnt := decompStream.Read(Buffer, BufferSize);
          while cnt > 0 do
          begin
            fDataStream.WriteBuffer(Buffer, cnt);
            cnt := decompStream.Read(Buffer, BufferSize);
           end;
        finally
          DecompStream.Free;
        end;
        //finally notify descendant components that data was loaded ...
        BinaryDataLoaded;
      end;
    finally
      inStream.Free;
    end;
  end else
    //otherwise simply ignore this property ...
    while (lineIdx < PropStrings.Count) and (length(PropStrings[lineIdx]) > 4) do
      inc(lineIdx);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.BinaryDataLoaded;
begin
  //called by descendant objects
end;
//------------------------------------------------------------------------------

procedure TDrawObject.LoadFromPropStrings;
var
  i, lineIdx: integer;
  line, clsName, propName, propVal: string;
  PersistObj: TPersistent;
  ObjPropInfo: PObjPropInfo;

  procedure GetNameValuePair(str: string);
  var
    i: integer;
  begin
    i := Pos('=',str);
    propName := trim(copy(str,1,i-1));
    propVal  := TrimNonPrintChrs(copy(str,i+1, length(str)));
  end;

  procedure GetBtnPts(const strPts: string);
  var
    i, cp, ep, idx: integer;
    pt: TPoint;
  begin
    idx := 0;
    cp := 1;
    ep := length(strPts);
    while (cp < ep) and (idx < ButtonCount) do
    begin
      while (cp < ep) and (strPts[cp] <> '(') do inc(cp);
      inc(cp);
      i := cp;
      while (cp < ep) and (strPts[cp] <> ',') do inc(cp);
      pt.X := strtointdef(copy(strPts,i, cp-i),0);
      inc(cp);
      i := cp;
      while (cp < ep) and (strPts[cp] <> ')') do inc(cp);
      pt.Y := strtointdef(copy(strPts,i, cp-i),0);
      if (cp > ep) then exit;
      BtnPoints[idx] := pt;
      inc(idx);
    end;
  end;

begin
  if PropStrings.Count = 0 then exit;
  fBlockResize := true;
  try
    line := PropStrings[0];
    i := pos(':',line);
    if (line[1] <> '[') or (i = 0) then exit;
    line := copy(line,2,i-2);
    if not SameText(line, Classname) then exit; //ie double check

    lineIdx := 0;
    while (lineIdx < PropStrings.Count -1) do  //01-dec-05: -2(???) --> -1
    begin
      inc(lineIdx);
      line := PropStrings[lineIdx];
      if (line = '') then continue;
      GetNameValuePair(line);
      if propName = '' then continue;
      i := pos('.',propName);
      try
        if (i > 1) then //ie class property (eg Pen.Width) ...
        begin
          clsName := copy(propName,1,i-1);
          propName := copy(propName,i+1,255);
          if IsPublishedProp(self, clsName) and
            (PropType(self,clsName) = tkClass) then
          begin
            PersistObj := TPersistent(GetOrdProp(self,clsName));
            if assigned(PersistObj) then
              case PropType(PersistObj,propName) of
                tkEnumeration: SetEnumProp(PersistObj, propName, propVal);
                tkInteger: SetPropValue(PersistObj, propName, strtoint(propVal));
                tkLString: SetPropValue(PersistObj, propName, propVal);
                tkSet: SetSetProp(PersistObj, propName, propVal);
              end;
          end;
        end
        else if IsPublishedProp(self, propName) then
        begin
          case PropType(self,propName) of
            tkEnumeration: SetEnumProp(self, propName, propVal);
            tkInteger: SetPropValue(self, propName, strtoint(propVal));
            tkLString: SetPropValue(self, propName, propVal);
            tkSet: SetSetProp(self, propName, propVal);
            tkClass:
              begin
                if assigned(ObjPropertyList) and
                  GetObjectPropClass(Self, PropName).InheritsFrom(TDrawObject) then
                begin
                  //ie: a link to another DrawObject (eg a Connection object)
                  //add this link to ObjPropertyList to fix up later ...
                  new(ObjPropInfo);
                  ObjPropertyList.Add(ObjPropInfo);
                  ObjPropInfo.fOwner := self;
                  ObjPropInfo.fProperty := PropName;
                  ObjPropInfo.fPropVal := PropVal; //ie streamed obj name
                end
                else if (TPersistent(GetOrdProp(self,propName)) is TStrings) then
                  GetStrings(PropStrings, TStrings(GetOrdProp(Self,PropName)), lineIdx);
              end;
          end;
        end else if SameText(propName, 'BtnPoints') then GetBtnPts(propVal)
        else if SameText(propName, 'Data') then GetBinaryData(lineIdx)
        else if SameText(propName, 'ObjId') then fStreamID := propVal;
      except
        continue;
      end;
    end;
  finally
    fBlockResize := false;
    fResizeNeeded := false;
  end;
  fBitmap.Width := width;
  fBitmap.Height := height;
  UpdateNeeded;
  DoSaveInfo;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.BeginTransform;
begin
  DoSaveInfo;
  SavedInfo.isTransforming := true;
  SavedInfo.RotateScreenPt := ClientToScreen(ObjectMidPoint);
end;
//------------------------------------------------------------------------------

procedure TDrawObject.EndTransform;
begin
  SavedInfo.isTransforming := false;
  DoSaveInfo;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.Rotate(degrees: integer);
var
  i, dx, dy: integer;
  radians: single;
  pt: TPoint;
begin
  degrees := degrees mod 360;
  if degrees < 0 then inc(degrees, 360);
  radians := degrees *pi / 180;
  with SavedInfo do
  begin
    if not isTransforming then
      Raise Exception.Create('Error: Rotate() without BeginTransform().');

    if not assigned(Parent) then
      Raise Exception.Create('Error: Rotate() without assigned Parent.');

    AngleInDegrees := degrees;

    dx := SavedLeft - left;
    dy := SavedTop - top;
    pt := ScreenToClient(RotateScreenPt);
    OffsetPt(pt, -dx, -dy);

    for i := 0 to ButtonCount-1 do
    begin
      BtnPoints[i].X := SavedPts[i].X;
      BtnPoints[i].Y := SavedPts[i].Y;
    end;
    RotatePts(BtnPoints, 0, pt, radians);
    OffsetBtns(dx, dy);
  end;
  ResizeNeeded;
end;
//------------------------------------------------------------------------------

procedure TDrawObject.Zoom(percent: integer);
var
  l,t,w,h: integer;
begin
  if (percent < 0) or (percent > 1000) then exit;
  with SavedInfo do
  begin
    if not isTransforming then
      Raise Exception.Create('Error: Zoom() without BeginTransform().');

    ZoomAsPercent := percent;
    
    w := SavedWidth * percent div 100;
    h := SavedHeight * percent div 100;
    l := SavedLeft + (SavedWidth div 2) - (w div 2);
    t := SavedTop + (SavedHeight div 2) - (h div 2);
  end;
  setbounds(l,t,w,h);
end;


//------------------------------------------------------------------------------
// TSolid methods
//------------------------------------------------------------------------------

constructor TSolid.Create(AOwner: TComponent);
begin
  inherited;
  fCanConnect := true;
end;
//------------------------------------------------------------------------------

destructor TSolid.Destroy;
begin
  if assigned(fConnectorList) then FreeAndNil(fConnectorList);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSolid.AddConnector(Connector: TConnector);
begin
  if not assigned(fConnectorList) then fConnectorList := TList.Create;
  if fConnectorList.IndexOf(Connector) < 0 then
    fConnectorList.Add(Connector);
end;
//------------------------------------------------------------------------------

procedure TSolid.RemoveConnector(Connector: TConnector);
var
  idx: integer;
begin
  if not assigned(fConnectorList) then exit;
  idx := fConnectorList.IndexOf(Connector);
  if idx >= 0 then fConnectorList.Delete(idx);
end;
//------------------------------------------------------------------------------

procedure TSolid.Resize;
var
  i: integer;
begin
  inherited;
  if not assigned(fConnectorList) then exit;
  for i := 0 to fConnectorList.Count -1 do
    TConnector(fConnectorList[i]).UpdateConnectionPoints(self);
end;
//------------------------------------------------------------------------------

// ClosestPoint: useful for finding the point where a line can 'join' an object.
// Override this method in descendant classes (uses screen coordinates)
// nb: although a button point is returned in this default method, a button
// point is certainly not necessary (nor often appropriate).
function TSolid.ClosestScreenPt(FromScreenPt: TPoint): TPoint;
var
  FromPt, mp, tl, tr, bl, br: TPoint;
  angleToFromPt, angleToTopRtCnr: single;
  l,t,r,b: integer;
begin
  mp := ObjectMidpoint;
  if ButtonCount = 1 then //ie TSolidPoints
  begin
    result := ClientToScreen(mp);
    exit;
  end;
  FromPt := ScreenToClient(FromScreenPt);

  l := min(BtnPoints[0].X,BtnPoints[1].X);
  t := min(BtnPoints[0].Y,BtnPoints[1].Y);
  r := max(BtnPoints[0].X,BtnPoints[1].X);
  b := max(BtnPoints[0].Y,BtnPoints[1].Y);
  tl := Point(l,t);
  tr := Point(r,t);
  bl := Point(l,b);
  br := Point(r,b);

  //if the area = nil then return the midpoint...
  if PointsEqual(tr,mp) or PointsEqual(FromPt, mp) then
  begin
    result := ClientToScreen(mp);
    exit;
  end;

  angleToFromPt := GetAnglePt2FromPt1(mp,FromPt);
  angleToTopRtCnr := GetAnglePt2FromPt1(mp,tr);

  if (angleToFromPt < angleToTopRtCnr) or
    (angleToFromPt > PI_MUL2 - angleToTopRtCnr)  then
      IntersectionPoint(tr, br, mp, FromPt, result)
  else if (angleToFromPt < PI - angleToTopRtCnr) then
    IntersectionPoint(tl,tr,mp,FromPt,result)
  else if (angleToFromPt < angleToTopRtCnr + PI) then
    IntersectionPoint(tl,bl,mp,FromPt,result)
  else
    IntersectionPoint(bl,br,mp,FromPt,result);

  result := ClientToScreen(result);
end;
//------------------------------------------------------------------------------

function TSolid.QuadScreenPt(QuadConnect: TQuadConnection): TPoint;
var
  pt: TPoint;
begin
  case QuadConnect of
    qcRight: pt := Point(width, Height div 2);
    qcTop: pt := Point(width div 2, 0);
    qcLeft: pt := Point(0, Height div 2);
    else pt := Point(width div 2, Height);
  end;
  result := ClosestScreenPt(ClientToScreen(pt));
end;

//------------------------------------------------------------------------------
// TSolidWithText methods
//------------------------------------------------------------------------------

constructor TSolidWithText.Create(AOwner: TComponent);
begin
  inherited;
  fPadding := 4;
  fStrings := TStringList.Create;
  TStringList(fStrings).OnChange := StringsOnChange;
  ParentFont := true;
end;
//------------------------------------------------------------------------------

destructor TSolidWithText.Destroy;
begin
  fStrings.free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.SetPadding(padding: integer);
begin
  if fPadding = padding then exit;
  fPadding := padding;
  UpdateNeeded;
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.SetStrings(strings: TStrings);
begin
  fStrings.Assign(strings);
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.StringsOnChange(Sender: TObject);
begin
  UpdateNeeded;
end;
//------------------------------------------------------------------------------

function TSolidWithText.ResizeObjectToFitText: boolean;
begin
  //implement in descendant classes
  result := false;
end;
//------------------------------------------------------------------------------

function TSolidWithText.GetAngle: integer;
begin
  result := SavedInfo.AngleInDegrees;
  if result > 180 then dec(result, 360);
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.SetAngle(angle: integer);
begin
  angle := angle mod 360;
  if angle < 0 then inc(angle, 360);
  if angle = SavedInfo.AngleInDegrees then exit;
  BeginTransform;
  Rotate(angle);
  EndTransform;
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.RotatedTextAtPt(Canvas: TCanvas; x,y: integer; const s: string);
var
  lf: TLogFont;
  OldFontHdl,NewFontHdl: HFont;
  mp, pt: TPoint;
begin
  if s = '' then exit;
  with Canvas do
  begin
    if GetObject(Font.Handle, SizeOf(lf), @lf) = 0 then exit;
    lf.lfEscapement := -Angle * 10;
    lf.lfOrientation := -Angle * 10;
    lf.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    NewFontHdl := CreateFontIndirect(lf);
    OldFontHdl := selectObject(handle,NewFontHdl);
    mp := Point((BtnPoints[0].X + BtnPoints[1].X) div 2,
      (BtnPoints[0].Y + BtnPoints[1].Y) div 2);
    pt := RotatePt(Point(x,y), mp, (Angle * pi) / 180);
    TextOut(pt.X, pt.Y, s);
    selectObject(handle,OldFontHdl);
    DeleteObject(NewFontHdl);
  end;
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.Rotate(degrees: integer);
begin
  if not SavedInfo.isTransforming then
    Raise Exception.Create('Error: Rotate() without BeginTransform().');
  degrees := degrees mod 360;
  if degrees < 0 then inc(degrees, 360);
  if degrees = SavedInfo.AngleInDegrees then exit;
  SavedInfo.AngleInDegrees := degrees;
  ResizeNeeded;
  //actually rotate drawing buttons (ie indexes >1) in descendant classes
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.DrawBtn(BtnPt: TPoint; index: integer; Pressed, LastBtn: boolean);
begin
  //TSolidWithText objects (ie rectangles, diamonds, elipses) only need
  //2 visible design btns, but use extra buttons to aid drawing when rotated ...
  if index > 1 then exit;
  inherited;
end;
//------------------------------------------------------------------------------

function TSolidWithText.IsValidBtnDown(BtnIdx: integer): boolean;
begin
  //TSolidWithText objects (ie rectangles, diamonds, elipses) only need
  //2 visible design btns, but use extra buttons to aid drawing when rotated ...
  result := BtnIdx < 2;
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.InternalBtnMove(BtnIdx: integer; NewPt: TPoint);
begin
  //this method prevents 'flipping' text objects but also assumes that
  //TSolidWithText only ever have 2 visible design btns
  if BtnIdx = 0 then
  begin
    NewPt.X := min(NewPt.X, BtnPoints[1].X);
    NewPt.Y := min(NewPt.Y, BtnPoints[1].Y);
  end else
  begin
    NewPt.X := max(NewPt.X, BtnPoints[0].X);
    NewPt.Y := max(NewPt.Y, BtnPoints[0].Y);
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSolidWithText.SaveToPropStrings;
var
  i: integer;
begin
  inherited;
  if Strings.Count > 0 then
  begin
    AddToPropStrings('Strings', '{');
    for i := 0 to Strings.Count -1 do AddToPropStrings('', '  '+Strings[i]);
    AddToPropStrings('', '}');
  end;
  if Action <> '' then AddToPropStrings('Action', Action);
  AddToPropStrings('Padding', inttostr(Padding));
  AddToPropStrings('Font.Charset', inttostr(Font.Charset));
  AddToPropStrings('Font.Color', '$'+inttohex(ColorToRGB(Font.Color),8));
  AddToPropStrings('Font.Name', Font.Name);
  AddToPropStrings('Font.Size', inttostr(Font.Size));
  AddToPropStrings('Font.Style', GetSetProp(Font, 'Style', false));
  AddToPropStrings('Angle', GetEnumProp(self, 'Angle'));
end;

//------------------------------------------------------------------------------
// TBaseLine methods
//------------------------------------------------------------------------------

constructor TBaseLine.Create(AOwner: TComponent);
begin
  inherited;
  DistinctiveLastBtn := true;
end;
//------------------------------------------------------------------------------

procedure TBaseLine.SetArrow1(Arrow: boolean);
begin
  if fArrow1 = Arrow then exit;
  fArrow1 := Arrow;
  CalcMargin;
end;
//------------------------------------------------------------------------------

procedure TBaseLine.SetArrow2(Arrow: boolean);
begin
  if fArrow2 = Arrow then exit;
  fArrow2 := Arrow;
  CalcMargin;
end;
//------------------------------------------------------------------------------

procedure TBaseLine.CalcMargin;
var
  arrowSize: integer;
begin
  inherited;
  if not fArrow1 and not fArrow2 then exit;
  arrowSize := 5 + Pen.Width;
  if fMargin < arrowSize then fMargin := arrowSize;
  ResizeNeeded;
end;
//------------------------------------------------------------------------------

function CreatePenHandle(TemplatePen: TPen): HPen;
var
  PatternLength: integer;
  lbrush : LOGBRUSH;
  userstyle : array [0..5] of DWORD;
  gapLen, dotLen, dashLen : Integer;
begin
  with TemplatePen do
  begin
    lbrush.lbStyle := BS_SOLID;
    lbrush.lbColor := color;
    lbrush.lbHatch := 0;
    PatternLength := 0;
    gaplen := Width*3 div 2; //because rounded dots erode into gaps
    dotLen := Width;
    dashLen := Width*3;

    case Style of
      psSolid: ;
      psDash :
        begin
          PatternLength:=2;
          userstyle[0] := dashLen; userstyle[1] := gapLen;
        end;
      psDot:
        begin
          PatternLength:=2;
          userstyle[0] := dotLen; userstyle[1] := gapLen;
        end;
      psDashDot:
        begin
          PatternLength:=4;
          userstyle[0] := dashLen; userstyle[1] := gapLen;
          userstyle[2] := dotLen; userstyle[3] := gapLen;
        end;
      psDashDotDot:
        begin
          PatternLength:=6;
          userstyle[0] := dashLen; userstyle[1] := gapLen;
          userstyle[2] := dotLen; userstyle[3] := gapLen;
          userstyle[4] := dotLen; userstyle[5] := gapLen;
        end;
      psClear: ;
    end;

    Result := ExtCreatePen(
      PS_GEOMETRIC or PS_USERSTYLE or PS_ENDCAP_ROUND or PS_JOIN_ROUND,
      Width, lbrush, PatternLength, @userstyle[0]);
  end;
end;
//------------------------------------------------------------------------------

procedure DrawDottedPolyline(Canvas: TCanvas; pts: array of TPoint);
var
  PenHdl, OldPenHdl: HPen;
begin
  //Canvas.Brush.Style := bsClear; //don't think this is necessary
  if (Canvas.Pen.Width = 1) or (Canvas.Pen.Style = psSolid) then
    Canvas.PolyLine(pts)
  else
  begin
    PenHdl := CreatePenHandle(canvas.Pen);
    OldPenHdl := SelectObject(canvas.Handle,PenHdl);
    Polyline(canvas.Handle, pts, high(pts)+1);
    DeleteObject(SelectObject(canvas.Handle, OldPenHdl));
  end;
end;
//------------------------------------------------------------------------------

function TBaseLine.GetButtonCount: integer;
begin
  result := inherited ButtonCount;
end;
//------------------------------------------------------------------------------

procedure TBaseLine.SetButtonCount(count: integer);
begin
  //override in descendant classes
end;
//------------------------------------------------------------------------------

function TBaseLine.Grow(TopEnd: boolean): boolean;
begin
  result := false; //override in descendant classes
end;
//------------------------------------------------------------------------------

function TBaseLine.Shrink(TopEnd: boolean): boolean;
begin
  result := false; //override in descendant classes
end;
//------------------------------------------------------------------------------

procedure TBaseLine.SaveToPropStrings;
begin
  inherited;
  if fArrow1 then AddToPropStrings('Arrow1', GetEnumProp(self, 'Arrow1'));
  if fArrow2 then AddToPropStrings('Arrow2', GetEnumProp(self, 'Arrow2'));
end;

//------------------------------------------------------------------------------
// TConnector methods
//------------------------------------------------------------------------------

constructor TConnector.Create(AOwner: TComponent);
begin
  inherited;
  fAutoOrientation := false;
end;
//------------------------------------------------------------------------------

destructor TConnector.Destroy;
begin
  //'unregister' any connections ...
  SetConnection1(nil);
  SetConnection2(nil);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TConnector.SetOrientation(orientation: TOrientation);
begin
  if fOrientation = orientation then exit;
  fOrientation := orientation;
  if not fBlockResize and IsConnected then //ie when loading from *.dob file
    UpdateConnectionPoints(nil);
  UpdateNeeded;
end;
//------------------------------------------------------------------------------

procedure TConnector.SetAutoOrientation(value: boolean);
begin
  if fAutoOrientation = value then exit;
  fAutoOrientation := value;
  if not fBlockResize and IsConnected then //ie when loading from *.dob file
    UpdateConnectionPoints(nil);
end;
//------------------------------------------------------------------------------

procedure TConnector.Notification(AComponent : TComponent;
  Operation : TOperation);
begin
  inherited;
  if (Operation <> opRemove) or not (AComponent is TSolid) then exit;
  if (AComponent = fConnection1) then SetConnection1(nil)
  else if (AComponent = fConnection2) then SetConnection2(nil);
end;
//------------------------------------------------------------------------------

function TConnector.GetCanMove: boolean;
begin
  result := focused and not IsConnected;
end;
//------------------------------------------------------------------------------

procedure TConnector.Resize;
begin
  inherited;
  if not fBlockResize and IsConnected then
    UpdateConnectionPoints(nil);
end;
//------------------------------------------------------------------------------


procedure TConnector.SetConnection1(Connection: TSolid);
begin
  if (fConnection1 = Connection) or
    //don't allow both connection objects to be the same object ...
    ((Connection <> nil) and (fConnection2 = Connection)) then exit;

  if assigned(fConnection1) then
  begin
    fConnection1.RemoveFreeNotification(Self);
    fConnection1.RemoveConnector(self);
  end;
  fConnection1 := nil;
  if assigned(Connection) and Connection.CanConnect then
  begin
    fConnection1 := Connection;
    Connection.FreeNotification(Self);
    Connection.AddConnector(self);
    if not fBlockResize then //ie when loading from *.dob file
      UpdateConnectionPoints(Connection);
  end;
end;
//------------------------------------------------------------------------------

procedure TConnector.SetConnection2(Connection: TSolid);
begin
  if (fConnection2 = Connection) or
    //don't allow both connection objects to be the same object ...
    ((Connection <> nil) and (fConnection1 = Connection)) then exit;

  if assigned(fConnection2) then
  begin
    fConnection2.RemoveFreeNotification(Self);
    fConnection2.RemoveConnector(self);
  end;
  fConnection2 := nil;
  if assigned(Connection) and Connection.CanConnect then
  begin
    fConnection2 := Connection;
    Connection.FreeNotification(Self);
    Connection.AddConnector(self);
    if not fBlockResize then //ie when loading from *.dob file
      UpdateConnectionPoints(Connection);
  end;
end;
//------------------------------------------------------------------------------

procedure TConnector.UpdateConnectionPoints(MovingConnection: TSolid);
var
  pt: TPoint;
begin
  //nb: Self.parent needed for ClientToScreen().
  //    (Would normally only get here without a parent while loading.)
  if not assigned(Parent) then exit;

  //make sure connection parents are assigned otherwise quit ...
  if (assigned(fConnection1) and not assigned(fConnection1.Parent)) or
     (assigned(fConnection2) and not assigned(fConnection2.Parent)) then exit;

  if fQuadPtConnect then
    DoQuadPtConnection
  else if assigned(fConnection1) and assigned(fConnection2) then
  begin
    with fConnection2 do
      pt := fConnection1.ClosestScreenPt(ClientToScreen(ObjectMidPoint));
    BtnPoints[0] := ScreenToClient(pt);
    with fConnection1 do
      pt := fConnection2.ClosestScreenPt(ClientToScreen(ObjectMidPoint));
    BtnPoints[ButtonCount-1] := ScreenToClient(pt);
  end
  else if assigned(fConnection1) then
  begin
    pt := fConnection1.ClosestScreenPt(ClientToScreen(BtnPoints[ButtonCount-1]));
    BtnPoints[0] := ScreenToClient(pt);
  end else if assigned(fConnection2) then
  begin
    pt := fConnection2.ClosestScreenPt(ClientToScreen(BtnPoints[0]));
    BtnPoints[ButtonCount-1] := ScreenToClient(pt);
  end;
  UpdateNonEndButtonsAfterBtnMove;
  ResizeNeeded;
end;
//------------------------------------------------------------------------------

procedure TConnector.DoQuadPtConnection;
var
  ScreenMp1, ScreenMp2: TPoint;
  HorzSlope: boolean;
begin
  if assigned(Connection1) then
    ScreenMp1 := Connection1.ClientToScreen(Connection1.ObjectMidPoint) else
  if assigned(Connection2) then
    ScreenMp2 := Connection2.ClientToScreen(Connection2.ObjectMidPoint) else
    ScreenMp2 := ClientToScreen(BtnPoints[ButtonCount-1]);

  HorzSlope := SlopeLessThanOne(ScreenMp1, ScreenMp2);

  //finally move ends to appropriate connection points ...
  if HorzSlope then
  begin
    if ScreenMp1.X < ScreenMp2.X then
    begin
      if assigned(fConnection1) then BtnPoints[0] :=
        ScreenToClient(fConnection1.QuadScreenPt(qcRight));
      if assigned(fConnection2) then BtnPoints[ButtonCount-1] :=
        ScreenToClient(fConnection2.QuadScreenPt(qcLeft));
    end else
    begin
      if assigned(fConnection1) then BtnPoints[0] :=
        ScreenToClient(fConnection1.QuadScreenPt(qcLeft));
      if assigned(fConnection2) then BtnPoints[ButtonCount-1] :=
        ScreenToClient(fConnection2.QuadScreenPt(qcRight));
    end;
  end else
  begin
    if ScreenMp1.Y < ScreenMp2.Y then
    begin
      if assigned(fConnection1) then BtnPoints[0] :=
        ScreenToClient(fConnection1.QuadScreenPt(qcBottom));
      if assigned(fConnection2) then BtnPoints[ButtonCount-1] :=
        ScreenToClient(fConnection2.QuadScreenPt(qcTop));
    end else
    begin
      if assigned(fConnection1) then BtnPoints[0] :=
        ScreenToClient(fConnection1.QuadScreenPt(qcTop));
      if assigned(fConnection2) then BtnPoints[ButtonCount-1] :=
        ScreenToClient(fConnection2.QuadScreenPt(qcBottom));
    end;
  end;
end;
//------------------------------------------------------------------------------

function TConnector.IsValidBtnDown(BtnIdx: integer): boolean;
begin
  //don't allow clicking of connected ends ...
  if ((BtnIdx = 0) and assigned(fConnection1)) or
      ((BtnIdx = ButtonCount-1) and assigned(fConnection2)) then
    result := false else
    result := true;
end;
//------------------------------------------------------------------------------

procedure TConnector.UpdateNonEndButtonsAfterBtnMove;
begin
  //override when needed in descendant classes
end;
//------------------------------------------------------------------------------

procedure TConnector.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Moving and IsConnected then
    //should never happen, but all the same - do nothing
  else if (fPressedBtnIdx < 0) then
    //do nothing
  else if (fPressedBtnIdx = 1) and assigned(fConnection1) then
    UpdateConnectionPoints(fConnection1)
  else if (fPressedBtnIdx = ButtonCount -2) and assigned(fConnection2) then
    UpdateConnectionPoints(fConnection2);
end;
//------------------------------------------------------------------------------

procedure TConnector.SaveToPropStrings;
begin
  inherited;
  //include Connections in our custom streaming ...
  if assigned(fConnection1) then
    AddToPropStrings('Connection1', inttohex(longint(fConnection1), 8));
  if assigned(fConnection2) then
    AddToPropStrings('Connection2', inttohex(longint(fConnection2), 8));
end;
//------------------------------------------------------------------------------

procedure TConnector.Rotate(degrees: integer);
begin
  //don't rotate if connected ...
  if not IsConnected then inherited;
end;
//------------------------------------------------------------------------------

function TConnector.IsConnected: boolean;
begin
  result := assigned(Connection1) or assigned(Connection2);
end;
//------------------------------------------------------------------------------

end.
