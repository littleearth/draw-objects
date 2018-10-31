unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DrawObjects1, DrawObjects2, Menus, ComCtrls, Printers,
  Types, ClipBrd, ExtCtrls, ActnList, XPStyleActnCtrls, ActnMan, IniFiles,
  ExtDlgs, System.Actions, System.UITypes, Vcl.ToolWin;

type
  TDragListRec = record
    control: TControl;
    startPt: TPoint;
  end;

  PDragListRec = ^TDragListRec;

  TfrmDrawObjects = class(TForm)
    PopupMenuDiagram: TPopupMenu;
    NewRectangle1: TMenuItem;
    NewDiamond1: TMenuItem;
    NewEllipse1: TMenuItem;
    NewArc1: TMenuItem;
    NewPolygon1: TMenuItem;
    NewStar1: TMenuItem;
    NewSolidArrow1: TMenuItem;
    NewSolidBezier1: TMenuItem;
    N1: TMenuItem;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    N3: TMenuItem;
    Delete1: TMenuItem;
    DisableDesigning1: TMenuItem;
    StatusBar1: TStatusBar;
    ColorDialog1: TColorDialog;
    N4: TMenuItem;
    GrowLine1: TMenuItem;
    N5: TMenuItem;
    NewLine1: TMenuItem;
    NewBezier1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Rotate1: TMenuItem;
    NewPlainText1: TMenuItem;
    ScrollBox1: TScrollBox;
    N6: TMenuItem;
    Print1: TMenuItem;
    N7: TMenuItem;
    BringtoFront1: TMenuItem;
    SendtoBack1: TMenuItem;
    FontDialog1: TFontDialog;
    N9: TMenuItem;
    NewLLine1: TMenuItem;
    NewZLine1: TMenuItem;
    NewSolidPoint1: TMenuItem;
    SetBackgroundColor1: TMenuItem;
    ActionManager1: TActionManager;
    actRectangle: TAction;
    actDiamond: TAction;
    actEllipse: TAction;
    actPlainText: TAction;
    actArc: TAction;
    actPolygon: TAction;
    actStar: TAction;
    actArrow: TAction;
    actSolidBezier: TAction;
    actSolidPoint: TAction;
    actLine: TAction;
    actLLine: TAction;
    actZLine: TAction;
    actBezier: TAction;
    actBringToFront: TAction;
    actSendToBack: TAction;
    actProperties: TAction;
    Properties1: TMenuItem;
    actDelete: TAction;
    actCopy: TAction;
    actCopyAsBitmap: TAction;
    actPaste: TAction;
    actShrink: TAction;
    actRotate: TAction;
    Object1: TMenuItem;
    Properties2: TMenuItem;
    N10: TMenuItem;
    DeleteObjects1: TMenuItem;
    N11: TMenuItem;
    CopyFocusedObjects1: TMenuItem;
    CopyAllObjectsasImage1: TMenuItem;
    PasteObjects1: TMenuItem;
    N12: TMenuItem;
    GrowLine2: TMenuItem;
    Rotate2: TMenuItem;
    actPrint: TAction;
    actBackground: TAction;
    actSaveAs: TAction;
    actOpen: TAction;
    actCopyAsMetafile: TAction;
    CopyAllObjectsasMetafile1: TMenuItem;
    Configure1: TMenuItem;
    DefaultLineProperties1: TMenuItem;
    DefaultSolidProperties1: TMenuItem;
    N13: TMenuItem;
    New1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    actPic: TAction;
    NewBitmapPic1: TMenuItem;
    actCut: TAction;
    CutFocusedObjects2: TMenuItem;
    SplitPolyButton1: TMenuItem;
    DeletePolyButton1: TMenuItem;
    actGrowTop: TAction;
    GrowLinefromTop1: TMenuItem;
    GrowLinefromTop2: TMenuItem;
    actShrinkTop: TAction;
    actGrow: TAction;
    ShrinkLinefromTop1: TMenuItem;
    ShrinkLinefromBottom1: TMenuItem;
    ShrinkLinefromTop2: TMenuItem;
    ShrinkLinefromBottom2: TMenuItem;
    actFlip: TAction;
    FlipHorizontally1: TMenuItem;
    N8: TMenuItem;
    UseTextDialogPrompt1: TMenuItem;
    actTextBezier: TAction;
    NewTextBezier1: TMenuItem;
    actSave: TAction;
    Save2: TMenuItem;
    actSavePicToBMP: TAction;
    SavePictureDialog1: TSavePictureDialog;
    SavePictoBMPfile1: TMenuItem;
    PrintDialog1: TPrintDialog;
    UseHitTest1: TMenuItem;
    actAlignLeft: TAction;
    actAlignTop: TAction;
    actAlignRight: TAction;
    actAlignBottom: TAction;
    N14: TMenuItem;
    AlignLeft1: TMenuItem;
    AlignTop1: TMenuItem;
    AlignRight1: TMenuItem;
    AlignBottom1: TMenuItem;
    Align1: TMenuItem;
    Spacing1: TMenuItem;
    actSpaceHorz: TAction;
    actSpaceVert: TAction;
    SpaceEquallyHorizontally1: TMenuItem;
    SpaceEquallyVertically1: TMenuItem;
    actEqualWidths: TAction;
    actEqualHeights: TAction;
    Sizing1: TMenuItem;
    EqualWidths1: TMenuItem;
    EqualHeights1: TMenuItem;
    PaintBox1: TPaintBox;
    actGrid: TAction;
    ShowGrid1: TMenuItem;
    N15: TMenuItem;
    actSnapToGrid: TAction;
    SnaptoGrid1: TMenuItem;
    actAlignCenterHorz: TAction;
    actAlignCenterVert: TAction;
    AlignCenterHorzontally1: TMenuItem;
    AlignCenterHorzontally2: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    toolButtonNew: TToolButton;
    PopupMenuSave: TPopupMenu;
    SaveAs1: TMenuItem;
    PopupMenuNew: TPopupMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    procedure NewSolidBezier1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure DisableDesigning1Click(Sender: TObject);
    procedure GrowLine1Click(Sender: TObject);
    procedure actSaveAsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actOpenClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Rotate1Click(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure actPrintClick(Sender: TObject);
    procedure CopyObject1Click(Sender: TObject);
    procedure PasteObject1Click(Sender: TObject);
    procedure CopyAsBitmapClick(Sender: TObject);
    procedure BringtoFront1Click(Sender: TObject);
    procedure SendtoBack1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure ScrollBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ScrollBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actBackgroundClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure actCopyAsMetafileClick(Sender: TObject);
    procedure DefaultLineProperties1Click(Sender: TObject);
    procedure DefaultSolidProperties1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actCutExecute(Sender: TObject);
    procedure SplitPolyButton1Click(Sender: TObject);
    procedure DeletePolyButton1Click(Sender: TObject);
    procedure ShrinkLine1Click(Sender: TObject);
    procedure actFlipExecute(Sender: TObject);
    procedure UseTextDialogPrompt1Click(Sender: TObject);
    procedure actSaveClick(Sender: TObject);
    procedure actSavePicToBMPClick(Sender: TObject);
    procedure UseHitTest1Click(Sender: TObject);
    procedure actAlignLeftClick(Sender: TObject);
    procedure actSpaceHorzClick(Sender: TObject);
    procedure actEqualWidthsClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure actGridClick(Sender: TObject);
    procedure actSnapToGridClick(Sender: TObject);
    procedure ActionManager1Update(Action: TBasicAction; var Handled: Boolean);
    procedure toolButtonNewClick(Sender: TObject);
  private
    popupPt: TPoint;
    tmpLine: TLine;
    tmpSolid: TRectangle;
    fPastingFromClipboard: Boolean;
    fCustomColors: TStringList;
    DragList: TList;
    startDragPt: TPoint;
    procedure DrawFocusRec(Rec: TRect);
    procedure ClearAllDrawObjFocuses;
    function ScrollboxHasDrawObjects: Boolean;
    procedure DrawObjMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawObjMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DrawObjMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawObjDblClick(Sender: TObject);
    procedure CountFocusedDrawObjs(out cnt: Integer; out last: TDrawObject);
    function GetSolidObjFromScreenPt(pt: TPoint): TSolid;
    procedure DrawObjLoaded(Sender: TObject);
    procedure ObjectInspectorClose(Sender: TObject);
    procedure LoadIniSettings;
    procedure SaveIniSettings;
    procedure OpenObjects(const Filename: string);
    procedure SaveObjects(const Filename: string);
    // dragging stuff ...
    procedure ClearDragList;
    procedure AddControlToDragList(control: TControl);
    function DragListObj(index: Integer): PDragListRec;
  public
    { Public declarations }
  end;

var
  frmDrawObjects: TfrmDrawObjects;

  CF_DRAWOBJECTS: Cardinal;
  GridSize: Integer = 8;

implementation

uses rotation, objInspector, preview;

{$R *.dfm}
// ------------------------------------------------------------------------------
// Miscellaneous functions ...
// ------------------------------------------------------------------------------

function min(val1, val2: Integer): Integer;
begin
  if val2 < val1 then
    result := val2
  else
    result := val1;
end;
// ------------------------------------------------------------------------------

function max(val1, val2: Integer): Integer;
begin
  if val2 > val1 then
    result := val2
  else
    result := val1;
end;
// ------------------------------------------------------------------------------

function ScreenToPrinterX(pxl: Integer): Integer;
begin
  result := round(pxl * GetDeviceCaps(Printer.Handle, LOGPIXELSX) /
    screen.PixelsPerInch);
end;
// ------------------------------------------------------------------------------

function ScreenToPrinterY(pxl: Integer): Integer;
begin
  result := round(pxl * GetDeviceCaps(Printer.Handle, LOGPIXELSY) /
    screen.PixelsPerInch);
end;
// ------------------------------------------------------------------------------

function MakeDarker(color: TColor): TColor;
var
  r, g, b: byte;
begin
  color := ColorToRGB(color);
  b := (color shr 16) and $FF;
  g := (color shr 8) and $FF;
  r := (color and $FF);
  b := b * 31 div 32;
  g := g * 31 div 32;
  r := r * 31 div 32;
  result := (b shl 16) or (g shl 8) or r;
end;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.FormCreate(Sender: TObject);
begin
  DragList := TList.create;
  fCustomColors := TStringList.create;

  SaveDialog1.InitialDir := ExtractFilePath(paramstr(0));
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
  OpenPictureDialog1.InitialDir := ExtractFilePath(paramstr(0));
  SavePictureDialog1.InitialDir := ExtractFilePath(paramstr(0));
  fCustomColors.Add('ColorA=' + inttohex(ColorToRGB(color), 6));
  fCustomColors.Add('ColorB=' + inttohex(ColorToRGB(clCream), 6));
  fCustomColors.Add('ColorC=' + inttohex(ColorToRGB(clMoneyGreen), 6));
  fCustomColors.Add('ColorD=' + inttohex(ColorToRGB(clSkyBlue), 6));

  ColorDialog1.CustomColors.Assign(fCustomColors);

  tmpLine := TLine.create(self);
  tmpLine.Name := 'DefaultLine';
  tmpLine.CanFocus := false;
  tmpLine.Arrow2 := true;

  tmpSolid := TRectangle.create(self);
  tmpSolid.Name := 'DefaultSolid';
  tmpSolid.Strings.Text := 'DefaultSolid';
  tmpSolid.CanFocus := false;

  LoadIniSettings;
  CF_DRAWOBJECTS := RegisterClipboardFormat('DrawObjects Format');

  PaintBox1.Canvas.Pen.color := MakeDarker(color);

  // the following line is important to prevent flickering whenever
  // moving or adjusting drawObject controls...
  ScrollBox1.DoubleBuffered := true;

  // just in case a user wants to associate *.dob files with DrawObjects ...
  if ParamCount > 0 then
    OpenObjects(paramstr(1));
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.FormDestroy(Sender: TObject);
begin
  ClearDragList;
  DragList.Free;
  fCustomColors.Free;
  SaveIniSettings;
  tmpLine.Free;
  tmpSolid.Free;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.LoadIniSettings;
var
  l, t, w, h: Integer;
begin
  with TIniFile.create(changeFileExt(paramstr(0), '.ini')) do
    try
      // mainform position ...
      l := ReadInteger('MainWinPos', 'Left', MaxInt);
      t := ReadInteger('MainWinPos', 'Top', MaxInt);
      w := ReadInteger('MainWinPos', 'Width', 100);
      h := ReadInteger('MainWinPos', 'Height', 100);
      if (l <> MaxInt) and (t <> MaxInt) and (w > 100) and (h > 100) then
      begin
        if (l < 0) and (t < 0) then
          WindowState := wsMaximized
        else
        begin
          Position := poDesigned;
          if l + w > screen.WorkAreaWidth then
            l := screen.WorkAreaWidth - w;
          if t + h > screen.WorkAreaHeight then
            t := screen.WorkAreaHeight - h;
          setbounds(l, t, w, h);
        end;
      end;

      tmpLine.color := TColor(ReadInteger('DefLine', 'Color', $FFFFFF));
      // ie filled beziers

      tmpLine.ShadowSize := ReadInteger('DefLine', 'ShadowSize', 2);
      tmpLine.ColorShadow := TColor(ReadInteger('DefLine', 'ShadowColor',
        $C0C0C0));

      tmpLine.Pen.color := TColor(ReadInteger('DefLine', 'PenColor', $000000));
      tmpLine.Pen.Width := ReadInteger('DefLine', 'PenWidth',
        tmpLine.Pen.Width);
      tmpLine.Pen.Style :=
        TPenStyle(byte(ReadInteger('DefLine', 'PenStyle', 0)));

      tmpSolid.Font.Name := ReadString('DefSolid', 'FontName', Font.Name);
      tmpSolid.Font.Size := ReadInteger('DefSolid', 'FontSize', Font.Size);
      tmpSolid.Font.Style :=
        TFontStyles(byte(ReadInteger('DefSolid', 'FontStyle', 1)));
      tmpSolid.color := TColor(ReadInteger('DefSolid', 'Color', $EEEEEE));
      tmpSolid.ShadowSize := ReadInteger('DefSolid', 'ShadowSize', 2);
      tmpSolid.ColorShadow := TColor(ReadInteger('DefSolid', 'ShadowColor',
        $C0C0C0));

      tmpSolid.Pen.color := TColor(ReadInteger('DefSolid', 'PenColor',
        $000000));
      tmpSolid.Pen.Width := ReadInteger('DefSolid', 'PenWidth',
        tmpLine.Pen.Width);
      tmpSolid.Pen.Style :=
        TPenStyle(byte(ReadInteger('DefSolid', 'PenStyle', 0)));

      color := TColor(ReadInteger('Background', 'Color', Integer(color)));

      UseTextDialogPrompt1.Checked := ReadBool('Setup', 'TestPrompt', false);
      UseHitTest1.Checked := ReadBool('Setup', 'UseHitTest', true);

      actGrid.Checked := ReadBool('Setup', 'ShowGrid', true);
      if actGrid.Checked then
        PaintBox1.Visible := true;

    finally
      Free;
    end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.SaveIniSettings;
begin
  with TIniFile.create(changeFileExt(paramstr(0), '.ini')) do
    try
      WriteInteger('MainWinPos', 'Left', left);
      WriteInteger('MainWinPos', 'Top', top);
      WriteInteger('MainWinPos', 'Width', Width);
      WriteInteger('MainWinPos', 'Height', height);

      WriteInteger('DefLine', 'Color', Integer(tmpLine.color));

      WriteInteger('DefLine', 'ShadowSize', tmpLine.ShadowSize);
      WriteInteger('DefLine', 'ShadowColor', Integer(tmpLine.ColorShadow));

      WriteInteger('DefLine', 'PenColor', Integer(tmpLine.Pen.color));
      WriteInteger('DefLine', 'PenWidth', tmpLine.Pen.Width);
      WriteInteger('DefLine', 'PenStyle', byte(tmpLine.Pen.Style));

      WriteString('DefSolid', 'FontName', tmpSolid.Font.Name);
      WriteInteger('DefSolid', 'FontSize', tmpSolid.Font.Size);
      WriteInteger('DefSolid', 'FontStyle', byte(tmpSolid.Font.Style));
      WriteInteger('DefSolid', 'Color', Integer(tmpSolid.color));
      WriteInteger('DefSolid', 'ShadowSize', tmpSolid.ShadowSize);
      WriteInteger('DefSolid', 'ShadowColor', Integer(tmpSolid.ColorShadow));

      WriteInteger('DefSolid', 'PenColor', Integer(tmpSolid.Pen.color));
      WriteInteger('DefSolid', 'PenWidth', tmpLine.Pen.Width);
      WriteInteger('DefSolid', 'PenStyle', byte(tmpSolid.Pen.Style));

      WriteInteger('Background', 'Color', Integer(color));

      WriteBool('Setup', 'TestPrompt', UseTextDialogPrompt1.Checked);
      WriteBool('Setup', 'UseHitTest', UseHitTest1.Checked);
      WriteBool('Setup', 'ShowGrid', actGrid.Checked);

    finally
      Free;
    end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ClearAllDrawObjFocuses;
var
  i: Integer;
begin
  // hide design buttons & bounding rect for all TDrawObjects ...
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if Controls[i] is TDrawObject then
        TDrawObject(Controls[i]).Focused := false;
end;
// ------------------------------------------------------------------------------

function TfrmDrawObjects.ScrollboxHasDrawObjects: Boolean;
var
  i: Integer;
begin
  result := true;
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if Controls[i] is TDrawObject then
        exit;
  result := false;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.CountFocusedDrawObjs(out cnt: Integer;
  out last: TDrawObject);
var
  i: Integer;
begin
  // Count 'focused' TDrawObjects (ie with visible designer buttons).
  // nb: TGraphicControl descendants never get real 'focus' (ie they don't
  // respond directly to keyboard input).
  cnt := 0;
  last := nil;
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused then
      begin
        last := TDrawObject(Controls[i]);
        inc(cnt);
      end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ScrollBox1Click(Sender: TObject);
begin
  ClearAllDrawObjFocuses;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.DisableDesigning1Click(Sender: TObject);
var
  i: Integer;
begin
  DisableDesigning1.Checked := not DisableDesigning1.Checked;
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if Controls[i] is TDrawObject then
        TDrawObject(Controls[i]).CanFocus := not DisableDesigning1.Checked;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.NewSolidBezier1Click(Sender: TObject);
var
  drawObj: TDrawObject;
  str: string;
begin
  ClearAllDrawObjFocuses;

  // create a new DrawObject control ...
  if Sender = actRectangle then
    drawObj := TRectangle.create(self)
  else if Sender = actDiamond then
    drawObj := TDiamond.create(self)
  else if Sender = actEllipse then
    drawObj := TEllipse.create(self)
  else if Sender = actPlainText then
    drawObj := TText.create(self)
  else if Sender = actArc then
    drawObj := TArc.create(self)
  else if Sender = actPolygon then
    drawObj := TPolygon.create(self)
  else if Sender = actStar then
    drawObj := TStar.create(self)
  else if Sender = actArrow then
    drawObj := TSolidArrow.create(self)
  else if Sender = actTextBezier then
    drawObj := TTextBezier.create(self)
  else if Sender = actSolidBezier then
    drawObj := TSolidBezier.create(self)
  else if Sender = actLine then
    drawObj := TLine.create(self)
  else if Sender = actBezier then
    drawObj := TBezier.create(self)
  else if Sender = actLLine then
    drawObj := TLLine.create(self)
  else if Sender = actZLine then
    drawObj := TZLine.create(self)
  else if Sender = actSolidPoint then
    drawObj := TSolidPoint.create(self)
  else if (Sender = actPic) then
  begin
    if not OpenPictureDialog1.Execute then
      exit;
    drawObj := TPic.create(self);
    TPic(drawObj).LoadPicFromFile(OpenPictureDialog1.Filename);
  end
  else
    exit;

  MakeNameForControl(drawObj);
  drawObj.parent := ScrollBox1;
  DrawObjLoaded(drawObj);
  with ScreenToClient(popupPt) do
  begin
    drawObj.left := X;
    drawObj.top := Y;
  end;

  // assign the object with customizable 'default' properties ...
  if (drawObj is TSolidPoint) then
  begin
    drawObj.ShadowSize := tmpLine.ShadowSize;
    drawObj.ColorShadow := tmpLine.ColorShadow;
    drawObj.Pen.color := tmpLine.Pen.color;
  end
  else if (drawObj is TTextBezier) then
  begin
    drawObj.ColorShadow := tmpSolid.ColorShadow;
    drawObj.color := tmpSolid.color;
    TTextBezier(drawObj).Font.Assign(tmpSolid.Font);
    TTextBezier(drawObj).Font.Size := 26;
    TTextBezier(drawObj).Font.Style := [fsBold];
    drawObj.ShadowSize := tmpSolid.ShadowSize;
    if UseTextDialogPrompt1.Checked and InputQuery(application.Title,
      'Enter caption:', str) then
      TTextBezier(drawObj).Text := str;
  end
  else if (drawObj is TSolid) or (drawObj is TSolidBezier) then
  begin
    drawObj.color := tmpSolid.color;
    drawObj.ColorShadow := tmpSolid.ColorShadow;
    if drawObj is TSolidWithText then
    begin
      TSolidWithText(drawObj).Font.Assign(tmpSolid.Font);
      TSolidWithText(drawObj).Padding := tmpSolid.Padding;
    end;
    drawObj.Pen.Assign(tmpSolid.Pen);
    if (drawObj is TSolidBezier) then
      drawObj.Pen.Width := 20;
    drawObj.ShadowSize := tmpSolid.ShadowSize;

    if drawObj is TSolidWithText and UseTextDialogPrompt1.Checked and
      InputQuery(application.Title, 'Enter caption:', str) then
    begin
      TSolidWithText(drawObj).Padding := 0;
      TSolidWithText(drawObj).Strings.Text := str;
      TSolidWithText(drawObj).ResizeObjectToFitText;
    end;
  end
  else if (drawObj is TConnector) then
  begin
    drawObj.color := tmpLine.color; // ie 'filled' beziers
    drawObj.ColorShadow := tmpLine.ColorShadow;
    drawObj.Pen.Assign(tmpLine.Pen);
    drawObj.ShadowSize := tmpLine.ShadowSize;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.DrawObjLoaded(Sender: TObject);
begin
  with TDrawObject(Sender) do
  begin
    OnMouseDown := DrawObjMouseDown;
    OnMouseMove := DrawObjMouseMove;
    OnMouseUp := DrawObjMouseUp;
    OnDblClick := DrawObjDblClick;
    CanFocus := not DisableDesigning1.Checked;
    Focused := CanFocus;
    if Sender is TBaseLine then
      TBaseLine(Sender).UseHitTest := UseHitTest1.Checked;

    // if pasting from the clipboard, offset new objects slightly ...
    if fPastingFromClipboard then
    begin
      left := left + 10;
      top := top + 10;
    end;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.UseTextDialogPrompt1Click(Sender: TObject);
begin
  UseTextDialogPrompt1.Checked := not UseTextDialogPrompt1.Checked;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.UseHitTest1Click(Sender: TObject);
var
  i: Integer;
begin
  // 'UseHitTest' means that the object (not just somewhere within its bounding
  // rectangle) must be clicked to select that object. This is useful especially
  // with lines whose bounding rectangles typically overlap other objects making
  // it harder to select the objects underneath.
  UseHitTest1.Checked := not UseHitTest1.Checked;
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if (Controls[i] is TDrawObject) then
        TDrawObject(Controls[i]).UseHitTest := UseHitTest1.Checked;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.Delete1Click(Sender: TObject);
var
  i: Integer;
begin
  // delete TDrawObject controls that have focus ...
  with ScrollBox1 do
    for i := controlCount - 1 downto 0 do
      if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused then
        Controls[i].Free;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.BringtoFront1Click(Sender: TObject);
var
  cnt: Integer;
  drawObj: TDrawObject;
begin
  CountFocusedDrawObjs(cnt, drawObj);
  if (cnt = 1) then
    drawObj.BringToFront;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.SendtoBack1Click(Sender: TObject);
var
  cnt: Integer;
  drawObj: TDrawObject;
begin
  CountFocusedDrawObjs(cnt, drawObj);
  if (cnt = 1) then
    drawObj.SendToBack;
  // we don't want our drawingObjects behind the PaintBox grid ...
  PaintBox1.SendToBack;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actBackgroundClick(Sender: TObject);
begin
  ColorDialog1.color := color;
  if not ColorDialog1.Execute then
    exit;
  color := ColorDialog1.color;
  PaintBox1.Canvas.Pen.color := MakeDarker(color);
  fCustomColors.Assign(ColorDialog1.CustomColors);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actGridClick(Sender: TObject);
begin
  actGrid.Checked := not actGrid.Checked;
  PaintBox1.Visible := actGrid.Checked;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.CopyObject1Click(Sender: TObject);
var
  i: Integer;
  theList: TList;
  theStrings: TStringList;

  Data: THandle;
  DataPtr: Pointer;
  str: string;
begin
  theStrings := TStringList.create;
  theList := TList.create;
  try
    // copy all objects ...
    with ScrollBox1 do
      for i := 0 to controlCount - 1 do
        if Controls[i] is TDrawObject and TDrawObject(Controls[i]).Focused then
          theList.Add(Controls[i]);
    if theList.Count = 0 then
      exit;
    SaveDrawObjectsToStrings(theList, theStrings);

    // clipboard.AsText := theStrings.Text;
    // let's use a custom clipboard format instead ...
    str := theStrings.Text;
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, length(str) + 1);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(pchar(str)^, DataPtr^, length(str) + 1);
        clipboard.Open;
        try
          clipboard.clear;
          SetClipboardData(CF_DRAWOBJECTS, Data);
        finally
          clipboard.Close;
        end;
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
    end;

  finally
    theStrings.Free;
    theList.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actCutExecute(Sender: TObject);
begin
  CopyObject1Click(Sender);
  Delete1Click(Sender);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.CopyAsBitmapClick(Sender: TObject);
var
  i, l, t, w, h: Integer;
  bmp: TBitmap;
begin
  l := MaxInt;
  t := MaxInt;
  w := 0;
  h := 0;
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if Controls[i] is TDrawObject then
        with TDrawObject(Controls[i]) do
        begin
          l := min(l, left);
          t := min(t, top);
          w := max(w, left + Width);
          h := max(h, top + height);
        end;
  if (w = 0) or (h = 0) then
    exit;

  bmp := TBitmap.create;
  bmp.Width := w - l;
  bmp.height := h - t;
  bmp.Canvas.Brush.color := self.color;
  bmp.Canvas.FillRect(Rect(0, 0, w, h));
  try
    // we could use either the DrawObject.bitmap property or
    // the DrawObject.Draw() method here since no scaling is required ...
    with ScrollBox1 do
      for i := 0 to controlCount - 1 do
        if Controls[i] is TDrawObject then
          with TDrawObject(Controls[i]) do
            draw(bmp.Canvas, left - l, top - t);

    clipboard.SetAsHandle(CF_BITMAP, bmp.Handle);
  finally
    bmp.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actCopyAsMetafileClick(Sender: TObject);
var
  i, l, t, w, h: Integer;
  mf: TMetafile;
  mfc: TMetafileCanvas;
begin
  l := MaxInt;
  t := MaxInt;
  w := 0;
  h := 0;
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if Controls[i] is TDrawObject then
        with TDrawObject(Controls[i]) do
        begin
          l := min(l, left);
          t := min(t, top);
          w := max(w, left + Width);
          h := max(h, top + height);
        end;
  if (w = 0) or (h = 0) then
    exit;

  mf := TMetafile.create;
  try
    mf.Width := w - l;
    mf.height := h - t;
    mfc := TMetafileCanvas.create(mf, 0);
    try
      with ScrollBox1 do
        for i := 0 to controlCount - 1 do
          if (Controls[i] is TDrawObject) then
            with TDrawObject(Controls[i]) do
              draw(mfc, left - l, top - t);
    finally
      FreeAndNil(mfc);
    end;
    clipboard.SetAsHandle(CF_ENHMETAFILE, mf.Handle);
  finally
    mf.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.PasteObject1Click(Sender: TObject);
var
  theStrings: TStringList;
  Data: THandle;
  str: string;
begin
  if not clipboard.HasFormat(CF_DRAWOBJECTS) then
    exit;
  ClearAllDrawObjFocuses;

  fPastingFromClipboard := true;
  theStrings := TStringList.create;
  try

    // theStrings.Text := clipboard.AsText;
    str := '';
    clipboard.Open;
    try
      Data := GetClipboardData(CF_DRAWOBJECTS);
      if Data <> 0 then
        try
          str := pchar(GlobalLock(Data));
        finally
          GlobalUnlock(Data);
        end;
    finally
      clipboard.Close;
    end;
    theStrings.Text := str;

    LoadDrawObjectsFromStrings(theStrings, self, ScrollBox1, DrawObjLoaded);
  finally
    theStrings.Free;
    fPastingFromClipboard := false;
  end;

end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.GrowLine1Click(Sender: TObject);
var
  cnt: Integer;
  drawObj: TDrawObject;
begin
  CountFocusedDrawObjs(cnt, drawObj);
  if (cnt = 1) and (drawObj is TConnector) and
    not assigned(TConnector(drawObj).Connection2) then
    TConnector(drawObj).Grow(Sender = actGrowTop);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ShrinkLine1Click(Sender: TObject);
var
  cnt: Integer;
  drawObj: TDrawObject;
begin
  CountFocusedDrawObjs(cnt, drawObj);
  if (cnt = 1) and (drawObj is TConnector) and
    not assigned(TConnector(drawObj).Connection2) then
    TConnector(drawObj).Shrink(Sender = actShrinkTop);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.Rotate1Click(Sender: TObject);
var
  cnt: Integer;
begin
  with TRotationForm.create(self) do
    try
      // nb: RotationObject owned by TRotationForm
      CountFocusedDrawObjs(cnt, RotationObject);
      if (cnt <> 1) then
        exit;

      left := ScrollBox1.ClientOrigin.X + RotationObject.left +
        RotationObject.Width + 10;
      top := ScrollBox1.ClientOrigin.Y + RotationObject.top +
        RotationObject.height + 10;

      if RotationObject is TSolidWithText then
        TrackBar1.Position := TSolidWithText(RotationObject).angle
      else if RotationObject is TPic then
        TrackBar1.Position := TPic(RotationObject).angle;
      RotationObject.BeginTransform;
      // TRotationForm calls the DrawObject.Rotate() method passing the
      // rotation angle indicated by TRotationForm's slider position...
      ShowModal;
      RotationObject.EndTransform;
    finally
      Free;
    end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.SplitPolyButton1Click(Sender: TObject);
var
  i, cnt: Integer;
  drawObj: TDrawObject;
begin
  // split a polygon button into 2 buttons ...
  CountFocusedDrawObjs(cnt, drawObj);
  if not(cnt = 1) or not(drawObj is TPolygon) or (drawObj is TStar) then
    exit;

  with TPolygon(drawObj) do
    if BtnIdxFromPt(ScreenToClient(popupPt), true, i) then
      DuplicateButton(i);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.DeletePolyButton1Click(Sender: TObject);
var
  i, cnt: Integer;
  drawObj: TDrawObject;
begin
  // remove a polygon button ...
  CountFocusedDrawObjs(cnt, drawObj);
  if not(cnt = 1) or not(drawObj is TPolygon) or (drawObj is TStar) then
    exit;

  with TPolygon(drawObj) do
    if BtnIdxFromPt(ScreenToClient(popupPt), true, i) then
      RemoveButton(i);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actFlipExecute(Sender: TObject);
var
  cnt: Integer;
  drawObj: TDrawObject;
begin
  CountFocusedDrawObjs(cnt, drawObj);
  if (drawObj is TPolygon) then
    TPolygon(drawObj).Mirror
  else if (drawObj is TSolidBezier) then
    TSolidBezier(drawObj).Mirror;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actSavePicToBMPClick(Sender: TObject);
var
  cnt: Integer;
  drawObj: TDrawObject;
begin
  CountFocusedDrawObjs(cnt, drawObj);
  if (cnt = 1) and (drawObj is TPic) and SavePictureDialog1.Execute then
    TPic(drawObj).SavePicToFile(SavePictureDialog1.Filename);
end;
// ------------------------------------------------------------------------------

// Align selected controls ...
procedure TfrmDrawObjects.actAlignLeftClick(Sender: TObject);
var
  i, j: Integer;
begin
  j := -1;
  with ScrollBox1 do
  begin
    for i := 0 to controlCount - 1 do
      if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused then
      begin
        j := i;
        break;
      end;
    if j < 0 then
      exit;

    if Sender = actAlignLeft then
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].left := Controls[j].left
    end
    else if Sender = actAlignTop then
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].top := Controls[j].top
    end
    else if Sender = actAlignRight then
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].left := Controls[j].left + Controls[j].Width -
            Controls[i].Width;
    end
    else if Sender = actAlignBottom then
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].top := Controls[j].top + Controls[j].height -
            Controls[i].height
    end
    else if Sender = actAlignCenterHorz then
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].top := Controls[j].top + (Controls[j].height div 2) -
            (Controls[i].height div 2)
    end
    else if Sender = actAlignCenterVert then
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].left := Controls[j].left + (Controls[j].Width div 2) -
            (Controls[i].Width div 2)
    end;
  end;
end;
// ------------------------------------------------------------------------------

// Equally space selected controls ...
procedure TfrmDrawObjects.actSpaceHorzClick(Sender: TObject);
var
  objList: TStringList;
  i, k, spc: Integer;
  Rec: TRect;
begin
  // since the selected objects are likely NOT to have their Z-order the same as
  // the left-right (or top-bottom) order, we need to create a (sorted) list ...
  objList := TStringList.create;
  try
    with ScrollBox1 do
      for i := 0 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          objList.AddObject('', Controls[i]);
    if objList.Count < 2 then
      exit;

    if Sender = actSpaceHorz then
    begin
      with TControl(objList.Objects[0]) do
      begin
        Rec := BoundsRect;
        k := Width;
        objList[0] := format('%6.6d', [left]); // prepare to sort on Left edges
      end;
      for i := 1 to objList.Count - 1 do
        with TControl(objList.Objects[i]) do
        begin
          objList[i] := format('%6.6d', [left]);
          if left < Rec.left then
            Rec.left := left;
          if left + Width > Rec.Right then
            Rec.Right := left + Width;
          inc(k, Width);
        end;
      spc := (Rec.Right - Rec.left - k) div (objList.Count - 1);
      objList.Sort;
      for i := 1 to objList.Count - 1 do
        with TControl(objList.Objects[i - 1]) do
          TControl(objList.Objects[i]).left := left + Width + spc;
    end
    else // Sender = actSpaceVert
    begin
      with TControl(objList.Objects[0]) do
      begin
        Rec := BoundsRect;
        k := height;
        objList[0] := format('%6.6d', [top]); // prepare to sort on Top edges
      end;
      for i := 1 to objList.Count - 1 do
        with TControl(objList.Objects[i]) do
        begin
          objList[i] := format('%6.6d', [top]);
          if top < Rec.top then
            Rec.top := top;
          if top + height > Rec.Bottom then
            Rec.Bottom := top + height;
          inc(k, height);
        end;
      spc := (Rec.Bottom - Rec.top - k) div (objList.Count - 1);
      objList.Sort;
      for i := 1 to objList.Count - 1 do
        with TControl(objList.Objects[i - 1]) do
          TControl(objList.Objects[i]).top := top + height + spc;
    end;
  finally
    objList.Free;
  end;

end;
// ------------------------------------------------------------------------------

// Equally size selected controls ...
procedure TfrmDrawObjects.actEqualWidthsClick(Sender: TObject);
var
  i, j: Integer;
begin
  j := -1;
  with ScrollBox1 do
  begin
    for i := 0 to controlCount - 1 do
      if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused then
      begin
        j := i;
        break;
      end;
    if j < 0 then
      exit;

    if Sender = actEqualWidths then
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].Width := Controls[j].Width;
    end
    else
    begin
      for i := j + 1 to controlCount - 1 do
        if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused
        then
          Controls[i].height := Controls[j].height;
    end;
  end;
end;
// ------------------------------------------------------------------------------

// Paint Grid ...
procedure TfrmDrawObjects.PaintBox1Paint(Sender: TObject);
var
  i: Integer;
begin
  with PaintBox1 do
  begin
    for i := 0 to (Width div GridSize) do
    begin
      Canvas.MoveTo(i * GridSize, 0);
      Canvas.LineTo(i * GridSize, height);
    end;
    for i := 0 to (height div GridSize) do
    begin
      Canvas.MoveTo(0, i * GridSize);
      Canvas.LineTo(Width, i * GridSize);
    end;
  end;
end;
// ------------------------------------------------------------------------------

// Align to Grid the selected DrawObject's left and top ...
procedure TfrmDrawObjects.actSnapToGridClick(Sender: TObject);
var
  i, j, gsDiv2, pwDiv2: Integer;
begin
  if not PaintBox1.Visible then
    exit;

  gsDiv2 := GridSize div 2;
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused then
        with TDrawObject(Controls[i]) do
        begin
          pwDiv2 := Pen.Width div 2;

          j := (left + margin - pwDiv2) mod GridSize;
          if j <= gsDiv2 then
            left := left - j
          else
            left := left + GridSize - j;

          j := (top + margin - pwDiv2) mod GridSize;
          if j <= gsDiv2 then
            top := top - j
          else
            top := top + GridSize - j;
        end;
end;

// ------------------------------------------------------------------------------
// Object dragging methods ...
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ClearDragList;
var
  i: Integer;
begin
  for i := 0 to DragList.Count - 1 do
    dispose(PDragListRec(DragList[i]));
  DragList.clear;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.AddControlToDragList(control: TControl);
var
  DragListRec: PDragListRec;
begin
  if not assigned(control) then
    exit;
  New(DragListRec);
  DragList.Add(DragListRec);
  DragListRec.control := control;
  DragListRec.startPt := Point(control.left, control.top);
end;
// ------------------------------------------------------------------------------

function TfrmDrawObjects.DragListObj(index: Integer): PDragListRec;
begin
  if (index < 0) or (index >= DragList.Count) then
    result := nil
  else
    result := PDragListRec(DragList[index]);
end;
// ------------------------------------------------------------------------------

function ConnectorHasStuckEnd(connector: TConnector): Boolean;
begin
  with connector do
    result := (assigned(Connection1) and not Connection1.Focused) or
      (assigned(Connection2) and not Connection2.Focused);
end;
// ------------------------------------------------------------------------------

// Implement drag moving of multiple objects ...
procedure TfrmDrawObjects.DrawObjMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if not(ssShift in Shift) and not TDrawObject(Sender).Focused then
    ClearAllDrawObjFocuses;

  // prepare for possible drag moving ...
  ClearDragList;
  GetCursorPos(startDragPt);
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if (Controls[i] is TDrawObject) and TDrawObject(Controls[i]).Focused then
      begin
        // connectors with a fixed (non-moving) end are very tricky so ...
        if (Controls[i] is TConnector) and
          ConnectorHasStuckEnd(TConnector(Controls[i])) then
          continue;

        AddControlToDragList(Controls[i]);
      end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.DrawObjMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
  screenPt: TPoint;
begin
  if not(ssLeft in Shift) or (DragList.Count < 2) then
    exit;
  // drag move all focused objects ...
  GetCursorPos(screenPt);
  for i := 0 to DragList.Count - 1 do
    with DragListObj(i)^ do
      control.setbounds(startPt.X + (screenPt.X - startDragPt.X),
        startPt.Y + (screenPt.Y - startDragPt.Y), control.Width,
        control.height);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.DrawObjMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ClearDragList;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.FormDeactivate(Sender: TObject);
begin
  // because the 'Object Inspector' shows non-Modal we need to block
  // the mainform from responding to shortcut keys until it regains focus ...
  ActionManager1.State := asSuspended;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.FormActivate(Sender: TObject);
begin
  ActionManager1.State := asNormal;
end;
// ------------------------------------------------------------------------------

// Show the 'Object Inspector' to modify the properties of a DrawObject ...
procedure TfrmDrawObjects.Properties1Click(Sender: TObject);
var
  topLeftPt: TPoint;
  cnt: Integer;
  drawObj: TDrawObject;
begin
  CountFocusedDrawObjs(cnt, drawObj);
  if (cnt <> 1) then
    exit;

  // initialize and display the "object inspector" ...
  with TObjInspectForm.create(self) do
  begin
    ColorDialog1.CustomColors.Assign(fCustomColors);
    OnClosing := ObjectInspectorClose;

    // assign the object and list of properties to ignore ...
    if (drawObj is TSolidBezier) or (drawObj is TTextBezier) then
      AssignObj(drawObj, ['Arrow1', 'Arrow2', 'Action', 'ButtonSize',
        'CanFocus', 'CanMove', 'Connection1', 'Connection2', 'Cursor', 'Filled',
        'Focused', 'HelpContext', 'HelpKeyword', 'HelpType', 'Hint',
        'ParentFont', 'PopupMenu', 'Tag', 'UseHitTest'])
    else if drawObj is TConnector then
      AssignObj(drawObj, ['Action', 'ButtonSize', 'CanFocus', 'CanMove',
        'Cursor', 'Focused', 'HelpContext', 'HelpKeyword', 'HelpType', 'Hint',
        'ParentFont', 'PopupMenu', 'Tag', 'UseHitTest'])
    else
      AssignObj(drawObj, ['Action', 'ButtonSize', 'CanFocus', 'CanMove',
        'Cursor', 'Focused', 'HelpContext', 'HelpKeyword', 'HelpType', 'Hint',
        'ParentFont', 'PopupMenu', 'Tag']);

    // position the "object inspector" window ...
    with drawObj do
      topLeftPt := Point(left + Width + 10, top - 10);
    with self.ClientOrigin do
      OffsetPt(topLeftPt, X, Y);
    left := topLeftPt.X;
    top := topLeftPt.Y;
    if top + height > screen.WorkAreaHeight then
      top := screen.WorkAreaHeight - height;
    // now display it...
    Show;
    // nb: it'll free itself when it loses focus
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.DefaultLineProperties1Click(Sender: TObject);
var
  Rec: TRect;
begin
  // initialize and display the "object inspector" ...
  with TObjInspectForm.create(self) do
  begin
    // a bit of fancy stuff to display the default TLine in
    // the "object inspector" ...
    pnlBottom.height := 60;
    pnlBottom.ParentBackground := false; // themes workaround
    pnlBottom.color := self.color;
    Rec := pnlBottom.ClientRect;
    inflateRect(Rec, -20, -10);
    tmpLine.parent := pnlBottom;
    with Rec do
      tmpLine.setbounds(left, top, Right - left, Bottom - top);
    ColorDialog1.CustomColors.Assign(fCustomColors);
    OnClosing := ObjectInspectorClose;

    // assign the object and list of properties to ignore ...
    AssignObj(tmpLine, ['Action', 'Arrow1', 'Arrow2', 'ButtonSize',
      'ButtonCount', 'Canfocus', 'CanMove', 'Connection1', 'Connection2',
      'Cursor', 'Focused', 'Height', 'HelpContext', 'HelpKeyword', 'HelpType',
      'Hint', 'Left', 'Name', 'PopupMenu', 'Tag', 'Top', 'Width']);
    Position := poMainFormCenter;
    // now display it...
    Show;
    // nb: TObjInspectForm will free itself when it loses focus
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.DefaultSolidProperties1Click(Sender: TObject);
var
  Rec: TRect;
begin
  // initialize and display the "object inspector" ...
  with TObjInspectForm.create(self) do
  begin
    // a bit of fancy stuff to display the default TSolid in
    // the "object inspector" ...
    pnlBottom.height := 80;
    pnlBottom.ParentBackground := false; // themes workaround
    pnlBottom.color := self.color;
    Rec := pnlBottom.ClientRect;
    inflateRect(Rec, -10, -10);
    tmpSolid.parent := pnlBottom;
    with Rec do
      tmpSolid.setbounds(left, top, Right - left, Bottom - top);
    ColorDialog1.CustomColors.Assign(fCustomColors);
    OnClosing := ObjectInspectorClose;

    // assign the object and list of properties to ignore ...
    AssignObj(tmpSolid, ['Action', 'Angle', 'ButtonSize', 'Canfocus', 'CanMove',
      'Centered', 'Cursor', 'Focused', 'Height', 'HelpContext', 'HelpKeyword',
      'HelpType', 'Hint', 'Left', 'Name', 'ParentFont', 'PopupMenu', 'Rounded',
      'Strings', 'Tag', 'Top', 'Width']);
    Position := poMainFormCenter;

    // now display it...
    Show;
    // nb: TObjInspectForm will free itself when it loses focus
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ObjectInspectorClose(Sender: TObject);
begin
  tmpLine.parent := nil;
  tmpSolid.parent := nil;
  with TObjInspectForm(Sender) do
    fCustomColors.Assign(ColorDialog1.CustomColors);
end;
// ------------------------------------------------------------------------------

// GetSolidObjFromScreenPt(): Helper method for attaching TLines to TSolids ...
function TfrmDrawObjects.GetSolidObjFromScreenPt(pt: TPoint): TSolid;
var
  i: Integer;
begin
  with ScrollBox1 do
    for i := 0 to controlCount - 1 do
      if (Controls[i] is TSolid) and TSolid(Controls[i]).PointOverObject(pt)
      then
      begin
        result := TSolid(Controls[i]);
        exit;
      end;
  result := nil;
end;
// ------------------------------------------------------------------------------

// when double-clicking an end button of a line (TConnector) object,
// let's make it connect to an underlying solid object if there is one ...
procedure TfrmDrawObjects.DrawObjDblClick(Sender: TObject);
var
  pt: TPoint;
  btnIdx: Integer;
  solid: TSolid;
begin
  // check if there's a TSolid under the cursor point ...
  GetCursorPos(pt);
  solid := GetSolidObjFromScreenPt(pt);
  if not(Sender is TConnector) or not assigned(solid) then
  begin
    Properties1Click(Sender);
    exit;
  end;

  // also check if a TDrawObject designer 'button' is under the cursor point ...
  pt := TConnector(Sender).ScreenToClient(pt);
  if not TConnector(Sender).BtnIdxFromPt(pt, true, btnIdx) then
  begin
    Properties1Click(Sender);
    exit;
  end;

  if (btnIdx = 0) then
  begin
    // the button at the beginning of the line was clicked, so ...
    TConnector(Sender).Connection1 := solid;
  end
  else if (btnIdx = TConnector(Sender).ButtonCount - 1) then
  begin
    // the button at the end of the line was clicked, so ...
    TConnector(Sender).Connection2 := solid;
    // let's give it an arrow too ...
    // TConnector(Sender).Arrow2 := true;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.New1Click(Sender: TObject);
var
  i: Integer;
begin
  // clear all DrawObjects
  with ScrollBox1 do
    for i := controlCount - 1 downto 0 do
      if Controls[i] is TDrawObject then
        Controls[i].Free;
  caption := application.Title + ' - [untitled]';
  SaveDialog1.Filename := '';
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.OpenObjects(const Filename: string);
var
  Strings: TStringList;
begin
  // first clear existing objects ...
  New1Click(nil);
  // now load new objects from file ...
  Strings := TStringList.create;
  try
    Strings.LoadFromFile(Filename);
    DrawObjects1.LoadDrawObjectsFromStrings(Strings, self, ScrollBox1,
      DrawObjLoaded);
    ClearAllDrawObjFocuses;
    caption := application.Title + ' - ' + extractfilename(Filename);
    SaveDialog1.Filename := Filename;
    OpenDialog1.Filename := Filename;
  finally
    Strings.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenObjects(OpenDialog1.Filename);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.SaveObjects(const Filename: string);
var
  i: Integer;
  saveList: TList;
  Strings: TStringList;
begin
  saveList := TList.create;
  Strings := TStringList.create;
  try
    with ScrollBox1 do
      for i := 0 to controlCount - 1 do
        if (Controls[i] is TDrawObject) then
          saveList.Add(Controls[i]);
    DrawObjects1.SaveDrawObjectsToStrings(saveList, Strings);
    Strings.SaveToFile(Filename);
    caption := application.Title + ' - ' + extractfilename(Filename);
    OpenDialog1.Filename := Filename;
  finally
    saveList.Free;
    Strings.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actSaveClick(Sender: TObject);
begin
  if (SaveDialog1.Filename = '') then
    actSaveAsClick(Sender)
  else
    SaveObjects(SaveDialog1.Filename);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actSaveAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SaveObjects(SaveDialog1.Filename);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.Exit1Click(Sender: TObject);
begin
  Close;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.actPrintClick(Sender: TObject);
var
  i: Integer;
  mf: TMetafile;
  mfc: TMetafileCanvas;
  srcRec: TRect;
begin
  srcRec := Rect(MaxInt, MaxInt, 0, 0);
  mf := TMetafile.create;
  with ScrollBox1 do
    try
      // first, calculate the dimensions of the metafile ...
      for i := 0 to controlCount - 1 do
        if (Controls[i] is TDrawObject) then
          with TDrawObject(Controls[i]) do
          begin
            srcRec.left := min(srcRec.left, left);
            srcRec.top := min(srcRec.top, top);

            srcRec.Right := max(srcRec.Right, left + Width);
            srcRec.Bottom := max(srcRec.Bottom, top + height);
          end;
      if (srcRec.left = MaxInt) then
        exit; // ie quit if no objects
      mf.Width := srcRec.Right - srcRec.left;
      mf.height := srcRec.Bottom - srcRec.top;

      // now, draw each DrawObject onto a metafile canvas ...
      // Notes:
      // 1. Designer buttons etc will not be drawn using either TDrawObject.Bitmap
      // property or the TDrawObject.Draw() method.
      // 2. When metafile scaling is required (eg when printing, where screen
      // pixel sizes are (very much) larger than printer pixel sizes), it's
      // vastly preferable to Draw() to a metafile canvas rather than simply
      // 'stretching' bitmaps to a canvas otherwise marked pixelation will occur.
      mfc := TMetafileCanvas.create(mf, 0);
      try
        for i := 0 to controlCount - 1 do
          if (Controls[i] is TDrawObject) then
            with TDrawObject(Controls[i]) do
              draw(mfc, left - srcRec.left, top - srcRec.top);
      finally
        FreeAndNil(mfc);
      end;

      // now preview the metafile ...
      with TPrintPreviewForm.create(self) do
        try
          assignMetafile(mf);
          ShowModal;
        finally
          Free;
        end;
    finally
      mf.Free;
    end;
end;
// ------------------------------------------------------------------------------

// Implement rubberband selection of objects ...
var
  SelectionRec: TRect;
  SelectionShape: TShape;

procedure TfrmDrawObjects.DrawFocusRec(Rec: TRect);
begin
  if not assigned(SelectionShape) then
  begin
    SelectionShape := TShape.create(self);
    SelectionShape.parent := ScrollBox1;
    SelectionShape.Brush.Style := bsClear;
    SelectionShape.Pen.Style := psDot;
  end;
  with Rec do
    SelectionShape.setbounds(left, top, Right - left, Bottom - top);
end;
// ------------------------------------------------------------------------------

function NormalizeRect(r: TRect): TRect;
begin
  if r.left < r.Right then
  begin
    result.left := r.left;
    result.Right := r.Right;
  end
  else
  begin
    result.left := r.Right;
    result.Right := r.left;
  end;
  if r.top < r.Bottom then
  begin
    result.top := r.top;
    result.Bottom := r.Bottom;
  end
  else
  begin
    result.top := r.Bottom;
    result.Bottom := r.top;
  end;
end;

procedure TfrmDrawObjects.ActionManager1Update(Action: TBasicAction;
  var Handled: Boolean);
var
  i, cnt: Integer;
  drawObj: TDrawObject;
  hasDrawObjects: Boolean;
begin
  // first, get the location for the new control (in case adding an Object) ...
  GetCursorPos(popupPt);

  // now only show the popup menuitems relevant to focused controls ...
  hasDrawObjects := ScrollboxHasDrawObjects;
  CountFocusedDrawObjs(cnt, drawObj);

  actProperties.Enabled := (cnt = 1);
  actDelete.Visible := (cnt > 0);

  actGrow.Visible := (cnt = 1) and ((drawObj is TLine) or (drawObj is TBezier))
    and not assigned(TConnector(drawObj).Connection2);
  actGrowTop.Visible := (cnt = 1) and
    ((drawObj is TLine) or (drawObj is TBezier)) and
    not assigned(TConnector(drawObj).Connection1);

  actShrink.Visible := actGrow.Visible and
    (((drawObj is TBezier) and (drawObj.ButtonCount > 4)) or
    (not(drawObj is TBezier) and (drawObj.ButtonCount > 2)));
  actShrinkTop.Visible := actGrowTop.Visible and
    (((drawObj is TBezier) and (drawObj.ButtonCount > 4)) or
    (not(drawObj is TBezier) and (drawObj.ButtonCount > 2)));

  actRotate.Visible := (cnt = 1);
  actCut.Enabled := (cnt > 0);
  actCopy.Enabled := (cnt > 0);
  actPaste.Enabled := clipboard.HasFormat(CF_DRAWOBJECTS);
  actCopyAsBitmap.Enabled := hasDrawObjects;
  actCopyAsMetafile.Enabled := actCopyAsBitmap.Enabled;
  actPrint.Enabled := actCopyAsBitmap.Enabled;
  actBringToFront.Visible := (cnt = 1);
  actSendToBack.Visible := (cnt = 1);
  actFlip.Visible := (cnt = 1) and
    ((drawObj is TPolygon) or (drawObj is TSolidBezier));
  actSavePicToBMP.Visible := (cnt = 1) and (drawObj is TPic);

  SplitPolyButton1.Visible := (cnt = 1) and (drawObj is TPolygon) and
    not(drawObj is TStar) and
    (TPolygon(drawObj).BtnIdxFromPt(TPolygon(drawObj).ScreenToClient(popupPt),
    true, i));
  DeletePolyButton1.Visible := SplitPolyButton1.Visible and
    (TPolygon(drawObj).ButtonCount > 3);

  actSaveAs.Enabled := hasDrawObjects;
  actSave.Enabled := hasDrawObjects;

  Align1.Enabled := (cnt > 0);
  actAlignLeft.Enabled := (cnt > 1);
  actAlignTop.Enabled := (cnt > 1);
  actAlignRight.Enabled := (cnt > 1);
  actAlignBottom.Enabled := (cnt > 1);

  Spacing1.Enabled := (cnt > 2);
  Sizing1.Enabled := (cnt > 1);

end;


// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ScrollBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not(ssLeft in Shift) then
    exit;
  SelectionRec := Rect(X, Y, X, Y);
  DrawFocusRec(SelectionRec);
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ScrollBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not assigned(SelectionShape) then
    exit;
  SelectionRec.Right := X;
  SelectionRec.Bottom := Y;
  DrawFocusRec(NormalizeRect(SelectionRec));
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.ScrollBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  dummyRec: TRect;
begin
  if assigned(SelectionShape) then
  begin
    FreeAndNil(SelectionShape);
    SelectionRec := NormalizeRect(SelectionRec);
    with SelectionRec, ScrollBox1 do
      for i := 0 to controlCount - 1 do
        if (Controls[i] is TDrawObject) then
          with TDrawObject(Controls[i]) do
            Focused := Visible and IntersectRect(dummyRec, SelectionRec,
              BoundsRect);
  end;
end;
// ------------------------------------------------------------------------------

procedure TfrmDrawObjects.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    // Escape key will cancel any impending rubberband selection ...
    if assigned(SelectionShape) then
      FreeAndNil(SelectionShape);
  end;
end;

procedure TfrmDrawObjects.toolButtonNewClick(Sender: TObject);
begin
  toolButtonNew.CheckMenuDropdown;
end;

// ------------------------------------------------------------------------------

end.
