unit preview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, WinSpool, Printers, StdCtrls, ToolWin,
  ImgList, System.ImageList;

type
  TPrintPreviewForm = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    PrintDialog1: TPrintDialog;
    ToolBar1: TToolBar;
    tbFit: TToolButton;
    tb100: TToolButton;
    ToolButton3: TToolButton;
    tbPrint: TToolButton;
    ToolButton5: TToolButton;
    tbClose: TToolButton;
    ImageList1: TImageList;
    procedure ScrollBox1Resize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbFitClick(Sender: TObject);
    procedure tbPrintClick(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    fMF: TMetafile;
    fHasPrinterInstalled: boolean;
    fUseLandscape: boolean;
    fPtrDimensionsPx: TSize;
    fPtrPhysOffsetPx: TSize;
    fPtrPPI: TSize;
    fScaling: double;
    fScaleToFit: boolean;
    procedure SetScaleToFit(value: boolean);
  public
    procedure assignMetafile(metafile: TMetafile);
    property ScaleToFit: boolean read fScaleToFit write SetScaleToFit;
  end;

var
  PrintPreviewForm: TPrintPreviewForm;
const
  DEFAULT_PAGE_HEIGHT_MM = 297; //A4
  DEFAULT_PAGE_WIDTH_MM  = 210;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function PrinterDriverExists: boolean;
var
  Flags, Count, NumInfo: dword;
  Level: Byte;
begin
  //avoid using fPrinter.printers.count as this will raise an
  //exception if no printer driver is installed...
  Count := 0;
  try
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
      Level := 4;
    end else
    begin
      Flags := PRINTER_ENUM_LOCAL;
      Level := 5;
    end;
    EnumPrinters(Flags, nil, Level, nil, 0, Count, NumInfo);
  except
  end;
  result := (count > 0);
end;
//------------------------------------------------------------------------------

function CurrentPrinterName: string;
var
  Device : array[0..255] of char;
  Driver : array[0..255] of char;
  Port  : array[0..255] of char;
  hDeviceMode: THandle;
begin
  Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
  result := Device;
end;
//------------------------------------------------------------------------------

function CurrentPrinterPaperSize: string;
var
  PtrPPI: TPoint;
  size: TSize;
begin
  try
    PtrPPI.x := GetDeviceCaps(printer.Handle, LOGPIXELSX);
    PtrPPI.y := GetDeviceCaps(printer.Handle, LOGPIXELSY);
    size.cx := round(GetDeviceCaps(printer.Handle, PHYSICALWIDTH)*25.4/PtrPPI.x);
    size.cy := round(GetDeviceCaps(printer.Handle, PHYSICALHEIGHT)*25.4/PtrPPI.y);
  except
  end;
  with size do
  begin
    if cx > cy then
    begin
      //landscape ...
      case cy of
        148: if (cx = 210) then result := 'A5 (210mm x 148mm)';
        210: if (cx = 297) then result := 'A4 (297mm x 210mm)';
        216: if (cx = 279) then result := 'Letter (11" x 8½")'
             else if (cx = 356) then result := 'Legal (14" x 8½")';
        297: if (cx = 420) then result := 'A3 (420mm x 297mm)';
      end;
    end else
    begin
      //portrait ...
      case cx of
        148: if (cy = 210) then result := 'A5 (148mm x 210mm)';
        210: if (cy = 297) then result := 'A4 (210mm x 297mm)';
        216: if (cy = 279) then result := 'Letter (8½" x 11")'
             else if (cy = 356) then result := 'Legal (8½" x 14")';
        297: if (cy = 420) then result := 'A3 (297mm x 420mm)';
      end;
    end;
    if result = '' then result := format('Custom (%dmm x %dmm)',[cx, cy]);
  end;
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.FormCreate(Sender: TObject);
begin
  fMF := TMetafile.Create;
  fHasPrinterInstalled := PrinterDriverExists;
  tbPrint.Enabled := fHasPrinterInstalled;
  Caption := application.Title + '  -  Print Preview';
  fScaleToFit := true;
  if assigned(owner) and (owner is TCustomForm) then
    with TCustomForm(owner) do
      if WindowState = wsMaximized then
        self.WindowState := wsMaximized
      else
      begin
        self.Width := width;
        self.Height := height;
      end;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.FormDestroy(Sender: TObject);
begin
  fMF.Free;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.assignMetafile(metafile: TMetafile);
begin
  fMF.Assign(metafile);

  fUseLandscape := assigned(metafile) and (fMF.Width > fMF.Height);
  if fHasPrinterInstalled then
  begin
    if fUseLandscape then
      Printer.Orientation := poLandscape else
      Printer.Orientation := poPortrait;
    fPtrPPI.cx := GetDeviceCaps(printer.Handle, LOGPIXELSX);
    fPtrPPI.cy := GetDeviceCaps(printer.Handle, LOGPIXELSY);
    fPtrDimensionsPx.cx := GetDeviceCaps(printer.Handle, PHYSICALWIDTH);
    fPtrDimensionsPx.cy := GetDeviceCaps(printer.Handle, PHYSICALHEIGHT);
    fPtrPhysOffsetPx.cx := GetDeviceCaps(printer.Handle, PHYSICALOFFSETX);
    fPtrPhysOffsetPx.cy := GetDeviceCaps(printer.Handle, PHYSICALOFFSETY);

    StatusBar1.SimpleText :=
      '  ' + CurrentPrinterName + '  -  ' + CurrentPrinterPaperSize;
  end else
  begin
    //in case there is no printer driver installed ...
    fPtrPPI.cx := screen.PixelsPerInch;
    fPtrPPI.cy := screen.PixelsPerInch;
    fPtrPhysOffsetPx.cx := 0;
    fPtrPhysOffsetPx.cy := 0;
    if fUseLandscape then
    begin
      fPtrDimensionsPx.cx := round(DEFAULT_PAGE_HEIGHT_MM * fPtrPPI.cx / 25.4);
      fPtrDimensionsPx.cy := round(DEFAULT_PAGE_WIDTH_MM * fPtrPPI.cy / 25.4);
    end else
    begin
      fPtrDimensionsPx.cx := round(DEFAULT_PAGE_WIDTH_MM * fPtrPPI.cx / 25.4);
      fPtrDimensionsPx.cy := round(DEFAULT_PAGE_HEIGHT_MM * fPtrPPI.cy / 25.4);
    end;
    StatusBar1.SimpleText := '  No printer installed.'
  end;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.SetScaleToFit(value: boolean);
begin
  if fScaleToFit = value then exit;
  fScaleToFit := value;
  ScrollBox1Resize(nil);
  PaintBox1.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.ScrollBox1Resize(Sender: TObject);
var
  maxVirtualPageSize: TSize;
  scaling2: double;
begin
  //In case no metafile has been assigned, make sure page layout is calc'ed ...
  if fPtrPPI.cx = 0 then assignMetafile(nil);

  maxVirtualPageSize.cx := ScrollBox1.ClientWidth -20;
  maxVirtualPageSize.cy := ScrollBox1.ClientHeight -20;

  //get the scaling factor to shrink a page into the preview window with at
  //least a 10 pixel margin around the page ...
  if fScaleToFit then
  begin
    fScaling := (maxVirtualPageSize.cx) /
      (fPtrDimensionsPx.cx / fPtrPPI.cx * screen.PixelsPerInch);
    scaling2 := maxVirtualPageSize.cy /
      (fPtrDimensionsPx.cy / fPtrPPI.cy * screen.PixelsPerInch);
    if scaling2 < fScaling then fScaling := scaling2;
  end else
    fScaling := 1;

  //now size the Paintbox (ie virtual page) ...
  PaintBox1.Width :=
    round(fPtrDimensionsPx.cx / fPtrPPI.cx * screen.PixelsPerInch * fScaling);
  PaintBox1.Height :=
    round(fPtrDimensionsPx.cy / fPtrPPI.cy * screen.PixelsPerInch * fScaling);

  //center page (ie if scaled) in preview window ...
  PaintBox1.Left := 10; PaintBox1.Top := 10;
  if PaintBox1.Width < maxVirtualPageSize.cx then
    PaintBox1.Left := (ScrollBox1.ClientWidth - PaintBox1.Width) div 2;
  if PaintBox1.Height < maxVirtualPageSize.cy then
    PaintBox1.Top := (ScrollBox1.ClientHeight - PaintBox1.Height) div 2;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.PaintBox1Paint(Sender: TObject);
var
  rec: TRect;
begin
  with PaintBox1 do
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(clientRect);
    if not assigned(fMF) then exit;
    rec := Rect(0, 0,
      round(fMF.Width * fScaling),
      round(fMF.Height * fScaling));
    //now center rec ...
    OffsetRect(rec,
      (ClientWidth - rec.Right) div 2, (ClientHeight - rec.Bottom) div 2);
    //finally draw it on the canvas ...
    Canvas.StretchDraw(rec, fMF);
  end;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.tbFitClick(Sender: TObject);
begin
  ScaleToFit := (Sender = tbFit);
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.tbPrintClick(Sender: TObject);
var
  destRec: TRect;
begin
  if not PrintDialog1.Execute then exit;
  Printer.Title := application.Title + ' document';
  Printer.Copies := PrintDialog1.Copies;
  Printer.BeginDoc;
  try
    //make the printed metafile the same size as the original (screen) size ...
    destRec := Rect(0, 0,
      fMf.Width * fPtrPPI.cx div screen.PixelsPerInch,
      fMf.Height * fPtrPPI.cx div screen.PixelsPerInch);

    //center align the destination rect ...
    OffsetRect(destRec,
      (fPtrDimensionsPx.cx - destRec.right) div 2 - fPtrPhysOffsetPx.cx,
      (fPtrDimensionsPx.cy - destRec.bottom) div 2 - fPtrPhysOffsetPx.cy);
    //finally, do the printing  ...
    Printer.Canvas.StretchDraw(destRec, fMf);
  finally
    Printer.EndDoc;
  end;
  close;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.tbCloseClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TPrintPreviewForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then close;
end;
//------------------------------------------------------------------------------

end.
