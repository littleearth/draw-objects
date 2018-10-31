unit rotation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DrawObjects1;

const
  UM_TMCHANGED = WM_USER + 100;

type
  TRotationForm = class(TForm)
    btnOK: TButton;
    TrackBar1: TTrackBar;
    lblAngle: TLabel;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fMessagePending: boolean;
    procedure UmTmChanged(var message: TMessage); message UM_TMCHANGED;
    procedure UpdateLabel;
  public
    RotationObject: TDrawObject;
  end;

var
  RotationForm: TRotationForm;

implementation

{$R *.dfm}

type
  THackedDrawObj = class(TDrawObject);

procedure TRotationForm.UpdateLabel;
begin
  lblAngle.Caption := '&Rotation Angle: ' + inttostr(TrackBar1.Position);
end;
//------------------------------------------------------------------------------

procedure TRotationForm.UmTmChanged(var message: TMessage);
begin
  if not fMessagePending then exit;
  fMessagePending := false;
  //nb: Rotate() must be sandwiched between BeginTransform() & EndTransform().
  RotationObject.Rotate(TrackBar1.Position);
end;
//------------------------------------------------------------------------------

procedure TRotationForm.TrackBar1Change(Sender: TObject);
begin
  fMessagePending := true;
  //workaround to sensibly manage multiple rapid Rotate() calls ...
  PostMessage(handle, UM_TMCHANGED, 0,0);
  UpdateLabel;
end;
//------------------------------------------------------------------------------

procedure TRotationForm.FormShow(Sender: TObject);
begin
  UpdateLabel;
end;
//------------------------------------------------------------------------------

end.
