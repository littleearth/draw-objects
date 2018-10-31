program DrawObjectsDemo;

uses
  Forms,
  SysUtils,
  main in 'main.pas' {frmDrawObjects} ,
  rotation in 'rotation.pas' {RotationForm} ,
  objInspector in 'objInspector.pas' {ObjInspectForm} ,
  preview in 'preview.pas' {PrintPreviewForm};

{$R *.res}

function IsFontInstalled(const AFontName: string): boolean;
begin
  Result := Screen.Fonts.IndexOf(AFontName) <> -1;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;

  if CheckWin32Version(6) then
  begin

    if IsFontInstalled('Segoe UI') then
    begin
      Application.DefaultFont.Name := 'Segoe UI';
      Screen.MessageFont.Name := 'Segoe UI';
    end;
    //
    if IsFontInstalled('Roboto Lt') then
    begin
      Application.DefaultFont.Name := 'Roboto Lt';
      Screen.MessageFont.Name := 'Roboto Lt';
    end;
  end;

  Application.MainFormOnTaskBar := True;
  Application.Title := 'DrawObjects';
  Application.CreateForm(TfrmDrawObjects, frmDrawObjects);
  Application.Run;

end.
