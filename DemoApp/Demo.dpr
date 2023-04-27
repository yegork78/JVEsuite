program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainWnd in 'MainWnd.pas' {MainWindow};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end.

