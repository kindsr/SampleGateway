program SampleGW_GUI;

uses
  Vcl.Forms,
  ConvertHex in 'src\ConvertHex.pas',
  uCrypto in 'src\uCrypto.pas',
  uDCPCrypt in 'src\uDCPCrypt.pas',
  uFunction in 'src\uFunction.pas',
  uIndyTCPClient in 'src\uIndyTCPClient.pas',
  uIndyTCPClientThreaded in 'src\uIndyTCPClientThreaded.pas',
  uIndyTCPServer in 'src\uIndyTCPServer.pas',
  uIndyUDPServer in 'src\uIndyUDPServer.pas',
  uPacket in 'src\uPacket.pas',
  frmMain in 'src\frmMain.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
