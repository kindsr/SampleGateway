program SampleGW;

{$APPTYPE CONSOLE}

{$DEFINE TCP_CLI_ACTIVE}

{$R *.res}

uses
  System.SysUtils,
  Vcl.Forms,
  uFunction in 'src\uFunction.pas',
  uIndyTCPServer in 'src\uIndyTCPServer.pas',
  uIndyUDPServer in 'src\uIndyUDPServer.pas',
  uPacket in 'src\uPacket.pas',
  uDCPCrypt in 'src\uDCPCrypt.pas',
  uCrypto in 'src\uCrypto.pas',
  ConvertHex in 'src\ConvertHex.pas',
  ConsoleTimer in 'src\ConsoleTimer.pas',
  uIndyTCPClientThreaded in 'src\uIndyTCPClientThreaded.pas';

var
  DoContinue: Boolean;
  InputStr: string;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    LoadConfig(ChangeFileExt(ExtractFileName(Application.ExeName), '.ini'));

    MWTCPServer := TMWTCPServer.Create;
    MWUDPServer := TMWUDPServer.Create;

    DoContinue := True;
    while DoContinue do
    begin
      try
        {$IFDEF TCP_CLI_ACTIVE}
        MWTCPClient := TMWTCPClient.Create;
        {$ENDIF}

        MWTCPClient.Start;
        ReadLn;
      except
        {$IFDEF TCP_CLI_ACTIVE}
        MWTCPClient.Stop;
        MWTCPClient.Destroy;
        {$ENDIF}
      end;
    end;
    MWTCPServer.Destroy;
    MWUDPServer.Destroy;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
