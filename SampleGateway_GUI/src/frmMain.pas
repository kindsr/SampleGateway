unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uIndyTCPServer, uIndyUDPServer,
  uIndyTCPClient;

{$DEFINE TCP_CLI_ACTIVE}
{.$DEFINE TCPTHREAD_CLI_ACTIVE}

type
  TMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnTCPServer: TButton;
    btnUDPServer: TButton;
    Timer1: TTimer;
    messagesLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;
  MWTCPServer: TMWTCPServer;
  MWUDPServer: TMWUDPServer;
  {$IFDEF TCP_CLI_ACTIVE}
  MWTCPClient: TMWTCPClient;
  {$ENDIF}

implementation

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  MWTCPServer := TMWTCPServer.Create;
  MWUDPServer := TMWUDPServer.Create;
  {$IFDEF TCP_CLI_ACTIVE}
  MWTCPClient := TMWTCPClient.Create;
  {$ENDIF}
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {$IFDEF TCP_CLI_ACTIVE}
  MWTCPClient.Disconnect;
  MWTCPClient.Destroy;
  {$ENDIF}

  MWTCPServer.Destroy;
  MWUDPServer.Destroy;
end;

end.
