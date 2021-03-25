unit uIndyUDPServer;

interface

uses
  Winapi.Windows, Classes, SysUtils, IdUDPServer, IdSocketHandle, IdGlobal,
  uFunction, IdAntiFreezeBase;

const
  UDPCLIENT_PORT = 60001;

type
  TMWUDPServer = class(TObject)
    MWIdUDPServer: TIdUDPServer;
    MWIdAntiFreeze: TIdAntiFreezeBase;
    procedure IdUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
  private
    { Private declarations }
  public
    constructor Create;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  MWUDPServer: TMWUDPServer;

implementation

{ TMWTCPServer }

constructor TMWUDPServer.Create;
begin
  inherited Create;

  MWIdUDPServer := TIdUDPServer.Create(nil);
//  MWIdAntiFreeze := TIdAntiFreezeBase.Create(nil);

//  MWIdAntiFreeze.ApplicationHasPriority := True;
//  MWIdAntiFreeze.IdleTimeOut := 250;
//  MWIdAntiFreeze.OnlyWhenIdle := True;
//  MWIdAntiFreeze.Active := True;

  {Set Indy TCP server Properties}
  with MWIdUDPServer do
  begin
    BroadcastEnabled := True;
    ThreadedEvent := True;
    DefaultPort := UDPCLIENT_PORT;

    Bindings.Clear;
    Bindings.Add.Port := UDPCLIENT_PORT;

    OnUDPRead := IdUDPRead;

    {Activate the Indy Server}
    Active := True;

    Display('UDP_SERVER', 'STARTED!');
  end;
end;

destructor TMWUDPServer.Destroy;
begin
//  MWIdAntiFreeze.Active := False;
  MWIdUDPServer.Active := False;
  Display('UDP_SERVER', 'STOPPED!');
//  MWIdAntiFreeze.Free;
  MWIdUDPServer.Free;
  inherited Destroy;
end;

procedure TMWUDPServer.IdUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  PeerPort: Integer;
  PeerIP: string;
  msgFromClient: string;
begin
  try
    // ... get message from client
    msgFromClient := BytesToString(AData);

    PeerIP := ABinding.PeerIP;
    PeerPort := ABinding.PeerPort;

    Display('UDP_CLIENT', '(Peer=' + PeerIP + ':' + IntToStr(PeerPort) + ') ' + msgFromClient);

    slSendToServerMsgList.BeginUpdate;
    slSendToServerMsgList.Add(msgFromClient);
    slSendToServerMsgList.EndUpdate;
  except
    on E: Exception do
    begin
    end;
  end;
end;

end.
