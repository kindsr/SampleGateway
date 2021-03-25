unit uIndyTCPClient;

interface

uses
  System.Classes, System.SysUtils, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdThreadComponent, IdGlobal, uFunction, uPacket, ConsoleTimer;

const
  TCPHOST_IP = '127.0.0.1';
//  TCPHOST_IP = '192.168.0.240';
  TCPHOST_PORT = 13101;
  TCPHOST_PORT_SYNC = 13102;

type
  TEventHandlers = class
    procedure OnTimerTick(Sender : TObject);
  end;

type
  TMWTCPClient = class(TObject)
    FIsConnected: Boolean;
    procedure IdTCPClientConnected(Sender: TObject);
    procedure IdTCPClientDisconnected(Sender: TObject);
    procedure IdThreadComponentRun(Sender: TIdThreadComponent);
  private
    procedure SetIsConnected(const Value: Boolean);
    { Private declarations }
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Reconnect;
    procedure Disconnect;
    procedure SendToServer(msg: string);
    property IsConnected: Boolean read FIsConnected write SetIsConnected;
    { Public declarations }
  end;

var
  MWTCPClient: TMWTCPClient;
  MWIdTCPClient: TIdTCPClient;
  MWIdThreadComponent: TIdThreadComponent;
//  IsConnected: Boolean;
  PacketHeader: TReqPacketHeader;
  BodyLength: Integer;
  ConnTimer: TConsoleTimer;
  EventHandlers: TEventHandlers;

implementation

{ TMWTCPClient }

constructor TMWTCPClient.Create;
begin
  inherited;
  IsConnected := False;

  // ... create TIdTCPClient
  MWIdTCPClient := TIdTCPClient.Create();

  // ... set properties
  MWIdTCPClient.Host := TCPHOST_IP;
  MWIdTCPClient.Port := TCPHOST_PORT_SYNC;
  MWIdTCPClient.IPVersion := Id_IPv4;
  MWIdTCPClient.ConnectTimeout := 2000;
  // ... etc..

  // ... callback functions
  MWIdTCPClient.OnConnected := IdTCPClientConnected;
  MWIdTCPClient.OnDisconnected := IdTCPClientDisconnected;
  // ... etc..

  // ... create TIdThreadComponent
  MWIdThreadComponent := TIdThreadComponent.Create();

  // ... callback functions
  MWIdThreadComponent.OnRun := IdThreadComponentRun;
  // ... etc..

  // ... Retry connection
  EventHandlers := TEventHandlers.Create;
  ConnTimer := TConsoleTimer.Create;
  ConnTimer.Enabled := False;
  ConnTimer.Interval := 3000;
  ConnTimer.OnTimerEvent := EventHandlers.OnTimerTick;
end;

destructor TMWTCPClient.Destroy;
begin

  inherited;
end;

procedure TMWTCPClient.Connect;
begin
  if IsConnected then
    MWIdTCPClient.Disconnect;
  if MWIdTCPClient.IOHandler <> nil then
    MWIdTCPClient.IOHandler.InputBuffer.Clear;
  // ... try to connect to Server
  try
    MWIdTCPClient.Connect;
    IsConnected := MWIdTCPClient.Connected;
    ConnTimer.Enabled := True;
  except
//    on E: Exception do
//    begin
//      Display('TCP_CLIENT', 'CONNECTION ERROR! ' + E.Message);
      Display('TCP_CLIENT', 'CONNECTION ERROR! ');
      ConnTimer.Enabled := True;
//    end;
  end;
end;

procedure TMWTCPClient.Reconnect;
var
  k: Integer;
begin
  if IsConnected then
    MWIdTCPClient.Disconnect;
  if MWIdTCPClient.IOHandler <> nil then
    MWIdTCPClient.IOHandler.InputBuffer.Clear;

  try
    MWIdTCPClient.Connect;
    IsConnected := MWIdTCPClient.Connected;
    ConnTimer.Enabled := True;
    Exit;
  except
    Display('TCP_CLIENT', 'RECONNECTION ERROR! ');
    IsConnected := MWIdTCPClient.Connected;
    ConnTimer.Enabled := True;
  end;
end;

procedure TMWTCPClient.Disconnect;
begin
  // ... is connected?
  if MWIdTCPClient.Connected then
    // ... disconnect from Server
    MWIdTCPClient.Disconnect;

  IsConnected := MWIdTCPClient.Connected;
end;

procedure TMWTCPClient.IdTCPClientConnected(Sender: TObject);
begin
  // ... messages log
  Display('TCP_CLIENT', 'CONNECTED TO SERVER!');

  // ... after connection is ok, run the Thread ... waiting messages from server
  MWIdThreadComponent.Active := True;
end;

procedure TMWTCPClient.IdTCPClientDisconnected(Sender: TObject);
begin
  // ... message log
  Display('TCP_CLIENT', 'DISCONNECTED TO SERVER!');

  MWIdThreadComponent.Active := False;
end;

procedure TMWTCPClient.IdThreadComponentRun(Sender: TIdThreadComponent);
var
  msgFromServer: string;
  byteMsgFromServer: TIdBytes;
  byteBodyFromServer: TIdBytes;
  msgToClient: string;
begin
  // ... read message from server
//  msgFromServer := MWIdTCPClient.IOHandler.ReadLn();

  if BodyLength > 0 then
    MWIdTCPClient.IOHandler.ReadBytes(byteMsgFromServer, BodyLength)
  else
    MWIdTCPClient.IOHandler.ReadBytes(byteMsgFromServer, SizeOf(TReqPacketHeader));

  // ... messages log
  Display('TCP_CLIENT - FROM SERVER', IndyTextEncoding_ASCII.GetString(byteMsgFromServer));


  if (byteMsgFromServer[0] = PACKET_DELIMITER_1) and (byteMsgFromServer[1] = PACKET_DELIMITER_2) then
  begin
    SetPacketHeader(byteMsgFromServer, PacketHeader);
//    FillChar(byteBodyFromServer, PacketHeader.BodySize, #0);
    case PacketHeader.MsgType of
      PACKET_TYPE_REQ: ;
      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
          BodyLength := PacketHeader.BodySize;
        end;
    end;
  end
  else
  begin
    case PacketHeader.MsgType of
      PACKET_TYPE_REQ: ;
      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
//          len := Length(byteBodyFromServer);
          SetLength(byteBodyFromServer, BodyLength);
          Move(byteMsgFromServer[0], byteBodyFromServer[0], Length(byteMsgFromServer));
          Display('BODY', IndyTextEncoding_ASCII.GetString(byteBodyFromServer));
          Display('LENGTH', IntToStr(Length(byteBodyFromServer)));
          BodyLength := 0;

          // New ECDIS 로 전달할 내용
          slSendToClientMsgList.BeginUpdate;
          slSendToClientMsgList.Add(IndyTextEncoding_ASCII.GetString(GetPacketHeaderBytes(PacketHeader)+byteBodyFromServer));
          slSendToClientMsgList.EndUpdate;
        end;
    end;
  end;

  if (Pos('Goodbye', msgFromServer) > 0) or (Pos('Close', msgFromServer) > 0) then
  begin
    IsConnected := MWIdTCPClient.Connected;
    Reconnect;
  end;
end;

procedure TMWTCPClient.SendToServer(msg: string);
begin
  // ... send message to Server
  MWIdTCPClient.IOHandler.WriteLn(msg);
end;

procedure TMWTCPClient.SetIsConnected(const Value: Boolean);
begin
  FIsConnected := Value;
end;

{ TEventHandlers }

procedure TEventHandlers.OnTimerTick(Sender: TObject);
begin
  ConnTimer.Enabled := False;

  if not MWTCPClient.IsConnected then
    MWTCPClient.Reconnect;

  ConnTimer.Enabled := True;
end;

end.

