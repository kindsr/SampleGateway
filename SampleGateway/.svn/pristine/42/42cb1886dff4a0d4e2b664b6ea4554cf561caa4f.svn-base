unit uIndyTCPServer;

interface

uses
  Winapi.Windows, System.Classes, SysUtils, IdTCPServer, IdContext, Vcl.ExtCtrls,
  IdSocketHandle, IdGlobal, IdComponent, uFunction, System.StrUtils, uPacket;

const
  TCPCLIENT_PORT = 50001;

//type
//  TContextGuid = packed record
//    PeerIP: string;
//    Context: TIdContext;
//    Guid: string;
//  end;
//  TContextGuidArr = array of TContextGuid;

type
  TMWTCPServer = class(TObject)
    MWIdTCPServer: TIdTCPServer;
    procedure IdTCPServerConnect(AContext: TIdContext);
    procedure IdTCPServerDisconnect(AContext: TIdContext);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure ShowNumberOfClients(p_disconnected: Boolean = False);
    procedure BroadcastMessage(p_message: string);
  private
    { Private declarations }
  public
    constructor Create;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  MWTCPServer: TMWTCPServer;
  PacketHeader: TReqPacketHeader;
//  SendEchoTimer: TTimer;
//  ContextInfo: TContextGuidArr;

implementation

constructor TMWTCPServer.Create;
begin
  inherited Create;

  MWIdTCPServer := TIdTCPServer.Create(nil);

  {Set Indy TCP server Properties}
  with MWIdTCPServer do
  begin
    OnExecute := IdTCPServerExecute;
    OnConnect := IdTCPServerConnect;
    OnDisconnect := IdTCPServerDisconnect;
    OnStatus := IdTCPServerStatus;
    TerminateWaitTime := 5000;
    MaxConnections := 32;
    DefaultPort := TCPCLIENT_PORT;

    {Activate the Indy Server}
    Active := True;
    Display('TCP_SERVER', 'STARTED!');
  end;

//  SendEchoTimer := TTimer.Create(nil);
//  SendEchoTimer.Enabled := False;
//  SendEchoTimer.Interval := 1000;
end;

destructor TMWTCPServer.Destroy;
begin
  MWIdTCPServer.Active := False;
  Display('TCP_SERVER', 'STOPPED!');
  MWIdTCPServer.Free;
  inherited Destroy;
end;

procedure TMWTCPServer.IdTCPServerConnect(AContext: TIdContext);
var
  ip: string;
  port: Integer;
  peerIP: string;
  peerPort: Integer;
  nClients: Integer;
  msgToClient: string;
  typeClient: string;
  NewClient: TContextInfo;
begin
  // ... OnConnect is a TIdServerThreadEvent property that represents the event
  //     handler signalled when a new client connection is connected to the server.

  // ... Use OnConnect to perform actions for the client after it is connected
  //     and prior to execution in the OnExecute event handler.

  // ... see indy doc:
  //     http://www.indyproject.org/sockets/docs/index.en.aspx

  // ... getting IP address and Port of Client that connected
  ip := AContext.Binding.IP;
  port := AContext.Binding.Port;
  peerIP := AContext.Binding.PeerIP;
  peerPort := AContext.Binding.PeerPort;

  // Add ContextList
  NewClient := TContextInfo.Create;
  NewClient.PeerIp := peerIP;
  NewClient.Context := AContext;
  NewClient.LastStatus := 0;
  ContextList.Add(NewClient);

  // ... message log
  Display('TCP_SERVER', 'Client Connected!');
  Display('TCP_SERVER', 'Port=' + IntToStr(port) + ' ' + '(PeerIP=' + peerIP + ' - ' + 'PeerPort=' + IntToStr(peerPort) + ')');

  // ... display the number of clients connected
  ShowNumberOfClients();

  // ... CLIENT CONNECTED:
  case port of
    TCPCLIENT_PORT:
      begin
        // ... GUEST CLIENTS
        typeClient := 'New ECDIS';
      end;
      // ...
  end;

  // ... send the Welcome message to Client connected
  msgToClient := 'Welcome ' + typeClient + ' ' + 'Client :)';
  AContext.Connection.IOHandler.WriteLn(msgToClient);

end;

procedure TMWTCPServer.IdTCPServerDisconnect(AContext: TIdContext);
var
  ip: string;
  port: Integer;
  peerIP: string;
  peerPort: Integer;
  nClients: Integer;
begin

  // ... getting IP address and Port of Client that connected
  ip := AContext.Binding.IP;
  port := AContext.Binding.Port;
  peerIP := AContext.Binding.PeerIP;
  peerPort := AContext.Binding.PeerPort;

  // ... message log
  Display('TCP_SERVER', 'Client Disconnected! Peer=' + peerIP + ':' + IntToStr(peerPort));

  // ... display the number of clients connected
  ShowNumberOfClients(true);
end;

procedure TMWTCPServer.IdTCPServerExecute(AContext: TIdContext);
var
  Port: Integer;
  PeerPort: Integer;
  PeerIP: string;
  msgFromClient: string;
  byteMsgFromClient: TIdBytes;
  byteMsgToClient: TIdBytes;
  byteBodyFromServer: TIdBytes;
  // tmp
  len: Integer;
  tmpHeader: TReqPacketHeader;
  tmpBytes: TIdBytes;
  FileStream: TMemoryStream;
begin
  // ... OnExecute is a TIdServerThreadEvents event handler used to execute
  //     the task for a client connection to the server.

  // ... here you can check connection status and buffering before reading
  //     messages from client

  // ... see doc:
  // ... AContext.Connection.IOHandler.InputBufferIsEmpty
  // ... AContext.Connection.IOHandler.CheckForDataOnSource(<milliseconds>);
  //     (milliseconds to wait for the connection to become readable)
  // ... AContext.Connection.IOHandler.CheckForDisconnect;

  // ... received a message from the client

  // ... get message from client
  msgFromClient := AContext.Connection.IOHandler.ReadLn;
  byteMsgFromClient := IndyTextEncoding_ASCII.GetBytes(msgFromClient);
//  AContext.Connection.IOHandler.ReadBytes(byteMsgFromClient, 65535, False);
//  byteMsgFromClient := ToBytes(msgFromClient);


  // ... getting IP address, Port and PeerPort from Client that connected
  PeerIP := AContext.Binding.PeerIP;
  PeerPort := AContext.Binding.PeerPort;

  // ... message log
  Display('CLIENT', '(Peer=' + PeerIP + ':' + IntToStr(PeerPort) + ') ' + msgFromClient);
  // ...

  // ... process message from Client
  if (byteMsgFromClient[0] = PACKET_DELIMITER_1) and (byteMsgFromClient[1] = PACKET_DELIMITER_2) then
  begin
    SetPacketHeader(byteMsgFromClient, PacketHeader);

    case PacketHeader.MsgType of
      PACKET_TYPE_REQ:
        begin
          PacketHeader.MsgType := PACKET_TYPE_ACK;
          PacketHeader.BodySize := 0;
          byteMsgToClient := GetPacketHeaderBytes(PacketHeader);
//          AContext.Connection.IOHandler.Write(byteMsgToClient);
          slSendToClientMsgList.BeginUpdate;
          slSendToClientMsgList.Add(IndyTextEncoding_ASCII.GetString(byteMsgToClient));
          slSendToClientMsgList.EndUpdate;

          //////////////////////////////////////////////////////////////////////
          // Read BinaryFile
//          FileStream := TMemoryStream.Create;
//
//          try
//            FileStream.LoadFromFile('3_1011.txt');
//            FileStream.Position := 0;
//            SetLength(tmpBytes, FileStream.Size);
//            FileStream.Read(tmpBytes[0], FileStream.Size);
//          finally
//            FileStream.Free;
//          end;
//          FillChar(tmpHeader, SizeOf(TReqPacketHeader), 0);
//          SetPacketHeader(tmpBytes, tmpHeader);
//
//          len := tmpHeader.BodySize;
//          PacketHeader.BodySize := len;
//          byteMsgFromClient := GetPacketHeaderBytes(PacketHeader);
//
//          SetLength(byteBodyFromServer, len);
////          FillChar(byteBodyFromServer, len, #0);
//          Move(tmpBytes[SizeOf(TReqPacketHeader)], byteBodyFromServer[0], len);
////          if FileExists('temp1.txt') then
////            DeleteFile('temp1.txt');
////          Stream1 := TIdFileCreateStream.Create('temp1.txt');
////          try
////            Stream1.WriteBuffer(Pointer(byteBodyFromServer)^, Length(byteBodyFromServer));
////          finally
////            Stream1.Free;
////          end;
//          AContext.Connection.IOHandler.Write(byteMsgFromClient);
//          AContext.Connection.IOHandler.Write(byteBodyFromServer);
          //////////////////////////////////////////////////////////////////////

          // Shore G/W ·Î Àü´Þ
          slSendToServerMsgList.BeginUpdate;
//          slSendToServerMsgList.Add(IndyTextEncoding_ASCII.GetString(byteMsgFromClient)+IndyTextEncoding_ASCII.GetString(byteBodyFromServer));
          slSendToServerMsgList.Add(msgFromClient);
          slSendToServerMsgList.EndUpdate;
        end;

      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
//          len := PacketHeader.BodySize;
//          SetLength(byteBodyFromServer, len);
////          FillChar(byteBodyFromServer, len, #0);
//          Move(byteMsgFromClient[SizeOf(TReqPacketHeader)], byteBodyFromServer[0], len);
//
//          AContext.Connection.IOHandler.Write(byteMsgFromClient);
//          AContext.Connection.IOHandler.Write(byteBodyFromServer);

          slSendToServerMsgList.BeginUpdate;
//          slSendToServerMsgList.Add(IndyTextEncoding_ASCII.GetString(byteMsgFromClient)+IndyTextEncoding_ASCII.GetString(byteBodyFromServer));
          slSendToServerMsgList.Add(msgFromClient);
          slSendToServerMsgList.EndUpdate;
        end;
    end;
  end;

  // ...

  // ... send response to Client
  // ACK / RES ..
//  AContext.Connection.IOHandler.WriteLn('... message sent from server :)');
end;

procedure TMWTCPServer.IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  // ... OnStatus is a TIdStatusEvent property that represents the event handler
  //     triggered when the current connection state is changed...

  // ... message log
  Display('TCP_SERVER', AStatusText);
end;

procedure TMWTCPServer.ShowNumberOfClients(p_disconnected: Boolean);
var
  nClients: integer;
begin
  try
    // ... get number of clients connected
    nClients := MWIdTCPServer.Contexts.LockList.Count;
  finally
    MWIdTCPServer.Contexts.UnlockList;
  end;

  // ... client disconnected?
  if p_disconnected then
    dec(nClients);

  // ... display
//  TThread.Queue(nil,
//    procedure
//    begin
//      DisPlay('TCPCLIENT Number ', IntToStr(nClients));
//    end);
  Display('TCP_SERVER', 'Count of clients : ' + IntToStr(nClients));
end;

procedure TMWTCPServer.BroadcastMessage(p_message: string);
var
  tmpList: TList;
  contexClient: TidContext;
  nClients: Integer;
  i: integer;
begin
  // ... send a message to all clients connected

  // ... get context Locklist
  tmpList := MWIdTCPServer.Contexts.LockList;

  try
    i := 0;
    while (i < tmpList.Count) do
    begin
      // ... get context (thread of i-client)
      contexClient := tmpList[i];

      // ... send message to client
      contexClient.Connection.IOHandler.WriteLn(p_message);
      i := i + 1;
    end;

  finally
    // ... unlock list of clients!
    MWIdTCPServer.Contexts.UnlockList;
  end;
end;

end.

