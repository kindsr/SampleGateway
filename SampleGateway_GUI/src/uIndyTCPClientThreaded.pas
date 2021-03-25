unit uIndyTCPClientThreaded;

interface

uses
  Winapi.Windows, Classes, System.SyncObjs, IdTCPClient, Vcl.Forms, Vcl.StdCtrls,
  SysUtils, IdGlobal, uFunction;

const
  TCPHOST_IP = '127.0.0.1';
//  TCPHOST_IP = '192.168.0.240';
  TCPHOST_PORT = 13101;
  TCPHOST_PORT_SYNC = 13102;

type
  TWriteThread = class(TThread)
  private
    FData: TStringList;
    FIdClient: TIdTCPClient;
    FCnt: Integer;
    FCS: TCriticalSection;
    procedure SetData(const Value: TStringList);
    procedure SetIdClient(const Value: TIdTCPClient);
    procedure SetCnt(const Value: Integer);
    procedure SetCS(const Value: TCriticalSection);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Data : TStringList read FData write SetData;
    property IdClient : TIdTCPClient read FIdClient write SetIdClient;
    property Cnt : Integer read FCnt write SetCnt;
    property CS : TCriticalSection read FCS write SetCS;
  end;

type
  TReadThread = class(TThread)
  private
    FData: TStringList;
    FIdClient : TIdTCPClient;
    FCS: TCriticalSection;
    procedure SetData(const Value: TStringList);
    procedure SetIndy(const Value: TIdTCPClient);
    procedure SetCS(const Value: TCriticalSection);
  protected

    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Data : TStringList read FData write SetData;
    property IdClient : TIdTCPClient read FIdClient write SetIndy;
    property CS : TCriticalSection read FCS write SetCS;
  end;

type
  TDisplayThread = class(TThread)
  private
    FData: TStringList;
//    FListBox: TListBox;
//    FDisplayList: TStringList;
    FCS: TCriticalSection;
//    procedure SetListBox(const Value: TListBox);
//    procedure SetDisplayList(const Value: TStringList);
    procedure SetData(const Value: TStringList);
    procedure SetCS(const Value: TCriticalSection);
  protected

    procedure DisplayMsg;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

//    property ListBox : TListBox read FListBox write SetListBox;
//    property DisplayList : TStringList read FDisplayList write SetDisplayList;
    property Data : TStringList read FData write SetData;
    property CS : TCriticalSection read FCS write SetCS;
  end;

type
  TMWTCPClient = class(TObject)
    MWIdTCPClient: TIdTCPClient;
//    ListBox: TListBox;
    private
      { Private declarations }
    public
      slData : TStringList;
//      displayList: TStringList;
      iCnt : Integer;
      cs : TCriticalSection;
      readThread : TReadThread;
      writeThread : TWriteThread;
      displayThread : TDisplayThread;
      constructor Create;
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
      { Public declarations }
  end;

var
  MWTCPClient: TMWTCPClient:

implementation

{ TWriteThread }

constructor TWriteThread.Create;
begin
  FreeOnTerminate := False;
  Cnt := 0;
  inherited Create( true );
end;

destructor TWriteThread.Destroy;
begin
  inherited;
end;

procedure TWriteThread.Execute;
var
  cmd : String;
begin
  if not IdClient.Connected Then  exit;

  while not Terminated do
  begin

    IdClient.IOHandler.CheckForDisconnect(True, True);

    inc( FCnt );

    IdClient.IOHandler.Write(Inttostr( Cnt )+#02#$d#$a );

    CS.Enter;
    try
      Data.Add( 'Write = ' + IntToStr( FCnt ) );
    finally
      CS.Leave;
    end;

    Application.ProcessMessages;
    WaitForSingleObject( Handle, 10 );
  end;

end;

procedure TWriteThread.SetCnt(const Value: Integer);
begin
  FCnt := Value;
end;

procedure TWriteThread.SetIdClient(const Value: TIdTCPClient);
begin
  FIdClient := Value;
end;


procedure TWriteThread.SetData(const Value: TStringList);
begin
  FData := Value;
end;

procedure TWriteThread.SetCS(const Value: TCriticalSection);
begin
  FCS := Value;
end;

{ TReadThread }

constructor TReadThread.Create;
begin
  FreeOnTerminate := False;
  inherited Create( True );
end;

destructor TReadThread.Destroy;
begin
  inherited;
end;

procedure TReadThread.Execute;
var
  t : Cardinal;
  procedure AddData( aStr : String );
  begin
    CS.Enter;
    try
      Data.Add( aStr );
    finally
      CS.Leave;
    end;
  end;
begin
  while not Terminated do
  try
    t := GetTickCount;

    repeat
      IdClient.IOHandler.CheckForDisconnect(True, True);
      IdClient.IOHandler.CheckForDataOnSource( 100 );
      if GetTickCount - t > 3000 then
      begin
        AddData( '>>>>>>>>>>>>>>>>>>> Read Time Out <<<<<<<<<<<<<<<<<<<<' );
        Break;
      end;

      WaitForSingleObject( Handle, 10 );
      Application.ProcessMessages;

    until IdClient.IOHandler.InputBuffer.Size > 0;

    if GetTickCount - t > 3000 then
      Continue;


    CS.Enter;
    try
      Data.Add( 'Read = ' + IdClient.IOHandler.ReadLn );
    finally
      CS.Leave;
    end;

  finally
    WaitForSingleObject( Handle, 10 );
    Application.ProcessMessages;
  end;

end;

procedure TReadThread.SetIndy(const Value: TIdTCPClient);
begin
  FIdClient := Value;
end;


procedure TReadThread.SetData(const Value: TStringList);
begin
  FData := Value;
end;

procedure TReadThread.SetCS(const Value: TCriticalSection);
begin
  FCS := Value;
end;

{ TDisplayThread }

constructor TDisplayThread.Create;
begin
  inherited Create( True );
end;

destructor TDisplayThread.Destroy;
begin

  inherited;
end;

procedure TDisplayThread.DisplayMsg;
var
  i: Integer;
begin
//  With ListBox do
//  begin
//    Items.Add( 'Data Count = ' + IntToStr( Data.Count ) );
//    for i := 0 to Data.Count - 1 do
//    begin
//      Items.Add(  Data[i] );
//      ItemIndex := Count -1;
//    end;
//
//    Data.Clear;
//  end;

//  With DisplayList do
//  begin
    Display('TCP_CLIENT', 'Data Count = ' + IntToStr( Data.Count ));
    for i := 0 to Data.Count - 1 do
    begin
      Display('TCP_CLIENT', Data[i]);
    end;

    Data.Clear;
//  end;
end;

procedure TDisplayThread.Execute;
begin
  while not Terminated do
  begin
    CS.Enter;
    try
      Synchronize(  DisplayMsg );
    finally
      CS.Leave;
    end;
    Application.ProcessMessages;
    WaitForSingleObject( Handle, 10 );
  end;

end;

//procedure TDisplayThread.SetListBox(const Value: TListBox);
//begin
//  FListBox := Value;
//end;

procedure TDisplayThread.SetData(const Value: TStringList);
begin
  FData := Value;
end;

procedure TDisplayThread.SetCS(const Value: TCriticalSection);
begin
  FCS := Value;
end;

{ TMWTCPClient }

constructor TMWTCPClient.Create;
begin
  inherited;

  MWIdTCPClient := TIdTCPClient.Create(nil);

  with MWIdTCPClient do
  begin
    Host := TCPHOST_IP;
    Port := TCPHOST_PORT_SYNC;
    ReuseSocket := rsOSDependent;
    UseNagle := True;
  end;

//  ListBox := TListBox.Create(nil);
//  ListBox.Parent := nil;

  cs := TCriticalSection.Create;
  readThread := TReadThread.Create;
  writeThread := TWriteThread.Create;
  displayThread := TDisplayThread.Create;
  slData := TStringList.Create;
//  displayList := TStringList.Create;
end;

destructor TMWTCPClient.Destroy;
begin
  if readThread.Suspended then
    readThread.Resume;
  readThread.Terminate;

  if writeThread.Suspended then
    writeThread.Resume;
  writeThread.Terminate;

  if displayThread.Suspended then
    displayThread.Resume;
  displayThread.Terminate;

  writeThread.WaitFor;
  readThread.WaitFor;
  displayThread.WaitFor;

  readThread.Free;
  writeThread.Free;
  displayThread.Free;
  cs.Free;
  slData.free;
//  displayList.free;

  inherited;
end;

procedure TMWTCPClient.Start;
begin
//  ListBox.Clear;
//  displayList.Clear;

  if not MWIdTCPClient.Connected then
    MWIdTCPClient.Connect;

  Display('TCP_CLIENT', 'CONNECTED TO SERVER!');

  readThread.CS       := cs;
  readThread.IdClient := MWIdTCPClient;
  readThread.Data     := slData;

  writeThread.CS       := cs;
  writeThread.IdClient := MWIdTCPClient;
  writeThread.Data     := slData;

//  displayThread.DisplayList := displayList;
  displayThread.CS          := cs;
  displayThread.Data        := slData;

  readThread.Resume;
  writeThread.Resume;
  displayThread.Resume;
end;

procedure TMWTCPClient.Stop;
begin
  if not readThread.Suspended then
    readThread.Suspend;
  if not writeThread.Suspended then
    writeThread.Suspend;
  if not displayThread.Suspended then
    displayThread.Suspend;

//  MWIdTCPClient.Disconnect;
end;

end.
