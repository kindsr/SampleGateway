unit uFunction;

interface

uses
  SysUtils, Classes, Winapi.Windows, Vcl.Forms, IdSync, System.IniFiles;

type
  TLogEvent = procedure( strMsg : string ) of object;

type
  TLog = class(TIdNotify)
    protected
      FMsg: string;
      procedure DoNotify; override;
    public
      class procedure LogMsg(const AMsg: string);
  end;

type
  TConfigRec = packed record
    HostIP: string;
    HostPort: Integer;
    TCPPort: Integer;
    UDPPort: Integer;
    RetryPeriod: Integer;
    HeartbeatPeriod: Integer;
    MMSI: string;
  end;

procedure Log( strMssg : string );
procedure Display(p_sender, p_message: string);
procedure trimFromTo(var aData: ansistring; aFrom, aTo: int64);
function getNow(): string;
procedure Delay(Milliseconds: Integer);
procedure LoadConfig(AFileName: string);

var
  slSendToServerMsgList: TStringList;
  slSendToClientMsgList: TStringList;
  LogFile: TextFile;
  ConfigInfo: TConfigRec;

implementation

var
  LogEvent : TLogEvent = nil;

procedure Log(strMssg : string);
begin
  if Assigned( LogEvent ) then
    LogEvent( strMssg );
end;

procedure Display(p_sender, p_message: string);
var
  str: string;
  LogFileName: string;
begin
  // ... DISPLAY MESSAGE
  str := '[' + p_sender + '] - ' + getNow() + ': ' + p_message;
  LogFileName := 'log_' + FormatDateTime('yyyymmdd', Now) + '.txt';
//  TThread.Queue(nil,
//    procedure
//    begin
////      MessagesLog.Lines.Add('[' + p_sender + '] - ' + getNow() + ': ' + p_message);
////      OutputDebugString(PChar(str));
//      WriteLn(str);
//    end);

  System.WriteLn(str);
//  TLog.LogMsg(str);

  try
    AssignFile(LogFile, LogFileName);
    if FileExists(LogFileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    Writeln(LogFile, str);
  finally
    CloseFile(LogFile);
  end;

  // ... see doc..
  // ... TThread.Queue() causes the call specified by AMethod to
  //     be asynchronously executed using the main thread, thereby avoiding
  //     multi-thread conflicts.
end;

procedure trimFromTo(var aData: ansistring; aFrom, aTo: int64);
  var s: ansistring;
begin
  setLength(s, aTo - aFrom + 1);
  Move(aData[aFrom], s[1], aTo - aFrom + 1);
  setLength(aData, length(s));
  aData := s;
end;

function getNow(): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ';
end;

procedure Delay(Milliseconds: Integer);
var
  Tick: DWORD;
  Event: THandle;
begin
  Event := CreateEvent(nil, False, False, nil);
  try
    Tick := GetTickCount + DWORD(Milliseconds);
    while (Milliseconds > 0) and
      (MsgWaitForMultipleObjects(1, Event, False, Milliseconds,
      QS_ALLINPUT) <> WAIT_TIMEOUT) do
    begin
      Application.ProcessMessages;
      Milliseconds := Tick - GetTickCount;
    end;
  finally
    CloseHandle(Event);
  end;
end;

procedure LoadConfig(AFileName: string);
var
  iniFile: TIniFile;
begin
  if not FileExists(AFileName) then Exit;

  iniFile := TIniFile.Create(AFileName);

  try
    ConfigInfo.HostIP := iniFile.ReadString('MECYS', 'HOSTIP', '127.0.0.1');
    ConfigInfo.HostPort := iniFile.ReadInteger('MECYS', 'HOSTPORT', 13102);
    ConfigInfo.TCPPort := iniFile.ReadInteger('MECYS', 'TCPPORT', 50001);
    ConfigInfo.UDPPort := iniFile.ReadInteger('MECYS', 'UDPPORT', 60001);
    ConfigInfo.RetryPeriod := iniFile.ReadInteger('MECYS', 'RETRY', 5000);
    ConfigInfo.HeartbeatPeriod := iniFile.ReadInteger('MECYS', 'HBEAT', 10000);
    ConfigInfo.MMSI := iniFile.ReadString('MECYS', 'MMSI', '123456789');
  finally
    iniFile.Free;
  end;
end;

{ TLog }

procedure TLog.DoNotify;
begin
  System.Writeln(FMsg);
end;

class procedure TLog.LogMsg(const AMsg: string);
begin
  with TLog.Create do
  try
    FMsg := AMsg;
    Notify;
  except
    Free;
    raise;
  end;
end;

initialization
  slSendToServerMsgList := TStringList.Create;
  slSendToClientMsgList := TStringList.Create;

finalization
  slSendToClientMsgList.Free;

end.

