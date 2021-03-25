unit uFunction;

interface

uses
  SysUtils, Classes;

procedure Display(p_sender, p_message: string);
procedure trimFromTo(var aData: ansistring; aFrom, aTo: int64);
function getNow(): string;

var
  LogFile: TextFile;
  slSendToServerMsgList: TStringList;
  slSendToClientMsgList: TStringList;

implementation

procedure Display(p_sender, p_message: string);
var
  str: string;
  LogFileName: string;
begin
  // ... DISPLAY MESSAGE
  str := '[' + p_sender + '] - ' + getNow() + ': ' + p_message;
//  LogFileName := 'log_' + FormatDateTime('yyyymmdd', Now) + '.txt';
//  TThread.Queue(nil,
//    procedure
//    begin
////      MessagesLog.Lines.Add('[' + p_sender + '] - ' + getNow() + ': ' + p_message);
////      OutputDebugString(PChar(str));
//      WriteLn(str);
//    end);

  System.WriteLn(str);

//  try
//    AssignFile(LogFile, LogFileName);
//    if FileExists(LogFileName) then
//      Append(LogFile)
//    else
//      Rewrite(LogFile);
//    Writeln(LogFile, str);
//  finally
//    CloseFile(LogFile);
//  end;

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

initialization
  slSendToServerMsgList := TStringList.Create;
  slSendToClientMsgList := TStringList.Create;

finalization
  slSendToClientMsgList.Free;

end.

