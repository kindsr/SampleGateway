unit ConvertHex;

interface

uses
  SysUtils, Classes;

type
  byDynamicArr = array of byte;

function HexToByteArr(Hexs: string): byDynamicArr;
function HexToStr(Hexs: byDynamicArr): string;
function AnsiStrToHex(const str: string; Len: integer): string; overload;
function AnsiStrToHex(const str: string): string; overload;
function ByteArrToHex(Hexs: byDynamicArr): string;

implementation

function CharToByte(AChar: Char): byte; //Ascii Code
begin
  if charinset(AChar, ['0' .. '9']) then
    Result := byte(Ord(AChar) - Ord('0'))
  else
    Result := byte(10 + Ord(AChar) - Ord('A'));
end;

function ByteArrToHex(Hexs: byDynamicArr): string;
var
  i: integer;

begin
  Result := '';
  for i := low(Hexs) to high(Hexs) do
    Result := Result + inttohex(Hexs[i], 2);

end;

function HexToByteArr(Hexs: string): byDynamicArr; //String Hex -> Byte Array
var
  i: integer;
  byDynamicArray: byDynamicArr;
begin
  Hexs := stringreplace(Hexs, ' ', '', [rfReplaceAll]); // Space ' ' -> Null
  Hexs := UpperCase(Hexs);

  setlength(byDynamicArray, trunc(length(Hexs) / 2));
  fillchar(byDynamicArray[0], sizeof(byDynamicArray), 0);

  for i := 1 to trunc(length(Hexs) / 2) do // Hex -> Byte Arr
  begin
    byDynamicArray[i - 1] := (CharToByte(Hexs[i * 2 - 1]) * 16) + CharToByte(Hexs[i * 2]);
  end;
  Result := byDynamicArray;
end;

function HexToStr(Hexs: byDynamicArr): string; //Byte Array -> String Hex
var
  i: integer;
  charValue: byte;
  temp: ansistring;

begin
  temp := '';
  for i := Low(Hexs) to High(Hexs) do
  begin
    charValue := Hexs[i];
    temp := temp + ansiChar(charValue);
  end;
  Result := string(temp);

  if pos(#0, Result) <> 0 then
    Result := copy(Result, 1, pos(#0, Result) - 1);
end;


function AnsiStrToHex(const str: string; Len: integer): string; overload; //고정 길이 Hex
var
  Index: integer;
  temp: ansistring;

begin
  Result := '';
  temp := ansistring(str);

  for Index := 1 to Len do
    if length(temp) < Index then //문자열 길이를 초과하면 '00' 으로 채움
      Result := Result + '00'
    else
      Result := Result + inttohex(Ord(temp[Index]), 2);
end;

function AnsiStrToHex(const str: string): string; overload;
var
  Index: integer;
  temp: ansistring;

begin
  Result := '';
  temp := ansistring(str);

  for Index := 1 to length(temp) do
    Result := Result + inttohex(Ord(temp[Index]), 2);
end;

end.
