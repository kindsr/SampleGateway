unit uCrypto;

interface

uses
  System.SysUtils,
  uTPLb_CryptographicLibrary,
  uTPLb_Codec,
  uTPLb_Constants;

function StrEncryptV3(str: string; pass: string): string;
function StrDecryptV3(str: string; pass: string): string;

implementation

function StrEncryptV3(str: string; pass: string): string;
var
  Codec: TCodec;
  CryptographicLibrary: TCryptographicLibrary;
  s: string;
begin
  Codec := TCodec.Create( nil);
  CryptographicLibrary := TCryptographicLibrary.Create( nil);
  try
    Codec.CryptoLibrary  := CryptographicLibrary;
    Codec.StreamCipherId := uTPLb_Constants.BlockCipher_ProgId;
    Codec.BlockCipherId  := 'native.AES-256';
    Codec.ChainModeId    := uTPLb_Constants.CBC_ProgId;
    Codec.Password := pass;
    Codec.EncryptString(str, s, TEncoding.ASCII);
    result := s;
  finally
    Codec.Free;
    CryptographicLibrary.Free;
  end;
end;

function StrDecryptV3(str: string; pass: string): string;
var
  Codec: TCodec;
  CryptographicLibrary: TCryptographicLibrary;
  s: string;
begin
  Codec := TCodec.Create( nil);
  CryptographicLibrary := TCryptographicLibrary.Create( nil);
  try
    Codec.CryptoLibrary  := CryptographicLibrary;
    Codec.StreamCipherId := uTPLb_Constants.BlockCipher_ProgId;
    Codec.BlockCipherId  := 'native.AES-256';
    Codec.ChainModeId    := uTPLb_Constants.CBC_ProgId;
    Codec.Password := pass;
    Codec.DecryptString(s, str, TEncoding.ASCII);
    result := s;
  finally
    Codec.Free;
    CryptographicLibrary.Free;
  end;
end;

end.
