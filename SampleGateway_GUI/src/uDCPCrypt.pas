unit uDCPCrypt;

interface

uses
  Classes, System.SysUtils, DCPcrypt2, DCPrijndael;

function Min(a, b: integer): integer;
function DoEncryptFile(infile,outfile,passphrase:String; Hash: TDCP_hash;Cipher: TDCP_cipher):Boolean;
function DoDecryptFile(infile,outfile,passphrase:String; Hash: TDCP_hash;Cipher: TDCP_cipher):Boolean;

implementation

function Min(a, b: integer): integer;
begin
  if (a < b) then
    Result := a
  else
    Result := b;
end;

function DoEncryptFile(infile,outfile,passphrase:String; Hash: TDCP_hash;Cipher: TDCP_cipher):Boolean;
var
  CipherIV: array of byte;     // the initialisation vector (for chaining modes)
  HashDigest: array of byte;   // the result of hashing the passphrase with the salt
  Salt: array[0..7] of byte;   // a random salt to help prevent precomputated attacks
  strmInput, strmOutput: TFileStream;
  i: integer;
begin
  result := true;
  strmInput := nil;
  strmOutput := nil;
  try
    strmInput := TFileStream.Create(infile,fmOpenRead);
    strmOutput := TFileStream.Create(outfile,fmCreate);

    SetLength(HashDigest,Hash.HashSize div 8);
    for i := 0 to 7 do
      Salt[i] := Random(256);  // just fill the salt with random values (crypto secure PRNG would be better but not _really_ necessary)
    strmOutput.WriteBuffer(Salt,Sizeof(Salt));  // write out the salt so we can decrypt!
    Hash.Init;
    Hash.Update(Salt[0],Sizeof(Salt));   // hash the salt
    Hash.UpdateStr(passphrase);  // and the passphrase
    Hash.Final(HashDigest[0]);           // store the output in HashDigest

    if (Cipher is TDCP_blockcipher) then      // if the cipher is a block cipher we need an initialisation vector
    begin
      SetLength(CipherIV,TDCP_blockcipher(Cipher).BlockSize div 8);
      for i := 0 to (Length(CipherIV) - 1) do
        CipherIV[i] := Random(256);           // again just random values for the IV
      strmOutput.WriteBuffer(CipherIV[0],Length(CipherIV));  // write out the IV so we can decrypt!
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),CipherIV);  // initialise the cipher with the hash as key
      TDCP_blockcipher(Cipher).CipherMode := cmCBC;   // use CBC chaining when encrypting
    end
    else
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),nil); // initialise the cipher with the hash as key

    Cipher.EncryptStream(strmInput,strmOutput,strmInput.Size); // encrypt the entire file
    Cipher.Burn;   // important! get rid of keying information
    strmInput.Free;
    strmOutput.Free;

  except
    strmInput.Free;
    strmOutput.Free;
    result := false;
  end;
end;

function DoDecryptFile(infile,outfile,passphrase:String; Hash: TDCP_hash;Cipher: TDCP_cipher):Boolean;
var
  CipherIV: array of byte;     // the initialisation vector (for chaining modes)
  HashDigest: array of byte;   // the result of hashing the passphrase with the salt
  Salt: array[0..7] of byte;   // a random salt to help prevent precomputated attacks
  strmInput, strmOutput: TFileStream;
begin

  strmInput := nil;
  strmOutput := nil;
  try
    strmInput := TFileStream.Create(infile,fmOpenRead);
    strmOutput := TFileStream.Create(outfile,fmCreate);

    SetLength(HashDigest,Hash.HashSize div 8);
    strmInput.ReadBuffer(Salt[0],Sizeof(Salt));  // read the salt in from the file
    Hash.Init;
    Hash.Update(Salt[0],Sizeof(Salt));   // hash the salt
    Hash.UpdateStr(passphrase);  // and the passphrase
    Hash.Final(HashDigest[0]);           // store the hash in HashDigest

    if (Cipher is TDCP_blockcipher) then            // if it is a block cipher we need the IV
    begin
      SetLength(CipherIV,TDCP_blockcipher(Cipher).BlockSize div 8);
      strmInput.ReadBuffer(CipherIV[0],Length(CipherIV));       // read the initialisation vector from the file
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),CipherIV);  // initialise the cipher
      TDCP_blockcipher(Cipher).CipherMode := cmCBC;
    end
    else
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),nil);  // initialise the cipher

    Cipher.DecryptStream(strmInput,strmOutput,strmInput.Size - strmInput.Position); // decrypt!
    Cipher.Burn;
    strmInput.Free;
    strmOutput.Free;

  except
    strmInput.Free;
    strmOutput.Free;
  end;
end;

end.
