unit frmConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IniFiles;



type
  TConfig = class(TForm)
  private
    { Private declarations }
  public
    procedure LoadConfig(AFileName: string);
    { Public declarations }
  end;

var
  Config: TConfig;


implementation

{$R *.dfm}



end.
