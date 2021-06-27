unit unDmDados;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PQConnection, postgres3dyn;

type

  { TDmDados }

  TDmDados = class(TDataModule)
    PQConnection1: TPQConnection;
    procedure DataModuleCreate(Sender: TObject);
  private

  public

  end;

var
  DmDados: TDmDados;

implementation

{$R *.lfm}

{ TDmDados }

procedure TDmDados.DataModuleCreate(Sender: TObject);
begin
     InitialisePostgres3('/Library/PostgreSQL/10/lib/libpq.5.10.dylib');
     PQConnection1.Open;
end;

end.

