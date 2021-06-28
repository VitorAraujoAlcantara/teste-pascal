unit unDmDados;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PQConnection, odbcconn, postgres3dyn;

type

  { TDmDados }

  TDmDados = class(TDataModule)
    ODBCConnection1: TODBCConnection;
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
     // Alterar para ambiente do cliente.
     InitialisePostgres3('/Library/PostgreSQL/10/lib/libpq.5.10.dylib');
     PQConnection1.Open;
end;

end.

