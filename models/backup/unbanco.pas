unit unBanco;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unModelBase;

type

    { TBanco }

    TBanco = class(TModelBase)
    private
      FCodigoBanco: integer;
      FNomeBanco: string;
      procedure SetCodigoBanco(AValue: integer);
      procedure SetNomeBanco(AValue: string);
    public
      property CodigoBanco: integer read FCodigoBanco write SetCodigoBanco;
      property NomeBanco: string read FNomeBanco write SetNomeBanco;
    end;

implementation

{ TBanco }

procedure TBanco.SetCodigoBanco(AValue: integer);
begin
  if FCodigoBanco=AValue then Exit;
  FCodigoBanco:=AValue;
end;

procedure TBanco.SetNomeBanco(AValue: string);
begin
  if FNomeBanco=AValue then Exit;
  FNomeBanco:=AValue;
end;

end.

