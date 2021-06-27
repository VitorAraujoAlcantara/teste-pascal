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
      FObservacao: string;
      procedure SetCodigoBanco(AValue: integer);
      procedure SetNomeBanco(AValue: string);
      procedure SetObservacao(AValue: string);
    public
      property CodigoBanco: integer read FCodigoBanco write SetCodigoBanco;
      property NomeBanco: string read FNomeBanco write SetNomeBanco;
      property Observacao: string read FObservacao write SetObservacao;
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

procedure TBanco.SetObservacao(AValue: string);
begin
  if FObservacao=AValue then Exit;
  FObservacao:=AValue;
end;

end.

