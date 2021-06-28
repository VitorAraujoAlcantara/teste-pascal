unit unContaCorrente;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unModelBase;
type

  { TContaCorrente }

  TContaCorrente = class(TModelBase)
  private
    FDataAbertura: TDateTime;
    FIdBanco: string;
    FNomeBanco: string;
    FNomeConta: string;
    FNumero: string;
    FObservacao: string;
    FSaldoInicial: Extended;
    procedure SetDataAbertura(AValue: TDateTime);
    procedure SetIdBanco(AValue: string);
    procedure SetNomeBanco(AValue: string);
    procedure SetNomeConta(AValue: string);
    procedure SetNumero(AValue: string);
    procedure SetObservacao(AValue: string);
    procedure SetSaldoInicial(AValue: Extended);
  public
    property IdBanco: string read FIdBanco write SetIdBanco;
    property NomeConta: string read FNomeConta write SetNomeConta;
    property Numero: string read FNumero write SetNumero;
    property DataAbertura: TDateTime read FDataAbertura write SetDataAbertura;
    property Observacao: string read FObservacao write SetObservacao;
    property SaldoInicial: Extended read FSaldoInicial write SetSaldoInicial;
    property NomeBanco: string read FNomeBanco write SetNomeBanco;
  end;

implementation

{ TContaCorrente }

procedure TContaCorrente.SetDataAbertura(AValue: TDateTime);
begin
  if FDataAbertura=AValue then Exit;
  FDataAbertura:=AValue;
end;

procedure TContaCorrente.SetIdBanco(AValue: string);
begin
  if FIdBanco=AValue then Exit;
  FIdBanco:=AValue;
end;

procedure TContaCorrente.SetNomeBanco(AValue: string);
begin
  if FNomeBanco=AValue then Exit;
  FNomeBanco:=AValue;
end;

procedure TContaCorrente.SetNomeConta(AValue: string);
begin
  if FNomeConta=AValue then Exit;
  FNomeConta:=AValue;
end;

procedure TContaCorrente.SetNumero(AValue: string);
begin
  if FNumero=AValue then Exit;
  FNumero:=AValue;
end;

procedure TContaCorrente.SetObservacao(AValue: string);
begin
  if FObservacao=AValue then Exit;
  FObservacao:=AValue;
end;

procedure TContaCorrente.SetSaldoInicial(AValue: Extended);
begin
  if FSaldoInicial=AValue then Exit;
  FSaldoInicial:=AValue;
end;

end.

