unit unModelBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

    { TModelBase }

    TModelBase = class
    private
      FDataAlteracao: TDateTime;
      FDataCadastro: TDateTime;
      FId: string;
      procedure SetDataAlteracao(AValue: TDateTime);
      procedure SetDataCadastro(AValue: TDateTime);
      procedure SetId(AValue: string);
    public
      property Id: string read FId write SetId;
      property DataCadastro: TDateTime read FDataCadastro write SetDataCadastro;
      property DataAlteracao: TDateTime read FDataAlteracao write SetDataAlteracao;

    end;

implementation

{ TModelBase }

procedure TModelBase.SetId(AValue: integer);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TModelBase.SetDataAlteracao(AValue: TDateTime);
begin
  if FDataAlteracao=AValue then Exit;
  FDataAlteracao:=AValue;
end;

procedure TModelBase.SetDataCadastro(AValue: TDateTime);
begin
  if FDataCadastro=AValue then Exit;
  FDataCadastro:=AValue;
end;

end.

