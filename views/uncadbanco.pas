unit unCadBanco;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, unFrmCadBase,
  unBancoController, unDmDados, unBanco, unControllerBase, unModelBase, unFrmCadDetailBase;

type

  { TFrmCadBanco }

  TFrmCadBanco = class(TFrmCadBase)
    procedure Panel1Click(Sender: TObject);
  private
  protected
    function GetController: TAbstractController; override;
    function GetRowItens(AItem: TModelBase): TStringArray; override;
    function GetRowHeaders: TStringArray; override;
    function GetFrmDetail(AController: TAbstractController;
      AOperation: CadOperation): TFrmCadDetailBase; override;
  public

  end;

var
  FrmCadBanco: TFrmCadBanco;

implementation
uses unFrmCadDetailBanco;

{$R *.lfm}

{ TFrmCadBanco }

procedure TFrmCadBanco.Panel1Click(Sender: TObject);
begin

end;

function TFrmCadBanco.GetController: TAbstractController;
begin
  Result := TBancoController.Create(DmDados.PQConnection1);
end;

function TFrmCadBanco.GetRowItens(AItem: TModelBase): TStringArray;
var
  ret: TStringArray;
begin
  SetLength(ret, 2);
  with (AItem as TBanco) do
  begin
    ret[0] := IntToStr(CodigoBanco);
    ret[1] := NomeBanco;
  end;
  result := ret;
end;

function TFrmCadBanco.GetRowHeaders: TStringArray;
begin
  setLength(result,2);
  result[0] := 'Código';
  result[1] := 'Nome';
end;

function TFrmCadBanco.GetFrmDetail(AController: TAbstractController;
  AOperation: CadOperation): TFrmCadDetailBase;
begin
  Result := TFrmCadDetailBanco.Create(self, AController, AOperation);
end;

end.

