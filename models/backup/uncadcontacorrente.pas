unit UnCadContaCorrente;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, unFrmCadBase,
  unControllerBase, unFrmCadDetailBase, unModelBase;

type

  { TFrmCadContaCorrente }

  TFrmCadContaCorrente = class(TFrmCadBase)
  private

  protected
    function GetController: TAbstractController; override;
    function GetFrmDetail(AControlbler: TAbstractController;
      AOperation: CadOperation): TFrmCadDetailBase; override;
    function GetRowHeaders: TStringArray; override;
    function GetRowItens(AItem: TModelBase): TStringArray; override;
  public

  end;

var
  FrmCadContaCorrente: TFrmCadContaCorrente;

implementation

uses unContaCorrenteController, unDmDados, unContaCorrente, UnFrmCadContaCorrenteDetail;

{$R *.lfm}

{ TFrmCadContaCorrente }

function TFrmCadContaCorrente.GetController: TAbstractController;
begin
  Result := TContaCorrenteController.Create(DmDados.PQConnection1);
end;

function TFrmCadContaCorrente.GetFrmDetail(AControlbler: TAbstractController;
  AOperation: CadOperation): TFrmCadDetailBase;
begin
  Result := TFrmCadContaCorrenteDetail.Create(self, AControlbler, AOperation);
end;

function TFrmCadContaCorrente.GetRowHeaders: TStringArray;
begin
  SetLength(Result, 3);
  Result[0] := 'Nome';
  Result[1] := 'Número';
  Result[2] := 'Banco';
end;

function TFrmCadContaCorrente.GetRowItens(AItem: TModelBase): TStringArray;
begin
  SetLength(Result, 3);
  with AItem as TContaCorrente do
  begin
    Result[0] := NomeConta;
    Result[1] := Numero;
    Result[2] := '';
  end;

end;

end.

