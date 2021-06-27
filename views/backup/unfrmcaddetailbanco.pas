unit unFrmCadDetailBanco;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, unFrmCadDetailBase, UnModelBase;

type

  { TFrmCadDetailBanco }

  TFrmCadDetailBanco = class(TFrmCadDetailBase)
    EdtNome: TEdit;
    EdtCodigo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LblDataAlteracao: TLabel;
    LblDataCadastro: TLabel;
    MemObs: TMemo;
    procedure LblEditCodigoChange(Sender: TObject);
    procedure EdtCodigoKeyPress(Sender: TObject; var Key: char);
  private

  protected
    procedure UpdateScreen(AModel: TModelBase); override;
    function GetModelFromScreenFields: TModelBase; override;
    procedure SetFilterParams; override;
  public

  end;

var
  FrmCadDetailBanco: TFrmCadDetailBanco;

implementation

uses UnBanco, unBancoController;

{$R *.lfm}

{ TFrmCadDetailBanco }

procedure TFrmCadDetailBanco.LblEditCodigoChange(Sender: TObject);
begin

end;

procedure TFrmCadDetailBanco.EdtCodigoKeyPress(Sender: TObject; var Key: char);
begin
  if (not (key in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', #46, #8])) then
  begin
    key := #0;
  end;
end;

procedure TFrmCadDetailBanco.UpdateScreen(AModel: TModelBase);
begin
  EdtNome.Text := '';
  EdtCodigo.Text := '0';
  MemObs.Text:= '';

  if (FOperation = view) then
  begin
    EdtCodigo.Enabled := False;
    EdtNome.Enabled := False;
    MemObs.Enabled := False;
  end;

  if (FOperation = filter ) then
  begin
    MemObs.Enabled := False;
  end;

  if (self.FOperation in [edit, view]) then
  begin
    with AModel as TBanco do
    begin
      EdtNome.Text := NomeBanco;
      EdtCodigo.Text := IntToStr(CodigoBanco);
      MemObs.Text:= Observacao;
      LblDataAlteracao.Caption:= 'Alt: '+ FormatDateTime('dd/mm/yyyy hh:mm:ss', DataAlteracao);
      LblDataCadastro.Caption:= 'Cad: '+ FormatDateTime('dd/mm/yyyy hh:mm:ss', DataCadastro);
    end;
  end;

  if (EdtNome.Enabled) then
  begin
    EdtNome.SetFocus;
  end;

end;

function TFrmCadDetailBanco.GetModelFromScreenFields: TModelBase;
var
  banco: TBanco;
  codigo: integer;
begin
  banco := TBanco(Self.FController.Current);
  if ( banco = nil ) then
  begin
    banco := TBanco.Create;
  end;
  banco.NomeBanco := EdtNome.Text;
  banco.CodigoBanco := 0;
  banco.Observacao:= MemObs.Text;
  if (integer.TryParse(EdtCodigo.Text, codigo)) then

  begin
    banco.CodigoBanco := codigo;
  end;
  Result := banco;
end;

procedure TFrmCadDetailBanco.SetFilterParams;
begin
  with (FController as TBancoController) do
  begin
    FilterNome := EdtNome.Text;
    FilterCodigo := StrToIntDef(EdtCodigo.Text, 0);
  end;
end;

end.
