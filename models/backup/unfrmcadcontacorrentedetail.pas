unit UnFrmCadContaCorrenteDetail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, MaskEdit, DateTimePicker, RTTICtrls, unFrmCadDetailBase,
  unModelBase;

type

  { TFrmCadContaCorrenteDetail }

  TFrmCadContaCorrenteDetail = class(TFrmCadDetailBase)
    CmbBanco: TComboBox;
    EdtDataAbertura: TDateTimePicker;
    EdtNome: TEdit;
    EdtNumero: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LblDataAlteracao: TLabel;
    LblDataCadastro: TLabel;
    MemObservacao: TMemo;
    EdtValorAbertura: TTIFloatSpinEdit;
    procedure FormShow(Sender: TObject);
  private

  protected
    function GetModelFromScreenFields: TModelBase; override;
    procedure SetFilterParams; override;
    procedure UpdateScreen(AModel: TModelBase); override;
  public

  end;

var
  FrmCadContaCorrenteDetail: TFrmCadContaCorrenteDetail;

implementation

uses unContaCorrente, unContaCorrenteController;

{$R *.lfm}

{ TFrmCadContaCorrenteDetail }

procedure TFrmCadContaCorrenteDetail.FormShow(Sender: TObject);
var
  i:Integer;
begin
  for i := 0 to TContaCorrenteController(FController).ListaBanco.Count -1 do
  begin
    CmbBanco.Items.Add(TContaCorrenteController(FController).ListaBanco.Names[i]);
  end;

  inherited;
end;

function TFrmCadContaCorrenteDetail.GetModelFromScreenFields: TModelBase;
begin
  Result := TContaCorrente.Create;
  with Result as TContaCorrente do
  begin
    NomeConta := EdtNome.Text;
    Numero := EdtNumero.Text;
    DataAbertura := EdtDataAbertura.Date;
    SaldoInicial := StrToFloatDef(EdtValorAbertura.Text, 0);
    Observacao := MemObservacao.Text;
    if ( CmbBanco.ItemIndex > -1 ) then
    begin
      IdBanco:= TContaCorrenteController(FController).ListaBanco.ValueFromIndex[CmbBanco.ItemIndex];
    end;
  end;
end;

procedure TFrmCadContaCorrenteDetail.SetFilterParams;
begin

end;

procedure TFrmCadContaCorrenteDetail.UpdateScreen(AModel: TModelBase);
begin
  EdtNumero.Clear;
  EdtNome.Clear;
  EdtValorAbertura.Clear;
  EdtDataAbertura.Date := Now;
  MemObservacao.Clear;

  EdtNumero.Enabled:= FOperation in [edit, insert, filter];
  EdtNome.Enabled:= FOperation in [edit, insert, filter];
  EdtValorAbertura.Enabled:= FOperation in [edit, insert];
  EdtDataAbertura.Enabled:= FOperation in [edit, insert];
  MemObservacao.Enabled:= FOperation in [edit, insert] ;

  if (FOperation in [edit, view]) then
  begin
    with AModel as TContaCorrente do
    begin
      EdtNumero.Text := Numero;
      EdtNome.Text := NomeConta;
      EdtValorAbertura.Text := FloatToStr(SaldoInicial);
      MemObservacao.Text := Observacao;
      EdtDataAbertura.Date := DataAbertura;
      CmbBanco.ItemIndex:= TContaCorrenteController(FController).IndiceBanco;

      LblDataAlteracao.Caption:= 'Alt: '+ FormatDateTime('dd/mm/yyyy hh:mm:ss', DataAlteracao);
      LblDataCadastro.Caption:= 'Cad: '+ FormatDateTime('dd/mm/yyyy hh:mm:ss', DataCadastro);


    end;
  end;


end;

end.
