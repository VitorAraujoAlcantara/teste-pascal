unit unFrmPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  UnFrmBase, UnDmDAdos, DllistDyn, unCadBanco;

type

  { TFrmPrincipal }

  TFrmPrincipal = class(TFrmBase)
    ImageList1: TImageList;
    RadioGroup1: TRadioGroup;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private

  public

  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.lfm}

{ TFrmPrincipal }

procedure TFrmPrincipal.FormCreate(Sender: TObject);
begin
  DmDados := TDmDados.Create(self);
end;

procedure TFrmPrincipal.ToolButton1Click(Sender: TObject);
begin
  FrmCadBanco := TFrmCadBanco.Create(nil);
  try
    FrmCadBanco.ShowModal;
  finally
    FreeAndNil(FrmCadBanco);
  end;
end;

end.

