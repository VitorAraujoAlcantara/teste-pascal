program teste_admissao;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, runtimetypeinfocontrols, unFrmPrincipal, unFrmBase,
  unModelBase, unBanco, unControllerBase, unDmDados, unBancoController,
  unFrmCadBase, unCadBanco, unFrmCadDetailBase, unFrmCadDetailBanco,
  unContaCorrenteController, unContaCorrente, UnCadContaCorrente,
  UnFrmCadContaCorrenteDetail
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.

