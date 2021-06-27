unit unFrmCadBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, Grids, unFrmBase, unControllerBase, unMOdelBase, unFrmCadDetailBase,
  Types, LCLType;

type

  TStringArray = array of string;

  { TUpdater }

  TUpdater = class(TAggregatedObject, IObserverList, IObserverCurrent)
  private
    FFrm: TForm;
    FController: TAbstractController;
  public
    procedure UpdateList(AList: TList);
    procedure UpdateCurrent(ACurrent: TModelBase);
    constructor Create(AFrm: TForm; AController: TAbstractController);
    destructor Destroy; override;
  end;

  { TFrmCadBase }

  TFrmCadBase = class(TFrmBase)
    BtnFilter: TBitBtn;
    BtnAll: TBitBtn;
    BtnInsert: TBitBtn;
    BtnSair: TBitBtn;
    BtnUpdate: TBitBtn;
    BtnDelete: TBitBtn;
    BtnSearch: TBitBtn;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    procedure BtnAllClick(Sender: TObject);
    procedure BtnFilterClick(Sender: TObject);
    procedure BtnInsertClick(Sender: TObject);
    procedure BtnSairClick(Sender: TObject);
    procedure BtnUpdateClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1Selection(Sender: TObject; aCol, aRow: integer);
  private
    FUpdater: TUpdater;
    procedure UpdateGrid(AList: TList);
  protected
    FIndexCurrent: integer;
    FController: TAbstractController;
    function GetController: TAbstractController; virtual; abstract;
    function GetRowItens(AItem: TModelBase): TStringArray; virtual; abstract;
    function GetRowHeaders: TStringArray; virtual; abstract;
    function GetFrmDetail(AController: TAbstractController;
      AOperation: CadOperation): TFrmCadDetailBase; virtual; abstract;
  public

  end;

var
  FrmCadBase: TFrmCadBase;

implementation

{$R *.lfm}

{ TUpdater }

procedure TUpdater.UpdateList(AList: TList);
begin
  TFrmCadBase(FFrm).UpdateGrid(AList);
  FController.First;
end;

procedure TUpdater.UpdateCurrent(ACurrent: TModelBase);
begin
  TFrmCadBase(FFrm).FIndexCurrent := FController.CurrentIndex;
  TFrmCadBase(FFrm).StatusBar1.SimpleText :=
    string.Format('Registro %d de %d', [FController.CurrentIndex +
    1, FController.Itens.Count]);
end;

constructor TUpdater.Create(AFrm: TForm; AController: TAbstractController);
begin
  inherited Create(AFrm);
  FFrm := AFrm;
  FController := AController;
  FController.AtachObserverOnGetAll(self);
  FController.AtachObserverCurrent(self);

end;

destructor TUpdater.Destroy;
begin
  FController.UnAtachObserverCurrent(self);
  FController.UnAtachObserverOnGetAll(self);
  FFrm := nil;
  FController := nil;
  inherited Destroy;
end;

{ TFrmCadBase }

procedure TFrmCadBase.FormCreate(Sender: TObject);
begin
  FController := GetController();
  FUpdater := TUpdater.Create(self, FController);
end;

procedure TFrmCadBase.FormDestroy(Sender: TObject);
begin
  FUpdater := nil;
end;

procedure TFrmCadBase.FormShow(Sender: TObject);
begin
  FController.GetAll;
end;

procedure TFrmCadBase.StringGrid1Selection(Sender: TObject; aCol, aRow: integer);
begin
  FController.SetPos(ARow - 1);
end;

procedure TFrmCadBase.UpdateGrid(AList: TList);
var
  i: integer;
  model: TModelBase;
  headers: TStringArray;
begin
  headers := GetRowHeaders();
  StringGrid1.Columns.Clear;
  StringGrid1.Clear;
  StringGrid1.ColCount := length(headers);
  StringGrid1.FixedCols := 0;
  StringGrid1.FixedRows := 1;
  StringGrid1.RowCount := 1;
  StringGrid1.InsertRowWithValues(0, headers);

  for i := 0 to AList.Count - 1 do
  begin
    model := TModelBase(AList[i]);
    StringGrid1.InsertRowWithValues(i + 1, GetRowItens(model));
  end;
  StringGrid1.FixedRows := 1;
  StringGrid1.RowCount:= AList.Count + 1;
  StringGrid1.AutoSizeColumns;

end;

procedure TFrmCadBase.BtnAllClick(Sender: TObject);
begin
  FController.GetAll;
end;

procedure TFrmCadBase.BtnFilterClick(Sender: TObject);
var
  frm: TFrmCadDetailBase;
begin
  try
    frm := self.GetFrmDetail(FController, filter);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TFrmCadBase.BtnInsertClick(Sender: TObject);
var
  frm: TFrmCadDetailBase;
begin
  try
    frm := self.GetFrmDetail(FController, insert);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TFrmCadBase.BtnSairClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCadBase.BtnUpdateClick(Sender: TObject);
var
  frm: TFrmCadDetailBase;
begin
  try
    frm := self.GetFrmDetail(FController, edit);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TFrmCadBase.BtnDeleteClick(Sender: TObject);
begin
  if (Application.MessageBox('Deseja realmente excluir o registro selecionado?',
    'Atenção', MB_YESNO) <> ID_YES) then
  begin
    exit;
  end;

  FController.Delete(FController.Current.Id);
  FController.GetAll;

end;

procedure TFrmCadBase.BtnSearchClick(Sender: TObject);
var
  frm: TFrmCadDetailBase;
begin
  try
    frm := self.GetFrmDetail(FController, view);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
    FController.GetAll;
  end;
end;

end.






