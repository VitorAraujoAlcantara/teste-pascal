unit unFrmCadDetailBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  unFrmBase, unControllerBase, unModelBase, LCLType;

type

  CadOperation = (view, insert, edit, filter);

  { TUpdater }

  TUpdater = class(TAggregatedObject, IObserverCurrent)
  private
    FPointer: Pointer;
    FFrm: TForm;
    FController: TAbstractController;
  public
    procedure UpdateCurrent(ACurrent: TModelBase);
    constructor Create(AFrm: TForm; AController: TAbstractController);
    destructor Destroy; override;
  end;

  { TFrmCadDetailBase }

  TFrmCadDetailBase = class(TFrmBase)
    BtnRefresh: TBitBtn;
    BtnClose: TBitBtn;
    BtnOk: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    ImageList1: TImageList;
    Panel1: TPanel;
    PnlNavigator: TPanel;
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FUpdater: TUpdater;
  protected
    FController: TAbstractController;
    FOperation: CadOperation;
    procedure UpdateScreen(AModel: TModelBase); virtual; abstract;
    function GetModelFromScreenFields: TModelBase; virtual; abstract;
    procedure SetFilterParams; virtual; abstract;
  public
    constructor Create(TheOwner: TComponent; AController: TAbstractController;
      AOperation: CadOperation);
    destructor Destroy; override;
  end;

var
  FrmCadDetailBase: TFrmCadDetailBase;

implementation

{$R *.lfm}

{ TUpdater }

procedure TUpdater.UpdateCurrent(ACurrent: TModelBase);
begin
  if (ACurrent = nil) then
  begin
    exit;
  end;
  TFrmCadDetailBase(FFrm).UpdateScreen(ACurrent);
end;

constructor TUpdater.Create(AFrm: TForm; AController: TAbstractController);
begin
  inherited Create(AFrm);
  FFrm := AFrm;
  FController := AController;
  FPointer := FController.AtachObserverCurrent(self);
end;

destructor TUpdater.Destroy;
begin
  FController.UnAtachObserverCurrent(FPointer);
  FFrm := nil;
  FController := nil;
  inherited Destroy;
end;

{ TFrmCadDetailBase }

procedure TFrmCadDetailBase.FormShow(Sender: TObject);
begin
  UpdateScreen(FController.Current);
  case (FOperation) of
    insert,
    filter:
    begin
      PnlNavigator.Visible := False;
      BtnRefresh.Visible:= False;
    end;
    edit:
    begin
      PnlNavigator.Visible:= False;
      BtnRefresh.Visible:= True;
    end;
    view:
    begin
      PnlNavigator.Visible:= True;
      BtnRefresh.Visible:= False;
      BtnOk.Visible:= False;
    end;

  end;
end;

procedure TFrmCadDetailBase.BtnOkClick(Sender: TObject);
var
  model: TModelBase;
begin
  try
    case FOperation of
      Insert:
      begin
        FController.Insert(GetModelFromScreenFields());
        Close();
      end;
      Edit:
      begin
        model := GetModelFromScreenFields();
        model.Id := FController.Current.id;
        FController.Update(model);
        Close();
      end;
      filter:
      begin
        SetFilterParams;
        FController.GetByFilter;
        Close();
      end;
    end;

  except
    on e: EValidationError do
    begin
      Application.MessageBox(PChar(FController.Errors.Text),
        'Erros de validação', MB_ICONERROR + MB_OK);
    end;
  end;
end;

procedure TFrmCadDetailBase.BtnRefreshClick(Sender: TObject);
begin
  case FOperation of
    Insert:
    begin

    end;
    Edit:
    begin
      FController.Get(FController.Current.Id);
      UpdateScreen(FController.Current);
    end;
  end;
end;

procedure TFrmCadDetailBase.BtnCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TFrmCadDetailBase.BitBtn4Click(Sender: TObject);
begin
  FController.First;
end;

procedure TFrmCadDetailBase.BitBtn5Click(Sender: TObject);
begin
  FController.Prior;
end;

procedure TFrmCadDetailBase.BitBtn6Click(Sender: TObject);
begin
  FController.Next;
end;

procedure TFrmCadDetailBase.BitBtn7Click(Sender: TObject);
begin
  FController.Last;
end;

constructor TFrmCadDetailBase.Create(TheOwner: TComponent;
  AController: TAbstractController; AOperation: CadOperation);
begin
  inherited Create(TheOwner);
  FController := AController;
  FOperation := AOperation;
  FUpdater := TUpdater.Create(self, FController);
end;

destructor TFrmCadDetailBase.Destroy;
begin
  FUpdater.Free;
  FUpdater := nil;
  inherited Destroy;
end;

end.
