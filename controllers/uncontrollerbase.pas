unit unControllerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unModelBase, SQLDB;

type

  EValidationError = class(Exception)

  end;

  { IObserverList }

  IObserverList = interface
    ['{A491C2B6-243D-2D07-165C-3ACCA292BA8C}']
    procedure UpdateList(AList: TList);
  end;

  { IObserverCurrent }

  IObserverCurrent = interface
    ['{F0773E16-B809-BADB-45E6-0315E53B831D}']
    procedure UpdateCurrent(ACurrent: TModelBase);
  end;

  { IModelValidator }

  IModelValidator = interface
    ['{D9E67C45-89EA-2B87-E0F5-55E10F945453}']
    procedure Validate(AModel: TModelBase; out isValid: boolean; out errors: TStringList);
  end;

  { TAbstractController }

  TAbstractController = class
  private
    FConnection: TSQLConnection;
    FItens: TList;
    FCurrent: TModelBase;
    FCurrentIndex: integer;
    FObserverUpdateList: TList;
    FObserverCurrent: TList;
    FErrors: TStringList;
    function GetGuid(): TGUID;
    function ValidateModel(AModel: TModelBase): Boolean;
  protected
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
    function GetTableName: string; virtual; abstract;
    function GetInsertFieldList: TStringList; virtual; abstract;
    procedure SetInsertFieldValues(AModel: TModelBase; AQuery: TSQLQuery);
      virtual; abstract;
    procedure SetUpdateFieldValues(AModel: TModelBase; AQuery: TSQLQuery);
      virtual; abstract;
    procedure SetFilterParamsValues(AQuery: TSQLQuery);virtual; abstract;
    function PopulateModel(AQuery: TSQLQuery): TModelBase; virtual; abstract;
    function GetDefaultOrder: string; virtual;
    function GetValidator: IModelValidator; virtual;abstract;
    function GetFilterExpression: string; virtual;abstract;
    procedure SetCommonsValues(AQuery: TSQLQuery; AModel: TModelBase);
  public
    function Insert(AModel: TModelBase): TModelBase;
    function Update(AModel: TModelBase): TModelBase;
    function Delete(AId: string): boolean;
    function Get(AId: string): TModelBase;
    procedure GetAll;
    procedure GetByFilter;

    procedure Next;
    procedure Prior;
    procedure Last;
    procedure First;
    procedure SetPos(AIndex: integer);

    function AtachObserverOnGetAll(AItem: IObserverList): Pointer;
    procedure UnAtachObserverOnGetAll(APointer: Pointer);
    procedure NotifyOnGetAll;

    function AtachObserverCurrent(AItem: IObserverCurrent): Pointer;
    procedure UnAtachObserverCurrent(APointer: Pointer);
    procedure NotifyOnCurrent;

    constructor Create(AConnection: TSQLConnection);virtual;
    destructor Destroy; override;

    property Itens: TList read FItens;
    property Current: TModelBase read FCurrent;
    property CurrentIndex: integer read FCurrentIndex;
    property Errors: TStringList read FErrors;

  end;

implementation



{ TAbstractController }

function TAbstractController.GetGuid(): TGUID;
var
  guid: TGuid;
begin
  CreateGuid(guid);
  Result := guid;
end;

function TAbstractController.ValidateModel(AModel: TModelBase): Boolean;
var
  isValid: boolean;
begin
  GetValidator().Validate(AModel, isValid, FErrors);
  result := isValid;
end;

function TAbstractController.GetDefaultOrder: string;
begin
  Result := '';
end;

procedure TAbstractController.SetCommonsValues(AQuery: TSQLQuery;
  AModel: TModelBase);
begin
  AModel.DataCadastro:= AQuery.FieldByName('DataCadastro').AsDateTime;
  AModel.DataAlteracao:= AQuery.FieldByName('DataAlteracao').AsDateTime;
end;

function TAbstractController.Insert(AModel: TModelBase): TModelBase;
const
  TEMPLATE = 'INSERT INTO %s ( %s ) VALUES ( %s )';
var
  sql: string;
  sValues: string;
  sParams: string;
  i: integer;
  fieldList: TStringList;
  query: TSQLQuery;
  newId: TGuid;
  sId: string;
begin
  if ( not ValidateModel(AModel) ) then
  begin
     raise EValidationError.Create('Ocorreu erro de validação!');
  end;
  newId := GetGuid();
  sId := newId.ToString(True);
  sql := '';
  sValues := 'id, DataCadastro, DataAlteracao';
  sParams := ':id, :DataCadastro, :DataAlteracao';
  query := TSQLQuery.Create(nil);
  query.SQLConnection := FConnection;
  query.Transaction := FTransaction;
  try
    fieldList := GetInsertFieldList();
    for i := 0 to fieldList.Count - 1 do
    begin
      sValues := sValues + ', ' + fieldList[i];
      sParams := sParams + ', ' + ':' + fieldList[i];
    end;

    sql := string.Format(TEMPLATE, [GetTableName(), sValues, sParams]);

    try
      if (not FTransaction.Active) then
      begin
        FTransaction.StartTransaction;
      end;
      query.SQL.Text := sql;
      query.Prepare;
      SetInsertFieldValues(AModel, query);
      query.ParamByName('id').AsString := sId;
      query.ParamByName('DataCadastro').AsDateTime := now;
      query.ParamByName('DataAlteracao').AsDateTime := now;
      query.ExecSQL;
      FTransaction.CommitRetaining;
      Result := Get(sId);
    except
      FTransaction.RollbackRetaining;
      raise;
    end;

  finally
    query.Free;
    query := nil;
  end;
end;

function TAbstractController.Update(AModel: TModelBase): TModelBase;
const
  TEMPLATE = 'UPDATE %s SET DataAlteracao = :DataAlteracao %s  WHERE id = :id';
var
  sql: string;
  sValues: string;
  i: integer;
  fieldList: TStringList;
  query: TSQLQuery;
begin
  if ( not ValidateModel(AModel) ) then
  begin
     raise EValidationError.Create('Ocorreu erro de validação!');
  end;
  sql := '';
  sValues := '';
  query := TSQLQuery.Create(nil);
  query.SQLConnection := FConnection;
  query.Transaction := FTransaction;
  try
    fieldList := GetInsertFieldList();
    for i := 0 to fieldList.Count - 1 do
    begin
      sValues := sValues + ', ' + fieldList[i] + ' = :' + fieldList[i];
    end;

    sql := string.Format(TEMPLATE, [GetTableName(), sValues]);

    try
      if (not FTransaction.Active) then
      begin
        FTransaction.StartTransaction;
      end;
      query.SQL.Text := sql;
      query.Prepare;
      query.ParamByName('DataAlteracao').AsDateTime:= now;
      SetUpdateFieldValues(AModel, query);
      query.ParamByName('id').AsString := AModel.Id;
      query.ExecSQL;
      FTransaction.CommitRetaining;
      Result := Get(AModel.Id);
    except
      FTransaction.RollbackRetaining;
      raise;
    end;

  finally
    query.Free;
    query := nil;
  end;
end;

function TAbstractController.Delete(AId: string): boolean;
const
  TEMPLATE = 'DELETE FROM  %s WHERE id = :id';
var
  sql: string;
  query: TSQLQuery;
begin
  sql := '';
  query := TSQLQuery.Create(nil);
  query.SQLConnection := FConnection;
  query.Transaction := FTransaction;
  try
    sql := string.Format(TEMPLATE, [GetTableName()]);
    try
      if (not FTransaction.Active) then
      begin
        FTransaction.StartTransaction;
      end;
      query.SQL.Text := sql;
      query.Prepare;
      query.ParamByName('id').AsString := AId;
      query.ExecSQL;
      FTransaction.CommitRetaining;
      Result := True;
    except
      FTransaction.RollbackRetaining;
      raise;
    end;

  finally
    query.Free;
    query := nil;
  end;
end;

function TAbstractController.Get(AId: string): TModelBase;
const
  TEMPLATE = 'SELECT *  FROM %s WHERE id = :id';
var
  sql: string;
  sValues: string;
  query: TSQLQuery;
begin
  sql := '';
  sValues := '';
  query := TSQLQuery.Create(nil);
  query.SQLConnection := FConnection;
  query.Transaction := FTransaction;
  try
    sql := string.Format(TEMPLATE, [GetTableName(), sValues]);
    try
      query.SQL.Text := sql;
      query.Prepare;
      query.ParamByName('id').AsString := AId;
      query.Open;
      Result := PopulateModel(query);
      Result.Id := query.FieldByName('id').AsString;
      SetCommonsValues(query, Result);
    except
      raise;
    end;

  finally
    query.Free;
    query := nil;
  end;
end;

procedure TAbstractController.GetAll;
const
  TEMPLATE = 'SELECT cast(id as varchar) as _id, *  FROM %s %s';
var
  sql: string;
  sOrder: string;
  query: TSQLQuery;
  model: TModelBase;
begin
  sql := '';
  sOrder := '';
  query := TSQLQuery.Create(nil);
  query.SQLConnection := FConnection;
  query.Transaction := FTransaction;
  FCurrent := nil;
  FCurrentIndex := -1;
  if (GetDefaultOrder <> string.Empty) then
  begin
    sOrder := 'ORDER BY ' + GetDefaultOrder;
  end;
  if (Assigned(FItens)) then
  begin
    FItens.Clear;
    FreeAndNil(FItens);
  end;
  FItens := TList.Create;
  try
    sql := string.Format(TEMPLATE, [GetTableName(), sOrder]);
    try
      query.SQL.Text := sql;
      query.Prepare;
      query.ExecSQL;
      query.Open;
      query.First;
      query.DisableControls;
      while not query.EOF do
      begin
        model := PopulateModel(query);
        model.Id := query.FieldByName('_id').AsString;
        SetCommonsValues(query, model);
        FItens.Add(model);
        query.Next;
      end;

      if (FItens.Count > 0) then
      begin
        FCurrent := TModelBase(FItens.First);
        FCurrentIndex := 0;
      end;

      NotifyOnGetAll();

    except
      raise;
    end;

  finally
    query.Free;
    query := nil;
  end;
end;

procedure TAbstractController.GetByFilter;
const
  TEMPLATE = 'SELECT cast(id as varchar) as _id, *  FROM %s %s %s';
var
  sql: string;
  sOrder: string;
  query: TSQLQuery;
  model: TModelBase;
  sFilter: string;
begin
  sql := '';
  sOrder := '';
  sFilter := GetFilterExpression;
  query := TSQLQuery.Create(nil);
  query.SQLConnection := FConnection;
  query.Transaction := FTransaction;
  FCurrent := nil;
  FCurrentIndex := -1;
  if (GetDefaultOrder <> string.Empty) then
  begin
    sOrder := 'ORDER BY ' + GetDefaultOrder;
  end;
  if (Assigned(FItens)) then
  begin
    FItens.Clear;
    FreeAndNil(FItens);
  end;
  FItens := TList.Create;
  try
    sql := string.Format(TEMPLATE, [GetTableName(), sFilter, sOrder]);
    try
      query.SQL.Text := sql;
      query.Prepare;
      SetFilterParamsValues(query);
      query.ExecSQL;
      query.Open;
      query.First;
      query.DisableControls;
      while not query.EOF do
      begin
        model := PopulateModel(query);
        model.Id := query.FieldByName('_id').AsString;
        SetCommonsValues(query, model);
        FItens.Add(model);
        query.Next;
      end;

      if (FItens.Count > 0) then
      begin
        FCurrent := TModelBase(FItens.First);
        FCurrentIndex := 0;
      end;

      NotifyOnGetAll();

    except
      raise;
    end;

  finally
    query.Free;
    query := nil;
  end;
end;

procedure TAbstractController.Next;
begin
  try

    FCurrent := nil;
    if (not Assigned(FItens)) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    if (FItens.Count = 0) then
    begin
      exit;
    end;

    if (FCurrentIndex < FItens.Count - 1) then
    begin
      FCurrentIndex := FCurrentIndex + 1;
    end;

    FCurrent := TModelBase(FItens.Items[FCurrentIndex]);

  finally
    NotifyOnCurrent();
  end;

end;

procedure TAbstractController.Prior;
begin
  try
    FCurrent := nil;
    if (not Assigned(FItens)) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    if (FItens.Count = 0) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    if (FCurrentIndex > 0) then
    begin
      FCurrentIndex := FCurrentIndex - 1;
    end;

    FCurrent := TModelBase(FItens.Items[FCurrentIndex]);

  finally
    NotifyOnCurrent();
  end;
end;

procedure TAbstractController.Last;
begin
  try
    FCurrent := nil;
    if (not Assigned(FItens)) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    if (FItens.Count = 0) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    FCurrentIndex := FItens.Count - 1;
    FCurrent := TModelBase(FItens.Items[FCurrentIndex]);

  finally
    NotifyOnCurrent();
  end;
end;

procedure TAbstractController.First;
begin
  try
    FCurrent := nil;
    if (not Assigned(FItens)) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    if (FItens.Count = 0) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    FCurrentIndex := 0;
    FCurrent := TModelBase(FItens.Items[FCurrentIndex]);

  finally
    NotifyOnCurrent();
  end;
end;

procedure TAbstractController.SetPos(AIndex: integer);
begin
  try
    FCurrent := nil;
    if (not Assigned(FItens)) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    if (FItens.Count = 0) then
    begin
      FCurrentIndex := -1;
      exit;
    end;

    FCurrentIndex := AIndex;

    if (AIndex > FItens.Count - 1) then
    begin
      FCurrentIndex := FItens.Count - 1;
    end;

    FCurrent := TModelBase(FItens.Items[FCurrentIndex]);

  finally
    NotifyOnCurrent();
  end;
end;

function TAbstractController.AtachObserverOnGetAll(AItem: IObserverList): Pointer;
var
  index: integer;
begin
  if (FObserverUpdateList.IndexOf(AItem) > -1) then
  begin
    Result := nil;
    exit;
  end;
  index := FObserverUpdateList.Add(AItem);
  Result := FObserverUpdateList.Items[index];
end;

procedure TAbstractController.UnAtachObserverOnGetAll(APointer: Pointer);
begin
  if (FObserverUpdateList.IndexOf(APointer) > -1) then
    FObserverUpdateList.Delete(FObserverUpdateList.IndexOf(APointer));
end;

procedure TAbstractController.NotifyOnGetAll;
var
  i: integer;
begin
  for i := 0 to FObserverUpdateList.Count - 1 do
  begin
    IObserverList(FObserverUpdateList.Items[i]).UpdateList(FItens);
  end;
end;

function TAbstractController.AtachObserverCurrent(AItem: IObserverCurrent): Pointer;
var
  index: integer;
begin
  if (FObserverCurrent.IndexOf(AItem) > -1) then
  begin
    Result := nil;
    exit;
  end;
  index := FObserverCurrent.Add(AItem);
  Result := FObserverCurrent.Items[index];
end;

procedure TAbstractController.UnAtachObserverCurrent(APointer: Pointer);
begin
  if (FObserverCurrent.IndexOf(APointer) > -1) then
    FObserverCurrent.Delete(FObserverCurrent.IndexOf(APointer));
end;

procedure TAbstractController.NotifyOnCurrent;
var
  i: integer;
begin
  for i := 0 to FObserverCurrent.Count - 1 do
  begin
    IObserverCurrent(FObserverCurrent[i]).UpdateCurrent(FCurrent);
  end;

end;

constructor TAbstractController.Create(AConnection: TSQLConnection);
begin
  FConnection := AConnection;
  FTransaction := TSqlTransaction.Create(FConnection);
  FQuery := TSqlQuery.Create(FConnection);
  FTransaction.SQLConnection := FConnection;
  FQuery.SQLConnection := FConnection;
  FQuery.Transaction := FTransaction;
  FObserverUpdateList := TList.Create;
  FObserverCurrent := TList.Create;
  FErrors := TStringList.Create;
end;

destructor TAbstractController.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FQuery);
  FObserverUpdateList.Clear;
  FObserverUpdateList.Free;
  FObserverCurrent.Clear;
  FObserverCurrent.Free;
  FErrors.Clear;
  FErrors.Free;
  inherited Destroy;
end;

end.







