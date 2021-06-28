unit unContaCorrenteController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unControllerBase, unModelBase, SQLDB;

type

  { TLoadBanco }

  TLoadBanco = class(TInterfacedObject, IObserverCurrent, IObserverList)
  private
    FConnection: TSqlConnection;
    FCurrent: string;
    FList: TStringList;
    FIdxSelected: integer;
    procedure SetCurrent(AValue: string);
    procedure SetIdxSelected(AValue: integer);
    procedure SetList(AValue: TStringList);
    procedure LoadList;
  protected
    procedure UpdateCurrent(ACurrent: TModelBase);
    procedure UpdateList(AList: TList);
    property Current: string read FCurrent write SetCurrent;
    property List: TStringList read FList write SetList;
    property IdxSelected: integer read FIdxSelected write SetIdxSelected;
  public
    constructor Create(AConnection: TSqlConnection);
  end;

  { TContaCorrenteValidator }

  TContaCorrenteValidator = class(TInterfacedObject, IModelValidator)
  public
    procedure Validate(AModel: TModelBase; out isValid: boolean;
      out errors: TStringList);
  end;

  { TContaCorrenteController }

  TContaCorrenteController = class(TAbstractController)
  private
    FPointerCurrent: Pointer;
    FFilterIdBanco: string;
    FFilterNomeConta: string;
    FFilterNumeroConta: string;
    FLoadBanco: TLoadBanco;
    function GetIndiceBanco: integer;
    function GetListaBanco: TStringList;
    function GetNomeBanco: string;
    procedure SetFilterIdBanco(AValue: string);
    procedure SetFilterNomeConta(AValue: string);
    procedure SetFilterNumeroConta(AValue: string);

  protected
    function GetTemplateSelect: string; override;
    function GetTableName: string; override;
    function GetInsertFieldList: TStringList; override;
    procedure SetInsertFieldValues(AModel: TModelBase; AQuery: TSQLQuery);
      override;
    procedure SetUpdateFieldValues(AModel: TModelBase; AQuery: TSQLQuery);
      override;
    procedure SetFilterParamsValues(AQuery: TSQLQuery); override;
    function GetFilterExpression: string; override;
    function GetValidator: IModelValidator; override;
    function PopulateModel(AQuery: TSQLQuery): TModelBase; override;

  public
    property FilterIdBanco: string read FFilterIdBanco write SetFilterIdBanco;
    property FilterNomeConta: string read FFilterNomeConta write SetFilterNomeConta;
    property FilterNumeroConta: string read FFilterNumeroConta
      write SetFilterNumeroConta;
    property NomeBanco: string read GetNomeBanco;
    property ListaBanco: TStringList read GetListaBanco;
    property IndiceBanco:integer read GetIndiceBanco;
    constructor Create(AConnection: TSQLConnection); override;
    destructor Destroy; override;
  end;

implementation

uses unContaCorrente;

const
  FIELD_ID_BANCO = 'idBanco';
  FIELD_NOME_CONTA = 'nomeConta';
  FIELD_NUMERO = 'numero';
  FIELD_DATA_ABERTURA = 'dataAbertura';
  FIELD_OBSERVACAO = 'observacao';
  FIELD_SALDO_INICIAL = 'saldoInicial';

{ TLoadBanco }

procedure TLoadBanco.SetCurrent(AValue: string);
begin
  if FCurrent = AValue then
    Exit;
  FCurrent := AValue;
end;

procedure TLoadBanco.SetIdxSelected(AValue: integer);
begin
  if FIdxSelected = AValue then
    Exit;
  FIdxSelected := AValue;
end;

procedure TLoadBanco.SetList(AValue: TStringList);
begin
  if FList = AValue then
    Exit;
  FList := AValue;
end;

procedure TLoadBanco.LoadList;
var
  query: TSQLQuery;
begin
  if (Assigned(FList)) then
  begin
    exit;
  end;

  FList := TStringList.Create;
  query := TSQLQuery.Create(nil);
  try
    query.SQLConnection := FConnection;
    query.SQL.Text := 'SELECT CAST ( id as VARCHAR) as _id, nomeBanco FROM banco ORDER BY nomeBanco';
    query.Open;
    query.First;

    while not query.EOF do
    begin
      FList.AddPair(query.FieldByName('nomeBanco').AsString, query.FieldByName('_id').AsString );
      query.Next;
    end;

  finally
    query.Free;
  end;
end;

procedure TLoadBanco.UpdateCurrent(ACurrent: TModelBase);
var
  pos: Integer;
begin
  LoadList;
  with ACurrent as TContaCorrente do
  begin
    for pos := 0 to FList.Count -1 do
    begin
      if ( FList.ValueFromIndex[pos] = IdBanco ) then
      begin
        Current:= FList[pos];
        IdxSelected:= pos;
        Break;
      end;
    end;
  end;
end;

procedure TLoadBanco.UpdateList(AList: TList);
begin
  LoadList;
end;

constructor TLoadBanco.Create(AConnection: TSqlConnection);
begin
  inherited Create;
  FConnection := AConnection;
  LoadList;
end;

{ TContaCorrenteValidator }

procedure TContaCorrenteValidator.Validate(AModel: TModelBase;
  out isValid: boolean; out errors: TStringList);
begin
  if (Assigned(errors)) then
  begin
    errors.Free;
  end;
  errors := TStringList.Create;
  errors.Clear;
  with AModel as TContaCorrente do
  begin
    if (IdBanco = string.Empty) then
    begin
      errors.Add('O banco da conta corrente é obrigatório.');
    end;

    if (NomeConta = string.Empty) then
    begin
      errors.Add('O nome da conta é obrigatório.');
    end;

    if (Numero = string.Empty) then
    begin
      errors.Add('O número da conta é obrigatório.');
    end;
  end;

  isValid := errors.Count = 0;
end;

{ TContaCorrenteController }

procedure TContaCorrenteController.SetFilterIdBanco(AValue: string);
begin
  if FFilterIdBanco = AValue then
    Exit;
  FFilterIdBanco := AValue;
end;

function TContaCorrenteController.GetNomeBanco: string;
begin
  if ( ASsigned(FLoadBanco)) then
  begin
    Result := FLoadBanco.Current;
    exit;
  end;
  result := string.Empty;
end;

function TContaCorrenteController.GetListaBanco: TStringList;
begin
  Result := FLoadBanco.List;
end;

function TContaCorrenteController.GetIndiceBanco: integer;
begin
  if ( ASsigned(FLoadBanco)) then
   begin
     Result := FLoadBanco.IdxSelected;
     exit;
   end;
   result := -1;
end;

procedure TContaCorrenteController.SetFilterNomeConta(AValue: string);
begin
  if FFilterNomeConta = AValue then
    Exit;
  FFilterNomeConta := AValue;
end;

procedure TContaCorrenteController.SetFilterNumeroConta(AValue: string);
begin
  if FFilterNumeroConta = AValue then
    Exit;
  FFilterNumeroConta := AValue;
end;

function TContaCorrenteController.GetTemplateSelect: string;
begin
  Result := 'SELECT cast(id as varchar) as _id, cast(idBanco as varchar) as _idBanco,  *  FROM %s ';
end;

constructor TContaCorrenteController.Create(AConnection: TSQLConnection);
begin
  inherited Create(AConnection);
  FLoadBanco := TLoadBanco.Create(AConnection);
  FPointerCurrent :=  AtachObserverCurrent(FLoadBanco);
end;

destructor TContaCorrenteController.Destroy;
begin
  UnAtachObserverCurrent(FPointerCurrent);
  inherited Destroy;
end;

function TContaCorrenteController.GetTableName: string;
begin
  Result := 'conta_corrente';
end;

function TContaCorrenteController.GetInsertFieldList: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(FIELD_ID_BANCO);
  Result.Add(FIELD_NOME_CONTA);
  Result.Add(FIELD_NUMERO);
  Result.Add(FIELD_DATA_ABERTURA);
  Result.Add(FIELD_OBSERVACAO);
  Result.Add(FIELD_SALDO_INICIAL);
end;

procedure TContaCorrenteController.SetInsertFieldValues(AModel: TModelBase;
  AQuery: TSQLQuery);
begin
  with AModel as TContaCorrente do
  begin
    AQuery.ParamByName(FIELD_ID_BANCO).AsString := IdBanco;
    AQuery.ParamByName(FIELD_NOME_CONTA).AsString := NomeConta;
    AQuery.ParamByName(FIELD_NUMERO).AsString := Numero;
    AQuery.ParamByName(FIELD_DATA_ABERTURA).AsDateTime := DataAbertura;
    AQuery.ParamByName(FIELD_OBSERVACAO).AsString := Observacao;
    AQuery.ParamByName(FIELD_SALDO_INICIAL).AsFloat := SaldoInicial;
  end;

end;

procedure TContaCorrenteController.SetUpdateFieldValues(AModel: TModelBase;
  AQuery: TSQLQuery);
begin
  with AModel as TContaCorrente do
  begin
    AQuery.ParamByName(FIELD_ID_BANCO).AsString := IdBanco;
    AQuery.ParamByName(FIELD_NOME_CONTA).AsString := NomeConta;
    AQuery.ParamByName(FIELD_NUMERO).AsString := Numero;
    AQuery.ParamByName(FIELD_DATA_ABERTURA).AsDateTime := DataAbertura;
    AQuery.ParamByName(FIELD_OBSERVACAO).AsString := Observacao;
    AQuery.ParamByName(FIELD_SALDO_INICIAL).AsFloat := SaldoInicial;
  end;
end;

procedure TContaCorrenteController.SetFilterParamsValues(AQuery: TSQLQuery);
begin
  if (FilterIdBanco <> string.Empty) then
  begin
    AQuery.FieldByName(FIELD_ID_BANCO).AsString := FilterIdBanco;
  end;

  if (FilterNomeConta <> string.Empty) then
  begin
    AQuery.FieldByName(FIELD_NOME_CONTA).AsString := FilterNomeConta;
  end;

  if (FilterNumeroConta <> string.Empty) then
  begin
    AQuery.FieldByName(FIELD_NUMERO).AsString := FilterNumeroConta;
  end;
end;



function TContaCorrenteController.GetFilterExpression: string;
begin
  Result := 'WHERE 1 = 1 ';

  if (FilterIdBanco <> string.Empty) then
  begin
    Result := ' AND ' + FIELD_ID_BANCO + ' = :' + FIELD_ID_BANCO;
  end;

  if (FilterNomeConta <> string.Empty) then
  begin
    Result := ' AND UPPER(' + FIELD_NOME_CONTA + ') LIKE UPPER(:' +
      FIELD_NOME_CONTA + ') ';
  end;

  if (FilterNumeroConta <> string.Empty) then
  begin
    Result := ' AND ' + FIELD_NUMERO + ' = :' + FIELD_NUMERO;
  end;

end;

function TContaCorrenteController.GetValidator: IModelValidator;
begin
  Result := TContaCorrenteValidator.Create;
end;

function TContaCorrenteController.PopulateModel(AQuery: TSQLQuery): TModelBase;
begin
  Result := TContaCorrente.Create;
  with Result as TContaCorrente do
  begin
    IdBanco := AQuery.FieldByName('_idBanco').AsString;
    NomeConta := AQuery.FieldByName(FIELD_NOME_CONTA).AsString;
    Numero := AQuery.FieldByName(FIELD_NUMERO).AsString;
    DataAbertura := AQuery.FieldByName(FIELD_DATA_ABERTURA).AsDateTime;
    Observacao := AQuery.FieldByName(FIELD_OBSERVACAO).AsString;
    SaldoInicial := AQuery.FieldByName(FIELD_SALDO_INICIAL).AsFloat;
  end;
end;


end.
