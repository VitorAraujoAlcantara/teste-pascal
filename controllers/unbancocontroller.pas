unit unBancoController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unControllerBase, unBanco, unModelBase, SQLDB;

type

  { TBancoValidator }

  TBancoValidator = class(TInterfacedObject, IModelValidator)
    procedure Validate(AModel: TModelBase; out isValid: boolean;
      out errors: TStringList);
  end;

  { TBancoController }

  TBancoController = class(TAbstractController)
  private
    FFilterCodigo: integer;
    FFilterNome: string;
    procedure SetFilterCodigo(AValue: integer);
    procedure SetFilterNome(AValue: string);
  protected
    function GetTableName: string; override;
    function GetInsertFieldList: TStringList; override;
    procedure SetInsertFieldValues(AModel: TModelBase; AQuery: TSQLQuery); override;
    procedure SetUpdateFieldValues(AModel: TModelBase; AQuery: TSQLQuery); override;
    function PopulateModel(AQuery: TSQLQuery): TModelBase; override;
    function GetDefaultOrder: string; override;
    function GetValidator: IModelValidator; override;
    procedure SetFilterParamsValues(AQuery: TSQLQuery); override;
    function GetFilterExpression: string; override;
  public
    property FilterNome: string read FFilterNome write SetFilterNome;
    property FilterCodigo: integer read FFilterCodigo write SetFilterCodigo;
  end;

  const PARAM_FILTER_CODIGO = 'codigoBanco';
  const PARAM_FILTER_NOME = 'nomeBanco';

implementation

{ TBancoValidator }

procedure TBancoValidator.Validate(AModel: TModelBase; out isValid: boolean;
  out errors: TStringList);
begin
  errors.Clear;
  with AModel as TBanco do
  begin
    if (trim(NomeBanco) = string.Empty) then
    begin
      errors.Add('O nome do banco é obrigatório!');
    end;
    if (CodigoBanco <= 0) then
    begin
      errors.Add('O código do banco é obrigatório!');
    end;
  end;

  isValid := errors.Count = 0;
end;

{ TBancoController }

procedure TBancoController.SetFilterCodigo(AValue: integer);
begin
  if FFilterCodigo=AValue then Exit;
  FFilterCodigo:=AValue;
end;

procedure TBancoController.SetFilterNome(AValue: string);
begin
  if FFilterNome=AValue then Exit;
  FFilterNome:=AValue;
end;

function TBancoController.GetTableName: string;
begin
  Result := 'banco';
end;

function TBancoController.GetInsertFieldList: TStringList;
var
  ret: TStringList;
begin
  ret := TStringList.Create;
  ret.Add('codigoBanco');
  ret.Add('nomeBanco');
  ret.Add('observacao');
  Result := ret;
end;

procedure TBancoController.SetInsertFieldValues(AModel: TModelBase; AQuery: TSQLQuery);
begin
  with AModel as TBanco do
  begin
    Aquery.ParamByName('codigoBanco').AsInteger := CodigoBanco;
    AQuery.ParamByName('nomeBanco').AsString := NomeBanco;
    AQuery.ParamByName('observacao').AsString:= Observacao;
  end;
end;

procedure TBancoController.SetUpdateFieldValues(AModel: TModelBase; AQuery: TSQLQuery);
begin
  with AModel as TBanco do
  begin
    Aquery.ParamByName('codigoBanco').AsInteger := CodigoBanco;
    AQuery.ParamByName('nomeBanco').AsString := NomeBanco;
    AQuery.ParamByName('observacao').AsString:= Observacao;
  end;
end;

function TBancoController.PopulateModel(AQuery: TSQLQuery): TModelBase;
var
  ret: TBanco;
begin
  ret := TBanco.Create;
  ret.NomeBanco := AQuery.FieldByName('nomeBanco').AsString;
  ret.CodigoBanco := AQuery.FieldByName('codigoBanco').AsInteger;
  ret.Observacao:= AQuery.FieldByName('observacao').AsString;
  Result := ret;

end;

function TBancoController.GetDefaultOrder: string;
begin
  Result := 'nomeBanco ASC';
end;

function TBancoController.GetValidator: IModelValidator;
begin
  Result := TBancoValidator.Create;
end;

procedure TBancoController.SetFilterParamsValues(AQuery: TSQLQuery);
begin
  if ( FFilterCodigo > 0 ) then
  begin
    AQuery.ParamByName(PARAM_FILTER_CODIGO).AsInteger:= FFilterCodigo;
  end;

  if ( FFilterNome <> String.Empty ) then
  begin
    AQuery.ParamByName(PARAM_FILTER_NOME).AsString:= '%'+FFilterNome+'%';
  end;
end;

function TBancoController.GetFilterExpression: string;
var
  sSql : string;
begin
  sSql := 'WHERE 1 = 1 ';
  if ( FFilterCodigo > 0 ) then
  begin
    sSql := sSql + ' AND codigoBanco = :'+ PARAM_FILTER_CODIGO;
  end;

  if ( FFilterNome <> String.Empty ) then
  begin
    sSql := sSql + ' AND UPPER(nomeBanco) like UPPER(:'+ PARAM_FILTER_NOME+')';
  end;


  Result := sSql;

end;

end.


