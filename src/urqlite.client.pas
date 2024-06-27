unit urqlite.client;
// ****************************************************************************
// This unit contains the client class and interface of the rqliteClient4Delphi
// The TRqliteClientFactory class is able to create an instance of the class
// ****************************************************************************
// Created by: OwlHatSoftware
// License: GNU General Public License v3.0
// ****************************************************************************

interface

uses
  System.Generics.Collections,
  System.Classes,
  System.SysUtils,
  System.JSON,
  urqlite.net;

type
  TRow = record
  private
    FValues: TArray<string>;
    function GetColValue(idx: integer): string;
  public
    constructor Create(Values: TArray<string>);
    property Value[idx: integer]: string read GetColValue;
  end;

  IRqliteClient = interface
    ['{002E26F4-55AB-4FFB-8324-99413E5F91AA}']
    procedure SetDatabase(const Value: string);
    procedure SetHostname(const Value: string);
    procedure SetPort(const Value: integer);
    function GetDatabase: string;
    function GetHostname: string;
    function GetPort: integer;
    function GetColumnNames: TList<string>;
    function GetColumnTypes: TList<string>;
    function GetRowCount: integer;
    function GetRow(idx: integer): TRow;
    function Execute(const QueryStr: TStringList;
      AsTransaction: boolean = false): boolean;
    function Query(const QueryStr: TStringList): boolean;
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
  end;

  TRqliteClientFactory = class
    class function CreateInstance(const AHttpClient: IHttpClient)
      : IRqliteClient;
  end;

  // Conversion functions
function BytesToHexString(ByteData: TBytes): String;
function Base64ToBytes(ABase64String: string): TBytes;

implementation

uses IdCoder, IdCoderMIME;

type
  TRqliteClient = class(TInterfacedObject, IRqliteClient)
  private
    FColumns: TList<string>;
    FTypes: TList<string>;
    FValues: TList<TRow>;
    FHttpClient: IHttpClient;
    FPort: integer;
    FDatabase: string;
    FHostname: string;
    procedure SetDatabase(const Value: string);
    procedure SetHostname(const Value: string);
    procedure SetPort(const Value: integer);
    function GetDatabase: string;
    function GetHostname: string;
    function GetPort: integer;
    function CreateJSONArray(AStrings: TStrings): TJSONArray;
    function TryGetValueFromJSONArray<T>(const AName: string;
      JSONArr: TJSONArray; out Val: T): boolean;
  public
    function GetColumnNames: TList<string>;
    function GetColumnTypes: TList<string>;
    function GetRowCount: integer;
    function GetRow(idx: integer): TRow;
    function Execute(const QueryStr: TStringList;
      AsTransaction: boolean = false): boolean;
    function Query(const QueryStr: TStringList): boolean;
    constructor Create(const AHttpClient: IHttpClient);
    destructor Destroy; override;
  published
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
  end;

function BytesToHexString(ByteData: TBytes): String;
(*
  referenced source : heidisql
  unit : dbconnection
  function : TDBQuery.HexValue(var ByteData: TBytes): String;
  Purpose: Convert Byte data to a Hexidecimal string representation to
  write raw Blob data.
*)
var
  BinLen: integer;
  Ansi: AnsiString;
begin
  BinLen := Length(ByteData);
  SetString(Ansi, PAnsiChar(ByteData), BinLen);

  if BinLen = 0 then
  begin
    Result := 'null';
  end
  else
  begin
    SetLength(Result, BinLen * 2);
    BinToHex(PAnsiChar(Ansi), PChar(Result), BinLen);
    Result := 'X''' + Result + '''';
  end;
end;

function Base64ToBytes(ABase64String: string): TBytes;
begin
  Result := TBytes(TIdDecoderMIME.DecodeBytes(ABase64String));
end;

{ TRqliteClient }

constructor TRqliteClient.Create(const AHttpClient: IHttpClient);
begin
  FHttpClient := AHttpClient;
  FColumns := TList<string>.Create;
  FTypes := TList<string>.Create;
  FValues := TList<TRow>.Create;
end;

function TRqliteClient.CreateJSONArray(AStrings: TStrings): TJSONArray;
var
  i: integer;
  LStr: string;
  vi: integer;
  vf: double;
  vb: boolean;
begin
  i := 0;
  Result := TJSONArray.Create;
  for LStr in AStrings do
  begin
    if TryStrToInt(LStr, vi) then
      Result.Add(vi)
    else if TryStrToFloat(LStr, vf) then
      Result.Add(vf)
    else if TryStrToBool(LStr, vb) then
      Result.Add(vb)
    else
      Result.Add(LStr);
    Inc(i);
  end;
end;

destructor TRqliteClient.Destroy;
begin
  FValues.Free;
  FTypes.Free;
  FColumns.Free;
  inherited;
end;

function TRqliteClient.Execute(const QueryStr: TStringList;
  AsTransaction: boolean = false): boolean;
const
  cURI = 'http://%s:%d/db/execute?pretty&timings';
var
  s, LURI: string;
  LJSONObject: TJSONObject;
  LJSONArr, LJSONArrResults: TJSONArray;
  LJSONError: TJSONValue;
  SS: TStringStream;
begin
  Result := false;
  LJSONObject := nil;
  LURI := cURI;
  if AsTransaction then
    LURI := LURI + '&transaction';
  LJSONArr := CreateJSONArray(QueryStr);
  try
    SS := TStringStream.Create(LJSONArr.ToJSON);
    try
      LJSONObject := TJSONObject.ParseJSONValue
        (TEncoding.UTF8.GetBytes(FHttpClient.Post(Format(LURI,
        [FHostname, FPort]), SS)), 0) as TJSONObject;
      try
        Result := LJSONObject <> nil;
        s := LJSONObject.ToJSON;
        LJSONArrResults := LJSONObject.GetValue('results') as TJSONArray;
        if TryGetValueFromJSONArray<string>('error', LJSONArrResults, s) then
          raise Exception.Create(Format('Error: %s', [s]));
      finally
        LJSONObject.Free;
      end;
    finally
      SS.Free;
    end;
  finally
    LJSONArr.Free;
  end;
end;

function TRqliteClient.GetColumnNames: TList<string>;
begin
  Result := FColumns;
end;

function TRqliteClient.GetColumnTypes: TList<string>;
begin
  Result := FTypes;
end;

function TRqliteClient.GetDatabase: string;
begin
  Result := FDatabase;
end;

function TRqliteClient.GetHostname: string;
begin
  Result := FHostname;
end;

function TRqliteClient.GetPort: integer;
begin
  Result := FPort;
end;

function TRqliteClient.GetRow(idx: integer): TRow;
begin
  Result := FValues[idx];
end;

function TRqliteClient.GetRowCount: integer;
begin
  Result := FValues.Count;
end;

function TRqliteClient.TryGetValueFromJSONArray<T>(const AName: string;
  JSONArr: TJSONArray; out Val: T): boolean;
var
  LArrElement, FoundVal: TJSONValue;
begin
  Result := false;
  for LArrElement in JSONArr do
  begin
    FoundVal := LArrElement.FindValue(AName);
    if FoundVal <> nil then
    begin
      Val := LArrElement.GetValue<T>(AName);
      Result := True;
      break;
    end;
  end;
end;

function TRqliteClient.Query(const QueryStr: TStringList): boolean;
const
  cURI = 'http://%s:%d/db/query?pretty&timings&q=%s';
var
  i: integer;
  LArrOfStr: array of string;
  LURI, s: string;
  LJSONObject: TJSONObject;
  LJSONArr: TJSONArray;
  LCol, LType, LVal, LValElement: TJSONValue;
  LJSONColumnsArr, LJSONTypesArr, LJSONValuesArr, LValArray: TJSONArray;
begin
  Result := false;
  LJSONObject := nil;
  FColumns.Clear;
  FTypes.Clear;
  try
    s := '';
    LURI := Format(cURI, [FHostname, FPort, QueryStr.Text]);
    LJSONObject := TJSONObject.ParseJSONValue
      (TEncoding.UTF8.GetBytes(FHttpClient.Get(LURI)), 0) as TJSONObject;
    Result := LJSONObject <> nil;
    LJSONArr := LJSONObject.GetValue('results') as TJSONArray;
    if TryGetValueFromJSONArray<string>('Error', LJSONArr, s) then
      raise Exception.Create(Format('Error: %s', [s]));

    if TryGetValueFromJSONArray<TJSONArray>('columns', LJSONArr, LJSONColumnsArr)
    then
    begin
      for LCol in LJSONColumnsArr do
        FColumns.Add(LCol.GetValue<string>);
    end;

    if TryGetValueFromJSONArray<TJSONArray>('types', LJSONArr, LJSONTypesArr)
    then
    begin
      for LType in LJSONTypesArr do
        FTypes.Add(LType.GetValue<string>);
    end;

    if TryGetValueFromJSONArray<TJSONArray>('values', LJSONArr, LJSONValuesArr)
    then
    begin
      for LVal in LJSONValuesArr do
      begin
        i := 0;
        LValArray := LVal.AsType<TJSONArray>;
        SetLength(LArrOfStr, LValArray.Count);
        for LValElement in LValArray do
        begin
          LArrOfStr[i] := AnsiDequotedStr(LValElement.ToString, '"');
          Inc(i);
        end;
        FValues.Add(TRow.Create(TArray<string>(LArrOfStr)));
      end;
    end;
  finally
    LJSONObject.Free;
  end;
end;

procedure TRqliteClient.SetDatabase(const Value: string);
begin
  FDatabase := Value;
end;

procedure TRqliteClient.SetHostname(const Value: string);
begin
  FHostname := Value;
end;

procedure TRqliteClient.SetPort(const Value: integer);
begin
  FPort := Value;
end;

{ TRqliteClientFactory }

class function TRqliteClientFactory.CreateInstance(const AHttpClient
  : IHttpClient): IRqliteClient;
begin
  Result := TRqliteClient.Create(AHttpClient);
end;

{ TRow }

constructor TRow.Create(Values: TArray<string>);
begin
  FValues := Values;
end;

function TRow.GetColValue(idx: integer): string;
begin
  Result := FValues[idx];
end;

end.
