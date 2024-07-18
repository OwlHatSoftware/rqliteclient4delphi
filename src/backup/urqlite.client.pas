unit urqlite.client;

{$IFDEF FPC}
  {$MODE delphi}{$H+}
{$ENDIF}

// ****************************************************************************
// This unit contains the client class and interface of the rqliteClient4Delphi
// The TRqliteClientFactory class is able to create an instance of the class
// ****************************************************************************
// Created by: OwlHatSoftware
// License: GNU General Public License v3.0
// ****************************************************************************

interface

uses
  {$IFnDEF FPC}
  System.SysUtils, System.Classes, System.JSON,
  {$ELSE}
  SysUtils, Classes, fpjson,
  {$ENDIF}
  Generics.Collections,
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
    function GetColumnNames: TStringList;
    function GetColumnTypes: TStringList;
    function GetRowCount: integer;
    function GetRow(idx: integer): TRow;
    function Execute(const QueryStr: TStringList;
      AsTransaction: boolean = False): boolean; overload;
    function Execute(const QueryStr: string): boolean; overload;
    function Query(const QueryStr: TStringList): boolean;
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
  end;

  TRqliteClientFactory = class
    class function CreateInstance(const AHttpClient: IHttpClient): IRqliteClient;
  end;

// Conversion functions
function BytesToHexString(ByteData: TBytes): string;
function Base64ToBytes(ABase64String: string): TBytes;

implementation

uses
  {$IFnDEF FPC}
  {$ELSE}
  {$ENDIF}
  IdCoder, IdCoderMIME;

const
  cQueryURI = 'http://%s:%d/db/query?pretty&timings&q=%s';
  cExecuteURI = 'http://%s:%d/db/execute?pretty&timings';

type

  { TJSONConvert }

  TJSONConvert<T> = class
    class function TryGetValueFromJSONArray(const AName: string;
      JSONArr: TJSONArray; out Val: T): boolean; static;
  end;

  { TRqliteClient }

  TRqliteClient = class(TInterfacedObject, IRqliteClient)
  private
    FColumns: TStringList;
    FTypes: TStringList;
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
  public
    function GetColumnNames: TStringList;
    function GetColumnTypes: TStringList;
    function GetRowCount: integer;
    function GetRow(idx: integer): TRow;
    function Execute(const QueryStr: TStringList;
      AsTransaction: boolean = False): boolean; overload;
    function Execute(const QueryStr: string): boolean; overload;
    function Query(const QueryStr: TStringList): boolean;
    constructor Create(const AHttpClient: IHttpClient);
    destructor Destroy; override;
  published
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
  end;

function BytesToHexString(ByteData: TBytes): string;
(*
  referenced source : heidisql
  unit : dbconnection
  function : TDBQuery.HexValue(var ByteData: TBytes): String;
  Purpose: Convert Byte data to a Hexidecimal string representation to
  write raw Blob data.
*)
var
  BinLen: integer;
  Ansi: ansistring;
begin
  BinLen := Length(ByteData);
  SetString(Ansi, pansichar(ByteData), BinLen);

  if BinLen = 0 then
  begin
    Result := 'null';
  end
  else
  begin
    SetLength(Result, BinLen * 2);
    BinToHex(pansichar(Ansi), PChar(Result), BinLen);
    Result := 'X''' + Result + '''';
  end;
end;

function Base64ToBytes(ABase64String: string): TBytes;
begin
  Result := TBytes(TIdDecoderMIME.DecodeBytes(ABase64String));
end;

{ TJSONConvert }

class function TJSONConvert<T>.TryGetValueFromJSONArray(const AName: string;
  JSONArr: TJSONArray; out Val: T): boolean;
  {$IFnDEF FPC}
var
  LArrElement, FoundVal: TJSONValue;
begin
  Result := False;
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
{$ELSE}
var
  LArrElement: TJSONEnum;
  FoundVal: TJSONObject;
  LType: TTypeKind;
  JSONData: TJSONData;
begin
  Result := False;
  LType := GetTypeKind(T);
  for LArrElement in JSONArr do
  begin
    FoundVal := TJSONObject(LArrElement.Value);
    if FoundVal.Find(AName, JSONData) then
    begin
      Case LType of
        tkInteger, tkInt64:
          begin
            Integer(Val):= JSONData.AsInteger;
            result := True;
          end;
      //tkChar, tkWChar, tkUChar:
      //  begin
      //    Val := FoundVal.AsString;
      //  end;
      //tkEnumeration:
      //  begin
      //    Val := FoundVal.AsBoolean;
      //  end;
      //tkFloat:
      //  begin
      //    //Double(Val) := JSONData.AsFloat; //GIVES ILLEGAL TYPE CONVERSION!!
      //  end;
      //tkSet:
      //  begin
      //    //not supported
      //  end;
      //tkMethod:
      //  begin
      //    //not supported
      //  end;
        tkSString, tkLString, tkAString, tkWString, tkUString:
          begin
            String(Val) := JSONData.AsString;
            result := True;
          end;
      //tkVariant:
      //  begin
      //    Val := FoundVal.AsString;
      //  end;
      //tkBool:
      //  begin
      //    //Boolean(Val) := JSONData.AsBoolean; //GIVES ILLEGAL TYPE CONVERSION!!
      //  end;
      //tkQWord:
      //  begin
      //    //QWord(Val) := JSONData.AsQWord; GIVES ILLEGAL TYPE CONVERSION!!
      //  end;
      //tkUnknown,
      //tkArray,
      //tkRecord,
      //tkInterface,
      tkClass,tkObject:
        begin
          TJSONArray(Val) := TJSONArray(JSONData);
          result := True;
        end;
      //tkDynArray,
      //tkInterfaceRaw,
      //tkProcVar,
      //tkHelper,
      //tkFile,
      //tkClassRef,
      //tkPointer:
        else
          //not supported
          raise Exception.Create('Type not supported!');
        end;
      if result then
        break;
    end;
  end;
end;
{$ENDIF}

{ TRqliteClient }

constructor TRqliteClient.Create(const AHttpClient: IHttpClient);
begin
  FHttpClient := AHttpClient;
  FColumns := TStringList.Create;
  FTypes := TStringList.Create;
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

{$IFnDEF FPC}
function TRqliteClient.Execute(const QueryStr: TStringList;
  AsTransaction: boolean = False): boolean;
var
  s, LURI: string;
  LJSONObject: TJSONObject;
  LJSONArr, LJSONArrResults: TJSONArray;
  //LJSONError: TJSONValue;
  SS: TStringStream;
begin
  Result := False;
  LJSONObject := nil;
  LURI := cURI;
  if AsTransaction then
    LURI := LURI + '&transaction';
  LJSONArr := CreateJSONArray(QueryStr);
  try
    SS := TStringStream.Create(LJSONArr.ToJSON);
    try
      LJSONObject := TJSONObject.ParseJSONValue(
        TEncoding.UTF8.GetBytes(FHttpClient.Post(Format(LURI, [FHostname, FPort]), SS)),
        0) as TJSONObject;
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
{$ELSE}
function TRqliteClient.Execute(const QueryStr: TStringList;
  AsTransaction: boolean = False): boolean;
var
  s, LURI: string;
  SS: TStringStream;
  LJSONObject: TJSONObject;
  LJSONArr, LJSONArrResults, LJSONArrDef: TJSONArray;
begin
  Result := False;
  LJSONObject := nil;
  LURI := cExecuteURI;
  if AsTransaction then
    LURI := LURI + '&transaction';
  LJSONArr := CreateJSONArray(QueryStr);
  try
    SS := TStringStream.Create(LJSONArr.AsJSON);
    try
      LJSONObject := TJSONObject(GetJSON(FHttpClient.Post(Format(LURI, [FHostname, FPort]), SS)));
      try
        Result := LJSONObject <> nil;
        s := LJSONObject.AsJSON;
        LJSONArrDef:=TJSONArray.Create;
        LJSONArrResults := TJSONArray(LJSONObject.Get('results', LJSONArrDef));
        if TJSONConvert<string>.TryGetValueFromJSONArray('error', LJSONArrResults, s) then
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
{$ENDIF}

{$IFnDEF FPC}
{$ELSE}
function TRqliteClient.Execute(const QueryStr: string): boolean;
  var
    s, LURI: string;
    SS: TStringStream;
    LJSONObject: TJSONObject;
    LJSONArr, LJSONArrResults, LJSONArrDef: TJSONArray;
  begin
    Result := False;
    LJSONObject := nil;
    LURI := cExecuteURI;
    if AsTransaction then
      LURI := LURI + '&transaction';
    LJSONArr := TJSONArray.Create([QueryStr]);
    try
      SS := TStringStream.Create(LJSONArr.AsJSON);
      try
        LJSONObject := TJSONObject(GetJSON(FHttpClient.Post(Format(LURI, [FHostname, FPort]), SS)));
        try
          Result := LJSONObject <> nil;
          s := LJSONObject.AsJSON;
          LJSONArrDef:=TJSONArray.Create;
          LJSONArrResults := TJSONArray(LJSONObject.Get('results', LJSONArrDef));
          if TJSONConvert<string>.TryGetValueFromJSONArray('error', LJSONArrResults, s) then
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
{$ENDIF}

function TRqliteClient.GetColumnNames: TStringList;
begin
  Result := FColumns;
end;

function TRqliteClient.GetColumnTypes: TStringList;
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

{$IFnDEF FPC}
function TRqliteClient.Query(const QueryStr: TStringList): boolean;
var
  i: integer;
  LArrOfStr: array of string;
  LURI, s: string;
  LJSONObject: TJSONObject;
  LJSONArr: TJSONArray;
  LCol, LType, LVal, LValElement: TJSONValue;
  LJSONColumnsArr, LJSONTypesArr, LJSONValuesArr, LValArray: TJSONArray;
begin
  Result := False;
  LJSONObject := nil;
  FColumns.Clear;
  FTypes.Clear;
  try
    s := '';
    LURI := Format(cQueryURI, [FHostname, FPort, QueryStr.Text]);
    LJSONObject := TJSONObject.ParseJSONValue(
      TEncoding.UTF8.GetBytes(FHttpClient.Get(LURI)), 0) as TJSONObject;
    Result := LJSONObject <> nil;
    LJSONArr := LJSONObject.GetValue('results') as TJSONArray;
    if TryGetValueFromJSONArray<string>('Error', LJSONArr, s) then
      raise Exception.Create(Format('Error: %s', [s]));

    if TryGetValueFromJSONArray<TJSONArray>('columns', LJSONArr, LJSONColumnsArr) then
    begin
      for LCol in LJSONColumnsArr do
        FColumns.Add(LCol.GetValue<string>);
    end;

    if TryGetValueFromJSONArray<TJSONArray>('types', LJSONArr, LJSONTypesArr) then
    begin
      for LType in LJSONTypesArr do
        FTypes.Add(LType.GetValue<string>);
    end;

    if TryGetValueFromJSONArray<TJSONArray>('values', LJSONArr, LJSONValuesArr) then
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
{$ELSE}
function TRqliteClient.Query(const QueryStr: TStringList): boolean;
var
  i: integer;
  LURI, s: string;
  LArrOfStr: array of string;
  LJSONObject: TJSONObject;
  LJSONArr: TJSONArray;
  LCol, LType, LVal, LValElement: TJSONEnum;
  LJSONColumnsArr, LJSONTypesArr, LJSONValuesArr, LValArray: TJSONArray;
begin
  Result := False;
  LJSONObject := nil;
  FColumns.Clear;
  FTypes.Clear;
  try
   s := '';
    LURI := Format(cQueryURI, [FHostname, FPort, QueryStr.Text]);
    LJSONObject := TJSONObject(GetJSON(FHttpClient.Get(LURI)));
    Result := LJSONObject <> nil;
    LJSONArr := LJSONObject.Get('results', TJSONArray.Create) as TJSONArray;
    if TJSONConvert<string>.TryGetValueFromJSONArray('Error', LJSONArr, s) then
      raise Exception.Create(Format('Error: %s', [s]));

    if TJSONConvert<TJSONArray>.TryGetValueFromJSONArray('columns', LJSONArr, LJSONColumnsArr) then
    begin
      for LCol in LJSONColumnsArr do
        FColumns.Add(LCol.Value.AsString);
    end;

    if TJSONConvert<TJSONArray>.TryGetValueFromJSONArray('types', LJSONArr, LJSONTypesArr) then
    begin
      for LType in LJSONTypesArr do
        FTypes.Add(LType.Value.AsString);
    end;

    if TJSONConvert<TJSONArray>.TryGetValueFromJSONArray('values', LJSONArr, LJSONValuesArr) then
    begin
      for LVal in LJSONValuesArr do
      begin
        i := 0;
        LValArray := TJSONArray(LVal.Value);
        SetLength(LArrOfStr, LValArray.Count);
        for LValElement in LValArray do
        begin
          LArrOfStr[i] := AnsiDequotedStr(LValElement.Value.AsString, '"');
          Inc(i);
        end;
        FValues.Add(TRow.Create(TArray<string>(LArrOfStr)));
      end;
    end;
  finally
    LJSONObject.Free;
  end;
end;

{$ENDIF}

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

class function TRqliteClientFactory.CreateInstance(
  const AHttpClient: IHttpClient): IRqliteClient;
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
