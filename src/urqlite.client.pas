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
  SysUtils, Classes, Variants, TypInfo, fpjson, jsonparser,
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

  { IRqliteClient }

  IRqliteClient = interface
    ['{002E26F4-55AB-4FFB-8324-99413E5F91AA}']
    procedure ClearAll;
    function GetPathToRQLiteCLI: string;
    function GetPathToRQLited: string;
    procedure SetDatabase(const Value: string);
    procedure SetHostname(const Value: string);
    procedure SetPathToRQLiteCLI(AValue: string);
    procedure SetPathToRQLited(AValue: string);
    procedure SetPort(const Value: integer);
    function GetDatabase: string;
    function GetHostname: string;
    function GetPort: integer;
    function GetColumnNames: TStringList;
    function GetColumnTypes: TStringList;
    function GetRowCount: integer;
    function GetRow(idx: integer): TRow;
    function Execute(const QueryStr: TStringList; out RawJSONResult: TJSONObject;
      AsTransaction: boolean = False): boolean; overload;
    function Execute(const QueryStr: TStringList;
      AsTransaction: boolean = False): boolean; overload;
    function Execute(const QueryStr: string; out RawJSONResult: TJSONObject): boolean;
      overload;
    function Execute(const QueryStr: string): boolean; overload;
    function Query(const QueryStr: TStringList;
      out RawJSONResult: TJSONObject): boolean; overload;
    function Query(const QueryStr: TStringList): boolean; overload;
    //rqlite nodes serve a “ready” status at /readyz.
    //The endpoint will return HTTP 200 OK if the node is ready to respond
    //to database requests and cluster management operations.
    function GetReadyStatus: boolean;
    function ExecuteCommand(const ACommand: string): boolean;
    procedure RemoveNode(ANodeId: integer);
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
    property PathToRQLiteCLI: string read GetPathToRQLiteCLI write SetPathToRQLiteCLI;
    property PathToRQLited: string read GetPathToRQLited write SetPathToRQLited;
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
  Process,
  {$ENDIF}
  IdCoder, IdCoderMIME;

const
  cStatusURI = 'http://%s:%d/readyz?noleader';
  cRemoveNode = 'http://%s:%d/remove';
  cQueryURI = 'http://%s:%d/db/query?pretty&timings&q=%s';
  cExecuteURI = 'http://%s:%d/db/execute?pretty&timings';

type

  { TJSONConvert }

  TJSONConvert = class
    class function TryGetValueFromJSONArray(const AName: string;
      JSONArr: TJSONArray; out Val: variant): boolean; static;
  end;



  { TRqliteClient }

  TRqliteClient = class(TInterfacedObject, IRqliteClient)
  private
    FPathToRQLiteCLI: string;
    FPathToRQLited: string;
    FColumns: TStringList;
    FTypes: TStringList;
    FValues: TList<TRow>;
    FHttpClient: IHttpClient;
    FPort: integer;
    FDatabase: string;
    FHostname: string;
    function GetPathToRQLiteCLI: string;
    function GetPathToRQLited: string;
    procedure SetDatabase(const Value: string);
    procedure SetHostname(const Value: string);
    procedure SetPathToRQLiteCLI(AValue: string);
    procedure SetPathToRQLited(AValue: string);
    procedure SetPort(const Value: integer);
    function GetDatabase: string;
    function GetHostname: string;
    function GetPort: integer;
    function CreateJSONArray(AStrings: TStrings): TJSONArray;
  public
    procedure ClearAll;
    function GetColumnNames: TStringList;
    function GetColumnTypes: TStringList;
    function GetRowCount: integer;
    function GetRow(idx: integer): TRow;
    function GetReadyStatus: boolean;
    function Execute(const QueryStr: TStringList; out RawJSONResult: TJSONObject;
      AsTransaction: boolean = False): boolean; overload;
    function Execute(const QueryStr: TStringList;
      AsTransaction: boolean = False): boolean; overload;
    function Execute(const QueryStr: string; out RawJSONResult: TJSONObject): boolean;
      overload;
    function Execute(const QueryStr: string): boolean; overload;
    function Query(const QueryStr: TStringList;
      out RawJSONResult: TJSONObject): boolean; overload;
    function Query(const QueryStr: TStringList): boolean; overload;
    function ExecuteCommand(const ACommand: string): boolean;
    procedure RemoveNode(ANodeId: integer);
    constructor Create(const AHttpClient: IHttpClient);
    destructor Destroy; override;
  published
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
    property PathToRQLiteCLI: string read GetPathToRQLiteCLI write SetPathToRQLiteCLI;
    property PathToRQLited: string read GetPathToRQLited write SetPathToRQLited;
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

class function TJSONConvert.TryGetValueFromJSONArray(const AName: string;
  JSONArr: TJSONArray; out Val: variant): boolean;
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
      Val := LArrElement.GetValue<variant>(AName);
      Result := True;
      break;
    end;
  end;
end;
{$ELSE}
var
  LArrElement: TJSONEnum;
  FoundVal: TJSONObject;
  JSONData: TJSONData;
  object_type: String;
begin
  Result := False;
  //LType := GetTypeKind(T);
  for LArrElement in JSONArr do
  begin
    FoundVal := TJSONObject(LArrElement.Value);
    if FoundVal.Find(AName, JSONData) then
    begin
      object_type := GetEnumName(TypeInfo(TJSONtype), Ord(JSONData.JSONType));
      case JSONData.JSONType of
        jtNumber: Val := JSONData.AsInteger;
        jtString: Val := JSONData.AsString;
        jtBoolean: Val := JSONData.AsBoolean;
        jtNull: Val := null;
        jtArray: Val := TJSONArray(JSONData).AsJSON;
        jtObject: Val := TJSONObject(JSONData).AsJSON;
      else
        //not supported
        raise Exception.Create(Format('Type %s not supported!',[object_type]));
      end;
      result := not VarIsEmpty(Val);
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

procedure TRqliteClient.ClearAll;
begin
  FValues.Clear;
  FTypes.Clear;
  FColumns.Clear;
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
function TRqliteClient.Execute(const QueryStr: TStringList; out RawJSONResult: TJSONObject;
  AsTransaction: boolean = False): boolean;
var
  s, LURI: string;
  v: variant;
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
        RawJSONResult := LJSONObject;
        s := LJSONObject.AsJSON;
        LJSONArrDef:=TJSONArray.Create;
        LJSONArrResults := TJSONArray(LJSONObject.Get('results', LJSONArrDef));
        if TJSONConvert.TryGetValueFromJSONArray('error', LJSONArrResults, v) then
          raise Exception.Create(Format('Error: %s', [v]));
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

function TRqliteClient.Execute(const QueryStr: TStringList;
  AsTransaction: boolean): boolean;
var
  LRawJSON: TJSONObject;
begin
  result := Execute(QueryStr, LRawJSON, AsTransaction);
end;

{$ENDIF}

{$IFnDEF FPC}
//TODO: Delphi implementation
{$ELSE}
function TRqliteClient.Execute(const QueryStr: string; out
  RawJSONResult: TJSONObject): boolean;
  var
    s, LURI: string;
    v: variant;
    SS: TStringStream;
    LJSONObject: TJSONObject;
    LJSONArr, LJSONArrResults, LJSONArrDef: TJSONArray;
  begin
    Result := False;
    LJSONObject := nil;
    LURI := cExecuteURI;
    LJSONArr := TJSONArray.Create([QueryStr]);
    try
      SS := TStringStream.Create(LJSONArr.AsJSON);
      try
        LJSONObject := TJSONObject(GetJSON(FHttpClient.Post(Format(LURI, [FHostname, FPort]), SS)));
        try
          Result := LJSONObject <> nil;
          RawJSONResult := LJSONObject;
          s := LJSONObject.AsJSON;
          LJSONArrDef:=TJSONArray.Create;
          LJSONArrResults := TJSONArray(LJSONObject.Get('results', LJSONArrDef));
          if TJSONConvert.TryGetValueFromJSONArray('error', LJSONArrResults, v) then
            raise Exception.Create(Format('Error: %s', [v]));
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

function TRqliteClient.Execute(const QueryStr: string): boolean;
var
  LRawJSON: TJSONObject;
begin
  result := Execute(QueryStr, LRawJSON);
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

function TRqliteClient.GetReadyStatus: boolean;
var
  LResponse: string;
begin
  LResponse := FHttpClient.Get(Format(cStatusURI, [FHostname, FPort]));
  Result := LResponse = '[+]node ok';
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
function TRqliteClient.Query(const QueryStr: TStringList; out RawJSONResult: TJSONObject): boolean;
var
  i: integer;
  LURI, s: string;
  v: variant;
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
    RawJSONResult := LJSONObject;
    LJSONArr := LJSONObject.Get('results', TJSONArray.Create) as TJSONArray;
    if TJSONConvert.TryGetValueFromJSONArray('Error', LJSONArr, v) then
      raise Exception.Create(Format('Error: %s', [v]));

    if TJSONConvert.TryGetValueFromJSONArray('columns', LJSONArr, v) then
    begin
      LJSONColumnsArr := TJSONArray(GetJSON(v));
      for LCol in LJSONColumnsArr do
        FColumns.Add(LCol.Value.AsString);
    end;

    if TJSONConvert.TryGetValueFromJSONArray('types', LJSONArr, v) then
    begin
      LJSONTypesArr := TJSONArray(GetJSON(v));
      for LType in LJSONTypesArr do
        FTypes.Add(LType.Value.AsString);
    end;

    if TJSONConvert.TryGetValueFromJSONArray('values', LJSONArr, v) then
    begin
      LJSONValuesArr := TJSONArray(GetJSON(v));
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

function TRqliteClient.Query(const QueryStr: TStringList): boolean;
var
  LRawJSON: TJSONObject;
begin
  result := Query(QueryStr, LRawJSON);
end;

{$ENDIF}

function TRqliteClient.ExecuteCommand(const ACommand: string): boolean;
var
  FirstProcess, SecondProcess: TProcess;
  Buffer: array[0..127] of char;
  ReadCount: integer;
  ReadSize: integer;
begin
  Result := False;
  FirstProcess := TProcess.Create(nil);
  //SecondProcess := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
      FirstProcess.Executable := Format('%s\rqlite.exe',[FPathToRQLiteCLI]);
      //SecondProcess.Executable := ACommand;
      //SecondProcess.Parameters.Add(ACommand);
    {$ENDIF}
    {$IFDEF UNIX}
      FirstProcess.Executable := 'bash';
    {$ENDIF}
    FirstProcess.Options := [poUsePipes];
    //SecondProcess.Options := [poUsePipes,poStderrToOutPut];
    FirstProcess.Execute;
    //SecondProcess.Execute;
    while FirstProcess.Running or (FirstProcess.Output.NumBytesAvailable > 0) do
    begin
      if FirstProcess.Output.NumBytesAvailable > 0 then
      begin
        // make sure that we don't read more data than we have allocated
        // in the buffer
        //ReadSize := FirstProcess.Output.NumBytesAvailable;
        //if ReadSize > SizeOf(Buffer) then
        //   ReadSize := SizeOf(Buffer);
        // now read the output into the buffer
        //ReadCount := FirstProcess.Output.Read(Buffer[0], ReadSize);
        // and write the buffer to the second process
        FirstProcess.Input.WriteAnsiString(ACommand + #10#13);
        Break;
      end;
    end;
    // Close the input on the SecondProcess
    // so it finishes processing it's data
    //SecondProcess.CloseInput;
    Result := True;
  finally
    //SecondProcess.Free;
    FirstProcess.Free;
  end;
end;


procedure TRqliteClient.RemoveNode(ANodeId: integer);
var
  s: ansistring;
  SS: TStringStream;
begin
  SS := TStringStream.Create(Format('{"id": "%d"}', [ANodeId]));
  try
    SS.Position := 0;
    FHttpClient.Delete(Format(cRemoveNode, [Hostname, Port]), SS);
  finally
    SS.Free;
  end;
end;

function TRqliteClient.GetPathToRQLiteCLI: string;
begin
  Result := FPathToRQLiteCLI;
end;

function TRqliteClient.GetPathToRQLited: string;
begin
  Result := FPathToRQLited;
end;

procedure TRqliteClient.SetDatabase(const Value: string);
begin
  FDatabase := Value;
end;

procedure TRqliteClient.SetHostname(const Value: string);
begin
  FHostname := Value;
end;

procedure TRqliteClient.SetPathToRQLiteCLI(AValue: string);
begin
  FPathToRQLiteCLI := AValue;
end;

procedure TRqliteClient.SetPathToRQLited(AValue: string);
begin
  if FPathToRQLited = AValue then Exit;
  FPathToRQLited := AValue;
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
