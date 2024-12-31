unit urqlite.monitor;

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
  SysUtils, Classes, TypInfo, fpjson, jsonparser,
  {$ENDIF}
  Generics.Collections,
  urqlite.net;

type
  { TNodeStatus }

  TNodeStatus = record
    id: integer;
    api_addr: string;
    addr: string;
    voter: boolean;
    reachable: boolean;
    leader: boolean;
    time_s: string;
    error: string;
    procedure Init;
  end;

  TStatusChanged = procedure(Sender: TObject; NodeStatus: TNodeStatus) of object;
  IRqliteMonitor = interface
    ['{44E163DD-F2B3-4E9E-992A-1286544D4AAE}']
    procedure SetPort(AValue: integer);
    function GetPort: integer;
    procedure SetHostname(AValue: string);
    function GetHostname: string;
    procedure SetOnStatusChanged(AValue: TStatusChanged);
    function GetOnStatusChanged: TStatusChanged;
    //Launches a thread to check the status of the nodes
    //if there is any change on a node an event is fired returning the node status
    //When starting the thread the status of each node is reported once (initial status).
    procedure StartNodeCheck;
    procedure StopNodeCheck;
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property OnStatusChanged: TStatusChanged read GetOnStatusChanged
      write SetOnStatusChanged;
  end;



implementation

const
  cNodesURI = 'http://%s:%d/nodes?timeout=%s';
  cStatusURI = 'http://%s:%d/readyz?noleader';

type
  { TCheckNodesThread }
  TCheckNodesThread = class(TThread)
  private
    FHttpClient: IHttpClient;
    FNodeStatusList: TList<TNodeStatus>;
    FOldNodeStatusList: TList<TNodeStatus>;
    FNodeStatusURI: string;
    FStatusChanged: TStatusChanged;
    FInit: boolean;
    FChangedNode: TNodeStatus;
    procedure CheckNodes;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(const AHttpClient: IHttpClient; CreateSuspended: boolean);
    destructor Destroy; override;
    property NodeStatusURI: string read FNodeStatusURI write FNodeStatusURI;
    property OnStatusChanged: TStatusChanged read FStatusChanged write FStatusChanged;
  end;


  { TRQLiteMonitor }

  TRQLiteMonitor = class(TInterfacedObject, IRqliteMonitor)
  private
    FStatusChanged: TStatusChanged;
    FCheckNodesThread: TCheckNodesThread;
    FPort: integer;
    FHostname: string;
    FHttpClient: IHttpClient;
    function GetHostname: string;
    function GetOnStatusChanged: TStatusChanged;
    function GetPort: integer;
    procedure SetHostname(AValue: string);
    procedure SetOnStatusChanged(AValue: TStatusChanged);
    procedure DoStatusChanged(Sender: TObject; NodeStatus: TNodeStatus);
    procedure SetPort(AValue: integer);
  public
    constructor Create(const AHttpClient: IHttpClient);
    destructor Destroy; override;
    procedure StartNodeCheck;
    procedure StopNodeCheck;
    property Hostname: string read GetHostname write SetHostname;
    property Port: integer read GetPort write SetPort;
    property OnStatusChanged: TStatusChanged read GetOnStatusChanged
      write SetOnStatusChanged;
  end;

{ TNodeStatus }

procedure TNodeStatus.Init;
begin
  id := -1;
  api_addr := '';
  addr := '';
  voter := False;
  reachable := False;
  leader := False;
  time_s := '0s';
  error := '';
end;

  { TCheckNodesThread }

procedure TCheckNodesThread.CheckNodes;
var
  i, j: integer;
  LJSONData, LJSONDataItem: TJSONData;
  LNodeStatus: TNodeStatus;
  field_name, field_value: string;
begin
  if FNodeStatusURI = '' then
    Exit;
  LJSONData := GetJSON(FHttpClient.Get(FNodeStatusURI));
  for i := 0 to LJSONData.Count - 1 do
  begin
    LJSONDataItem := LJSONData.Items[i];
    for j := 0 to LJSONDataItem.Count - 1 do
    begin
      field_name := TJSONObject(LJSONDataItem).Names[j];
      field_value := LJSONDataItem.FindPath(
        TJSONObject(LJSONDataItem).Names[j]).AsString;
      if field_name = 'id' then
        LNodeStatus.id := StrToInt(field_value);
      if field_name = 'api_addr' then
        LNodeStatus.addr := field_value;
      if field_name = 'addr' then
        LNodeStatus.addr := field_value;
      if field_name = 'voter' then
        LNodeStatus.voter := StrToBool(field_value);
      if field_name = 'reachable' then
        LNodeStatus.reachable := StrToBool(field_value);
      if field_name = 'leader' then
        LNodeStatus.leader := StrToBool(field_value);
      if field_name = 'time_s' then
        LNodeStatus.time_s := field_value;
      if field_name = 'error' then
        LNodeStatus.error := field_value
      else
        LNodeStatus.error := '';
      FNodeStatusList.Add(LNodeStatus);
    end;
  end;
end;

procedure TCheckNodesThread.ShowStatus;
var
  i: integer;
begin
  if FInit then
  begin
    for i := 0 to FNodeStatusList.Count - 1 do
    begin
      if Assigned(OnStatusChanged) then
        OnStatusChanged(self, FNodeStatusList[i]);
    end;
    FInit := False;
  end
  else
  begin
    if Assigned(OnStatusChanged) then
      OnStatusChanged(self, FChangedNode);
  end;
end;

procedure TCheckNodesThread.Execute;
var
  i: integer;
  NewNodeStatus, OldNodeStatus: TNodeStatus;
  c: integer;
begin
  CheckNodes;
  FOldNodeStatusList.Clear;
  FOldNodeStatusList.AddRange(FNodeStatusList);
  Synchronize(Showstatus);
  while (not Terminated) do
  begin
    CheckNodes;
    if FNodeStatusList.Count >= FOldNodeStatusList.Count then
      c := FNodeStatusList.Count
    else
      c := FOldNodeStatusList.Count;
    for i := 0 to c - 1 do
    begin
      if not (i > FNodeStatusList.Count) then
        NewNodeStatus := FNodeStatusList[i]
      else
        NewNodeStatus.Init;
      if not (i > FOldNodeStatusList.Count) then
        OldNodeStatus := FOldNodeStatusList[i]
      else
        OldNodeStatus.Init;
      if not CompareMem(@NewNodeStatus, @OldNodeStatus, SizeOf(TNodeStatus)) then
      begin
        FChangedNode := FNodeStatusList[i];
        Synchronize(Showstatus);
      end;
    end;
  end;
end;

constructor TCheckNodesThread.Create(const AHttpClient: IHttpClient;
  CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FHttpClient := AHttpClient;
  FNodeStatusURI := '';
  FNodeStatusList := TList<TNodeStatus>.Create;
  FOldNodeStatusList := TList<TNodeStatus>.Create;
  FInit := True;
end;

destructor TCheckNodesThread.Destroy;
begin
  FOldNodeStatusList.Free;
  FNodeStatusList.Free;
  inherited Destroy;
end;

{ TRQLiteMonitor }

function TRQLiteMonitor.GetOnStatusChanged: TStatusChanged;
begin
  Result := FStatusChanged;
end;

function TRQLiteMonitor.GetHostname: string;
begin
  result := FHostName;
end;

function TRQLiteMonitor.GetPort: integer;
begin
  result := FPort;
end;

procedure TRQLiteMonitor.SetHostname(AValue: string);
begin
  FHostName := AValue;
end;

procedure TRQLiteMonitor.SetOnStatusChanged(AValue: TStatusChanged);
begin
  FStatusChanged := AValue;
end;

procedure TRQLiteMonitor.DoStatusChanged(Sender: TObject; NodeStatus: TNodeStatus);
begin
  if Assigned(OnStatusChanged) then
    OnStatusChanged(self, NodeStatus);
end;

procedure TRQLiteMonitor.SetPort(AValue: integer);
begin
  FPort := AValue;
end;

constructor TRQLiteMonitor.Create(const AHttpClient: IHttpClient);
begin
  FHttpClient := AHttpClient;
  FCheckNodesThread := TCheckNodesThread.Create(FHttpClient, True);
  FCheckNodesThread.OnStatusChanged := DoStatusChanged;
end;

destructor TRQLiteMonitor.Destroy;
begin
  StopNodeCheck;
  inherited Destroy;
end;

procedure TRQLiteMonitor.StartNodeCheck;
begin
  if (HostName = '') or (Port = 0) then
    raise Exception.Create('Hostname or port not set!');
  FCheckNodesThread.NodeStatusURI := Format(cNodesURI, [Hostname, Port, '10s']);
  FCheckNodesThread.Start;
end;

procedure TRQLiteMonitor.StopNodeCheck;
begin
  FCheckNodesThread.Terminate;
end;

end.
