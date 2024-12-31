unit urqlite.rqliteconnection;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}
  {$ELSE}
  Classes, SysUtils, DB, Dialogs,
  {$ENDIF}
  urqlite.client, urqlite.net;

type
  { TRQLiteConnection }

  TRQLiteConnection = class(TComponent)
  private
    FConnected: boolean;
    FDatabase: string;
    FHostName: string;
    FPathToRQLiteCLI: string;
    FPathToRQLited: string;
    FPort: integer;
    FRQliteClient: IRqliteClient;
    procedure SetDatabase(AValue: string);
    procedure SetHostName(AValue: string);
    procedure InternalSetPathToRQLiteCLI;
    procedure InternalSetPathToRQLited;
    procedure SetPathToRQLiteCLI(AValue: string);
    procedure SetPathToRQLited(AValue: string);
    procedure SetPort(AValue: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function Connected: boolean;
    procedure RemoveNode(ANodeId: integer);
  published
    property Hostname: string read FHostName write SetHostName;
    property Port: integer read FPort write SetPort;
    property Database: string read FDatabase write SetDatabase;
    property PathToRQLiteCLI: string read FPathToRQLiteCLI write SetPathToRQLiteCLI;
    property PathToRQLited: string read FPathToRQLited write SetPathToRQLited;
  end;

procedure Register;

implementation


{ TRQLiteConnection }

procedure TRQLiteConnection.SetDatabase(AValue: string);
begin
  if FDatabase = AValue then Exit;
  FDatabase := AValue;
end;

procedure TRQLiteConnection.SetHostName(AValue: string);
begin
  if FHostName = AValue then Exit;
  FHostName := AValue;
end;

procedure TRQLiteConnection.InternalSetPathToRQLiteCLI;
begin
  if not Assigned(FRQliteClient) then
    raise Exception.Create(' Unable to set path to rqlite CLI!');
  FRQliteClient.PathToRQLiteCLI := FPathToRQLiteCLI;
end;

procedure TRQLiteConnection.InternalSetPathToRQLited;
begin
  if not Assigned(FRQliteClient) then
    raise Exception.Create(' Unable to set path to rqlited!');
  FRQliteClient.PathToRQLiteCLI := FPathToRQLiteCLI;
end;

procedure TRQLiteConnection.SetPathToRQLiteCLI(AValue: string);
begin
   if FPathToRQLiteCLI=AValue then Exit;
  FPathToRQLiteCLI := AValue;
end;

procedure TRQLiteConnection.SetPathToRQLited(AValue: string);
begin
  if FPathToRQLited=AValue then Exit;
  FPathToRQLited:=AValue;
end;

procedure TRQLiteConnection.SetPort(AValue: integer);
begin
  if FPort = AValue then Exit;
  FPort := AValue;
end;

procedure TRQLiteConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    //remove dependancies to other components if there are any...
  end;
end;

constructor TRQLiteConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnected := False;
end;

destructor TRQLiteConnection.Destroy;
begin
  FRQliteClient := nil;
  inherited Destroy;
end;

procedure TRQLiteConnection.Open;
begin
  if not Assigned(FRQliteClient) then
    FRQliteClient := TRqliteClientFactory.CreateInstance(
      THttpClientFactory.CreateIndyInstance);
  FRQliteClient.Hostname := Hostname;
  FRQliteClient.Port := Port;
  FRQliteClient.Database := Database;
  //ping the RQLite service
  if not FRQliteClient.GetReadyStatus then
    raise Exception.Create('Unable to open connection!');
  FConnected := True;
  InternalSetPathToRQLiteCLI;
  InternalSetPathToRQLited;
end;

procedure TRQLiteConnection.Close;
begin
  FConnected := False;
end;

function TRQLiteConnection.Connected: boolean;
begin
  //ping the RQLite service
  FConnected := FRQliteClient.GetReadyStatus;
  if not FConnected then
    raise Exception.Create('Connection is lost!');
  Result := FConnected;
end;

procedure TRQLiteConnection.RemoveNode(ANodeId: integer);
begin
  FRQliteClient.RemoveNode(ANodeId);
end;

procedure Register;
begin
  RegisterComponents('RQLite', [TRQLiteConnection]);
end;

end.
