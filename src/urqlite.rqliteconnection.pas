unit urqlite.rqliteconnection;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}
  {$ELSE}
  Classes, SysUtils, DB,
  {$ENDIF}
  urqlite.client, urqlite.net;

type
  { TRQLiteConnection }

  TRQLiteConnection = class(TComponent)
  private
    FDatabase: string;
    FHostName: string;
    FPort: integer;
    FRQliteClient: IRqliteClient;
    procedure SetDatabase(AValue: string);
    procedure SetHostName(AValue: string);
    procedure SetPort(AValue: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
  published
    property Hostname: string read FHostName write SetHostName;
    property Port: integer read FPort write SetPort;
    property Database: string read FDatabase write SetDatabase;
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

procedure TRQLiteConnection.SetPort(AValue: integer);
begin
  if FPort = AValue then Exit;
  FPort := AValue;
end;

procedure TRQLiteConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
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
  FRQliteClient := TRqliteClientFactory.CreateInstance(
    THttpClientFactory.CreateIndyInstance);
  FRQliteClient.Hostname := 'localhost';
  FRQliteClient.Port := 4005;
  FRQliteClient.Database := 'Test';
end;

destructor TRQLiteConnection.Destroy;
begin
  FRQliteClient := nil;
  inherited Destroy;
end;

procedure TRQLiteConnection.Open;
begin
  //ping the RQLite service
end;

procedure TRQLiteConnection.Close;
begin

end;

procedure Register;
begin
  RegisterComponents('RQLite', [TRQLiteConnection]);
end;

end.
