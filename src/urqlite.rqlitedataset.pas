unit urqlite.rqlitedataset;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}
  {$ELSE}
  Classes, SysUtils, DB,
  {$ENDIF}
  gxbasedataset,
  urqlite.rqliteconnection;

type

  { TRQLiteDataSet }

  TRQLiteDataSet = class(TGXBaseDataset)
  private
    FCurRec: longint;
    FReadOnly: boolean;
    FRQLiteConnection: TRQLiteConnection;
    procedure SetReadOnly(AValue: boolean);
    procedure SetRQLiteConnection(AValue: TRQLiteConnection);
  protected
    procedure DoBeforeOpen; override;
    function DoOpen: boolean; override;
    procedure DoClose; override;
    procedure DoDeleteRecord; override;
    procedure DoCreateFieldDefs; override;
    function GetFieldValue(Field: TField): variant; override;
    procedure SetFieldValue(Field: TField; Value: variant); override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
    procedure DoFirst; override;
    procedure DoLast; override;
    procedure DoAfterScroll; override;
    function Navigate(GetMode: TGetMode): TGetResult; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //Record ID functions
    function AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;
    procedure GotoRecordID(Value: Pointer); override;
    //Bookmark functions
    function GetBookMarkSize: integer; override;
    procedure AllocateBookMark(RecordID: Pointer; ABookmark: Pointer); override;
    procedure DoGotoBookmark(ABookmark: Pointer); override;
    //Others
    procedure DoBeforeGetFieldValue; override;
    procedure DoAfterGetFieldValue; override;
    procedure DoBeforeSetFieldValue(Inserting: boolean); override;
    procedure DoAfterSetFieldValue(Inserting: boolean); override;
    {Overriden datatset methods}
    function GetCanModify: boolean; override;
    function GetRecordCount: integer; override;
    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;
  public
    procedure Open;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property IsReadOnly: boolean read FReadOnly write SetReadOnly;
    property Connection: TRQLiteConnection read FRQLiteConnection write SetRQLiteConnection;
  end;

implementation

{ TRQLiteDataSet }

procedure TRQLiteDataSet.SetReadOnly(AValue: boolean);
begin
  if (AValue <> FReadOnly) then
  begin
    if Active then DatabaseError(
        'Cannot change readonly property when dataset is active');
    FReadOnly := AValue;
  end;
end;

procedure TRQLiteDataSet.SetRQLiteConnection(AValue: TRQLiteConnection);
begin
  if FRQLiteConnection=AValue then Exit;
  FRQLiteConnection:=AValue;
  if FRQLiteConnection <> nil then
  begin
    FRQLiteConnection.FreeNotification(Self);
  end;
end;

procedure TRQLiteDataSet.DoBeforeOpen;
begin
  inherited DoBeforeOpen;
end;

function TRQLiteDataSet.DoOpen: boolean;
begin
  FCurRec := -1;
  Result := False;
  if Assigned(FRQLiteConnection) then
    Result := FRQLiteConnection.Connected;
end;

procedure TRQLiteDataSet.DoClose;
begin
  if Assigned(FRQLiteConnection) and (FRQLiteConnection.Connected) then
    FRQLiteConnection.Close;
  fCurRec := -1;
end;

procedure TRQLiteDataSet.DoDeleteRecord;
begin
  inherited DoDeleteRecord;
end;

procedure TRQLiteDataSet.DoCreateFieldDefs;
begin

end;

function TRQLiteDataSet.GetFieldValue(Field: TField): variant;
begin

end;

procedure TRQLiteDataSet.SetFieldValue(Field: TField; Value: variant);
begin

end;

procedure TRQLiteDataSet.GetBlobField(Field: TField; Stream: TStream);
begin

end;

procedure TRQLiteDataSet.SetBlobField(Field: TField; Stream: TStream);
begin

end;

procedure TRQLiteDataSet.DoFirst;
begin

end;

procedure TRQLiteDataSet.DoLast;
begin

end;

procedure TRQLiteDataSet.DoAfterScroll;
begin
  inherited DoAfterScroll;
end;

function TRQLiteDataSet.Navigate(GetMode: TGetMode): TGetResult;
begin

end;

procedure TRQLiteDataSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

function TRQLiteDataSet.AllocateRecordID: Pointer;
begin

end;

procedure TRQLiteDataSet.DisposeRecordID(Value: Pointer);
begin

end;

procedure TRQLiteDataSet.GotoRecordID(Value: Pointer);
begin

end;

function TRQLiteDataSet.GetBookMarkSize: integer;
begin

end;

procedure TRQLiteDataSet.AllocateBookMark(RecordID: Pointer; ABookmark: Pointer
  );
begin

end;

procedure TRQLiteDataSet.DoGotoBookmark(ABookmark: Pointer);
begin

end;

procedure TRQLiteDataSet.DoBeforeGetFieldValue;
begin

end;

procedure TRQLiteDataSet.DoAfterGetFieldValue;
begin

end;

procedure TRQLiteDataSet.DoBeforeSetFieldValue(Inserting: boolean);
begin

end;

procedure TRQLiteDataSet.DoAfterSetFieldValue(Inserting: boolean);
begin

end;

function TRQLiteDataSet.GetCanModify: boolean;
begin
  Result:=inherited GetCanModify;
end;

function TRQLiteDataSet.GetRecordCount: integer;
begin
  Result:=inherited GetRecordCount;
end;

function TRQLiteDataSet.GetRecNo: integer;
begin
  Result:=inherited GetRecNo;
end;

procedure TRQLiteDataSet.SetRecNo(Value: integer);
begin
  inherited SetRecNo(Value);
end;

procedure TRQLiteDataSet.Open;
begin

end;

constructor TRQLiteDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRQLiteDataSet.Destroy;
begin
  inherited Destroy;
end;

end.
