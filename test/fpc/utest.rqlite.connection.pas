unit utest.rqlite.connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, SQLDB, urqlite.rqliteconnection;

type

  { TTestRQLiteConnection }

  TTestRQLiteConnection = class(TTestCase)
  private
    FRQliteConnection: TRQLiteConnection;
    FSQLQuery: TSQLQuery;
    FSQLTransaction: TSQLTransaction;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnection;
    procedure TestNodeFailure;
  end;

implementation

procedure TTestRQLiteConnection.TestConnection;
begin
  FRQliteConnection.Open;
  // Wait for monitoring thread to start
  //Sleep(6000);
end;

procedure TTestRQLiteConnection.TestNodeFailure;
begin
  FRQliteConnection.Open;
  FRQliteConnection.RemoveNode(2);
end;

procedure TTestRQLiteConnection.SetUp;
begin
  //Set the Connection component
  FRQliteConnection := TRQLiteConnection.Create(nil);
  FRQliteConnection.Database := 'MyDB';
  FRQliteConnection.HostName := 'localhost';
  FRQliteConnection.Port := 4001;
  FRQliteConnection.PathToRQLiteCLI:='C:\Users\JaccoUijlenhoet\Documents\rqlite';
end;

procedure TTestRQLiteConnection.TearDown;
begin
  FRQliteConnection.Free;
end;

initialization

  RegisterTest(TTestRQLiteConnection);
end.
