unit utest.rqlite.connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, SQLDB, urqlite.rqliteconnection;

type

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
  end;

implementation

procedure TTestRQLiteConnection.TestConnection;
begin
  FRQliteConnection.Open;

  FSQLQuery.SQL.Add('select * from employee');
  FSQLQuery.Open;
end;

procedure TTestRQLiteConnection.SetUp;
begin
  //Set the Connection component
  FRQliteConnection := TRQLiteConnection.Create(nil);
  FRQliteConnection.DatabaseName := 'MyDB';
  FRQliteConnection.HostName := 'localhost';
  FRQliteConnection.Port := 4005;
  //Set Transaction component
  FSQLTransaction := TSQLTransaction.Create(nil);
  FRQliteConnection.Transaction := FSQLTransaction;
  FSQLTransaction.DataBase := FRQliteConnection;
  //Set TQuery component
  FSQLQuery := TSQLQuery.Create(nil);
  FSQLQuery.Transaction := FSQLTransaction;
  FSQLQuery.DataBase := FRQliteConnection;
end;

procedure TTestRQLiteConnection.TearDown;
begin
  FSQLTransaction.Free;
  FSQLQuery.Free;
  FRQliteConnection.Free;
end;

initialization

  RegisterTest(TTestRQLiteConnection);
end.
