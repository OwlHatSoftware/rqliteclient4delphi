unit utest.rqlite.monitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, urqlite.monitor, urqlite.net;

type

  TTestRQLiteMonitor= class(TTestCase)
  private
    FRQLiteMonitor: IRqliteMonitor;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestRQLiteMonitor.TestHookUp;
begin
  Fail('Write your own test');
end;

procedure TTestRQLiteMonitor.SetUp;
begin
  FRQLiteMonitor := TRqliteMonitorFactory.CreateInstance(THttpClientFactory.CreateIndyInstance);
end;

procedure TTestRQLiteMonitor.TearDown;
begin
  FRQLiteMonitor := nil;
end;

initialization

  RegisterTest(TTestRQLiteMonitor);
end.

