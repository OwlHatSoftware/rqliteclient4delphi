unit utest.rqlite.monitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, urqlite.monitor;

type

  TTestRQLiteMonitor= class(TTestCase)
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

end;

procedure TTestRQLiteMonitor.TearDown;
begin

end;

initialization

  RegisterTest(TTestRQLiteMonitor);
end.

