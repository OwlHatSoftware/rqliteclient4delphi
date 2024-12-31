program fpcrqliteconnectiontest;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$endif}
  Interfaces,
  Forms,
  indylaz,
  GuiTestRunner,
  urqlite.rqliteconnection,
  urqlite.rqlitedataset,
  urqlite.monitor,
  utest.rqlite.connection,
  utest.rqlite.monitor;

  {$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
