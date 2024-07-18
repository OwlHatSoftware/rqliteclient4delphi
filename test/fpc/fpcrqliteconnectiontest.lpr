program fpcrqliteconnectiontest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, indylaz, GuiTestRunner, utest.rqlite.connection,
  urqlite.rqliteconnection;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

