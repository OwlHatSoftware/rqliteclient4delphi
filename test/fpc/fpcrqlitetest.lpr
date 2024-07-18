program fpcrqlitetest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, indylaz, GuiTestRunner, utest.rqlite.integration,
  urqlite.client, urqlite.net, urqlite.rqliteconnection;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

