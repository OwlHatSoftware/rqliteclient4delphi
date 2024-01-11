unit utest.rqlite;

interface

uses
  DUnitX.TestFramework,
  urqlite.client,
  urqlite.net;

type

  [TestFixture]
  TRQLiteTest = class
  private
    rqliteClient: IRqliteClient;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure TestSelectAll;
    // Test with TestCase Attribute to supply parameters.
    // [Test]
    // [TestCase('TestA', '1,2')]
    // [TestCase('TestB', '3,4')]
    // procedure Test2(const AValue1: Integer; const AValue2: Integer);
  end;

implementation

uses System.Classes, System.SysUtils, System.DateUtils;

procedure TRQLiteTest.Setup;
const
  sStory = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. '+
  'Vestibulum lobortis venenatis enim, eu tristique massa molestie eget.'+
  ' Maecenas nec dictum felis, sit amet vestibulum tortor. Fusce a rutrum '+
  'elit. Phasellus varius gravida nisl, quis ultrices velit convallis id. '+
  'Ut finibus erat nec sem molestie, id porttitor odio pulvinar. Nullam '+
  'libero orci, mattis at rhoncus vel, interdum laoreet nunc. Aliquam '+
  'scelerisque tempor ligula, sit amet condimentum risus pretium at.';
var
  Sql: TStringList;
begin
  rqliteClient := TRqliteClientFactory.CreateInstance(TNetHttpClientFactory.CreateInstance);
  rqliteClient.Hostname := 'localhost';
  rqliteClient.Port := 4001;
  rqliteClient.Database := 'foo';
  Sql := TStringList.Create;
  try
    Sql.Add('DROP TABLE IF EXISTS foo');
    Sql.Add('CREATE TABLE foo (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, name TEXT, age INTEGER, birthdate NUMERIC, profile BLOB)');
    Formatsettings.DecimalSeparator := '.';
    Sql.Add(Format('INSERT INTO foo(name, age, birthdate, profile) VALUES("%s", %d, %f, %s)',['Fiona', 20, EncodeDate(2004, 10, 15), HexValue(TEncoding.UTF8.GetBytes(sStory))]));
    Sql.Add(Format('INSERT INTO foo(name, age, birthdate, profile) VALUES("%s", %d, %f, %s)',['Betty', 32, EncodeDate(2004, 10, 15), HexValue(TEncoding.UTF8.GetBytes(sStory))]));
    Sql.Add(Format('INSERT INTO foo(name, age, birthdate, profile) VALUES("%s", %d, %f, %s)',['Peter', 50, EncodeDate(2004, 10, 15), HexValue(TEncoding.UTF8.GetBytes(sStory))]));
    Sql.Add(Format('INSERT INTO foo(name, age, birthdate, profile) VALUES("%s", %d, %f, %s)',['Benjamin', 43, EncodeDate(2004, 10, 15), HexValue(TEncoding.UTF8.GetBytes(sStory))]));
    rqliteClient.Execute(Sql, True);
    Formatsettings.DecimalSeparator := ',';
  finally
    Sql.Free;
  end;
end;

procedure TRQLiteTest.TearDown;
begin
  rqliteClient := nil;
end;

procedure TRQLiteTest.TestSelectAll;
var
  Sql: TStringList;
  cn1, cn2: string;
  ct1, ct2: string;
  rv10, rv11, rv20, rv21: string;
begin
  Sql := TStringList.Create;
  try
    Sql.Add('SELECT * FROM foo');
    rqliteClient.Query(Sql);

    if rqliteClient.GetColumnNames.Count = 0 then
      Exit;
    cn1 := rqliteClient.GetColumnNames[0];
    cn2 := rqliteClient.GetColumnNames[1];

    if rqliteClient.GetColumnTypes.Count = 0 then
      Exit;
    ct1 := rqliteClient.GetColumnTypes[0];
    ct2 := rqliteClient.GetColumnTypes[1];

    if rqliteClient.GetRowCount = 0 then
      Exit;
    rv10 := rqliteClient.GetRow(0).Value[0];
    rv11 := rqliteClient.GetRow(0).Value[1];
    rv20 := rqliteClient.GetRow(1).Value[0];
    rv21 := rqliteClient.GetRow(1).Value[1];
  finally
    Sql.Free;
  end;
end;

// procedure TRQLiteTest.Test2(const AValue1: Integer; const AValue2: Integer);
// begin
// end;

initialization

TDUnitX.RegisterTestFixture(TRQLiteTest);

end.
