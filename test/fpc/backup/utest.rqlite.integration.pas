unit utest.rqlite.integration;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  urqlite.client,
  urqlite.net;

type

  { TFPCRQLiteTest }

  TFPCRQLiteTest = class(TTestCase)
  private
    rqliteClient: IRqliteClient;
    procedure DropAndCreateTable;
    function ConvertFileToHex(AFileName: string): string;
    procedure SelectFromDB(ATableName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDropAndCreateTable;
    procedure TestGetColumnNames;
    procedure TestGetColumnTypes;
    procedure TestGetValues;
  end;

implementation

uses DateUtils;

const
  // organisation table
  c_ot_fld_id = 'id';
  c_ot_fld_id_type = 'INTEGER';
  c_ot_fld_name = 'name';
  c_ot_fld_name_type = 'TEXT';

  // employee table
  cfld_id = 'id';
  cfld_id_type = 'INTEGER';
  cfld_name = 'name';
  cfld_name_type = 'TEXT';
  cfld_age = 'age';
  cfld_age_type = 'INTEGER';
  cfld_birthdate = 'birthdate';
  cfld_birthdate_type = 'REAL';
  cfld_profile = 'profile';
  cfld_profile_type = 'BLOB';
  cfld_photo = 'photo';
  cfld_photo_type = 'BLOB';
  cfld_org = 'org_id';
  cfld_org_type = 'INTEGER';

  cStory = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' +
    'Vestibulum lobortis venenatis enim, eu tristique massa molestie eget.' +
    ' Maecenas nec dictum felis, sit amet vestibulum tortor. Fusce a rutrum ' +
    'elit. Phasellus varius gravida nisl, quis ultrices velit convallis id. ' +
    'Ut finibus erat nec sem molestie, id porttitor odio pulvinar. Nullam ' +
    'libero orci, mattis at rhoncus vel, interdum laoreet nunc. Aliquam ' +
    'scelerisque tempor ligula, sit amet condimentum risus pretium at.';

procedure DecodeFile(const base64: ansistring; const FileName: string);
var
  stream: TBytesStream;
begin
  stream := TBytesStream.Create(Base64ToBytes(base64));
  try
    stream.SaveToFile(FileName);
  finally
    stream.Free;
  end;
end;

procedure TFPCRQLiteTest.DropAndCreateTable;
var
  Sql: TStringList;
  OldFormatsettings: TFormatSettings;
begin
  Sql := TStringList.Create;
  try
    // Drop the existing table
    Sql.Add('DROP TABLE IF EXISTS organisation');
    Sql.Add('DROP TABLE IF EXISTS employees');
    // Create the nwe organisation table
    Sql.Add(Format(
      'CREATE TABLE organisation (%s %s NOT NULL PRIMARY KEY AUTOINCREMENT, %s %s)',
      [c_ot_fld_id, c_ot_fld_id_type, c_ot_fld_name, c_ot_fld_name_type]));
    // Create the new employee table
    Sql.Add(Format(
      'CREATE TABLE employees (%s %s NOT NULL PRIMARY KEY AUTOINCREMENT, %s %s, %s %s, %s %s, %s %s, %s %s, org_id INTEGER NOT NULL, FOREIGN KEY (org_id) REFERENCES organisation (id) )', [cfld_id, cfld_id_type, cfld_name, cfld_name_type, cfld_age, cfld_age_type, cfld_birthdate, cfld_birthdate_type, cfld_profile, cfld_profile_type, cfld_photo, cfld_photo_type]));
    OldFormatsettings := Formatsettings; // Temp store old formatsettings
    Formatsettings.DecimalSeparator := '.';
    // set the decimal representation for float fields (NUMERIC/REAL in SQLite) to '.'
    // Add a record to organisation table
    Sql.Add(Format('INSERT INTO organisation(%s) VALUES("%s")',
      [c_ot_fld_name, 'Unilever']));
    // Add a second record to organisation table
    Sql.Add(Format('INSERT INTO organisation(%s) VALUES("%s")',
      [c_ot_fld_name, 'Shell']));
    // Add a record to employee table
    Sql.Add(Format(
      'INSERT INTO employees(%s, %s, %s, %s, %s, %s) VALUES("%s", %d, %f, %s, %s, %d)',
      [cfld_name, cfld_age, cfld_birthdate, cfld_profile, cfld_photo,
      cfld_org, 'Fiona', 20, EncodeDate(2004, 10, 15),
      BytesToHexString(TEncoding.UTF8.GetBytes(cStory)),
      ConvertFileToHex('RAD.png'), 1]));
    // Add a second record to employee table
    Sql.Add(Format(
      'INSERT INTO employees(%s, %s, %s, %s, %s, %s) VALUES("%s", %d, %f, %s, %s, %d)',
      [cfld_name, cfld_age, cfld_birthdate, cfld_profile, cfld_photo,
      cfld_org, 'Peter', 50, EncodeDate(1974, 6, 2),
      BytesToHexString(TEncoding.UTF8.GetBytes(cStory)),
      ConvertFileToHex('RAD.png'), 2]));
    // Write it to the database as One transaction
    rqliteClient.Execute(Sql, True);
    Formatsettings := OldFormatsettings; // Restore old formatsettings
  finally
    Sql.Free;
  end;
end;

function TFPCRQLiteTest.ConvertFileToHex(AFileName: string): string;
var
  LFileStream: TFileStream;
  BytesRead : Integer;
  LBuffer : array of byte;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LFileStream.Position := 0;
    SetLength(LBuffer, LFileStream.Size);
    BytesRead := LFileStream.Read(LBuffer[0], LFileStream.Size);
    Result := BytesToHexString(LBuffer);
  finally
    LFileStream.Free;
  end;
end;

procedure TFPCRQLiteTest.SelectFromDB(ATableName: string);
var
  Sql: TStringList;
begin
  Sql := TStringList.Create;
  try
    Sql.Add(Format('SELECT * FROM %s', [ATableName]));
    rqliteClient.Query(Sql);
  finally
    Sql.Free;
  end;
end;

procedure TFPCRQLiteTest.SetUp;
begin
  // Create an instance of the Rqlite Client an setup basic parameters
  rqliteClient := TRqliteClientFactory.CreateInstance(
    THttpClientFactory.CreateIndyInstance);
  rqliteClient.Hostname := 'localhost';
  rqliteClient.Port := 4005;
  rqliteClient.Database := 'employees';
end;

procedure TFPCRQLiteTest.TearDown;
begin
  rqliteClient := nil;
end;

procedure TFPCRQLiteTest.TestDropAndCreateTable;
begin
  DropAndCreateTable;
  SelectFromDB('organisation');
  AssertEquals(2, rqliteClient.GetColumnNames.Count);
  AssertEquals(2, rqliteClient.GetColumnTypes.Count);
  SelectFromDB('employees');
  AssertEquals(7, rqliteClient.GetColumnNames.Count);
  AssertEquals(7, rqliteClient.GetColumnTypes.Count);
end;

procedure TFPCRQLiteTest.TestGetColumnNames;
var
  cn0, cn1, cn2, cn3, cn4: string;
begin
  DropAndCreateTable;

  SelectFromDB('employees');
  AssertEquals(7, rqliteClient.GetColumnNames.Count);
  cn0 := rqliteClient.GetColumnNames[0];
  AssertEquals(cfld_id, cn0);
  cn1 := rqliteClient.GetColumnNames[1];
  AssertEquals(cfld_name, cn1);
  cn2 := rqliteClient.GetColumnNames[2];
  AssertEquals(cfld_age, cn2);
  cn3 := rqliteClient.GetColumnNames[3];
  AssertEquals(cfld_birthdate, cn3);
  cn4 := rqliteClient.GetColumnNames[4];
  AssertEquals(cfld_profile, cn4);
end;

procedure TFPCRQLiteTest.TestGetColumnTypes;
var
  ct0, ct1, ct2, ct3, ct4: string;
begin
  DropAndCreateTable;
  SelectFromDB('employees');
  AssertEquals(7, rqliteClient.GetColumnTypes.Count);
  ct0 := rqliteClient.GetColumnTypes[0];
  AssertEquals(cfld_id_type, UpperCase(ct0));
  ct1 := rqliteClient.GetColumnTypes[1];
  AssertEquals(cfld_name_type, UpperCase(ct1));
  ct2 := rqliteClient.GetColumnTypes[2];
  AssertEquals(cfld_age_type, UpperCase(ct2));
  ct3 := rqliteClient.GetColumnTypes[3];
  AssertEquals(cfld_birthdate_type, UpperCase(ct3));
  ct4 := rqliteClient.GetColumnTypes[4];
  AssertEquals(cfld_profile_type, UpperCase(ct4));
end;

procedure TFPCRQLiteTest.TestGetValues;
const
  // Returned BLOB value is in base64 encoded format
  cBlobVal = UnicodeString('TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2N' +
    'pbmcgZWxpdC4gVmVzdGlidWx1bSBsb2JvcnRpcyB2ZW5lbmF0aXMgZW5pbSwgZXUgdHJpc3Rpc' +
    'XVlIG1hc3NhIG1vbGVzdGllIGVnZXQuIE1hZWNlbmFzIG5lYyBkaWN0dW0gZmVsaXMsIHNpdCBh' +
    'bWV0IHZlc3RpYnVsdW0gdG9ydG9yLiBGdXNjZSBhIHJ1dHJ1bSBlbGl0LiBQaGFzZWxsdXMgdmF' +
    'yaXVzIGdyYXZpZGEgbmlzbCwgcXVpcyB1bHRyaWNlcyB2ZWxpdCBjb252YWxsaXMgaWQuIFV0IG' +
    'ZpbmlidXMgZXJhdCBuZWMgc2VtIG1vbGVzdGllLCBpZCBwb3J0dGl0b3Igb2RpbyBwdWx2aW5hc' +
    'i4gTnVsbGFtIGxpYmVybyBvcmNpLCBtYXR0aXMgYXQgcmhvbmN1cyB2ZWwsIGludGVyZHVtIGxh' +
    'b3JlZXQgbnVuYy4gQWxpcXVhbSBzY2VsZXJpc3F1ZSB0ZW1wb3IgbGlndWxhLCBzaXQgYW1ldCB' +
    'jb25kaW1lbnR1bSByaXN1cyBwcmV0aXVtIGF0Lg==');
var
  rv00, rv01, rv02, rv03, rv04: string;
  rv10, rv11, rv12, rv13, rv14: string;
  BlobVal: UnicodeString;
begin
  DropAndCreateTable;
  SelectFromDB('employees');
  AssertEquals(2, rqliteClient.GetRowCount);
  rv00 := rqliteClient.GetRow(0).Value[0];
  AssertEquals('1', rv00);
  rv01 := rqliteClient.GetRow(0).Value[1];
  AssertEquals('Fiona', rv01);
  rv02 := rqliteClient.GetRow(0).Value[2];
  AssertEquals('20', rv02);
  rv03 := rqliteClient.GetRow(0).Value[3];
  AssertEquals('38275', rv03);
  rv04 := rqliteClient.GetRow(0).Value[4];
  AssertEquals(cBlobVal, rv04);
  rv10 := rqliteClient.GetRow(1).Value[0];
  AssertEquals('2', rv10);
  rv11 := rqliteClient.GetRow(1).Value[1];
  AssertEquals('Peter', rv11);
  rv12 := rqliteClient.GetRow(1).Value[2];
  AssertEquals('50', rv12);
  rv13 := rqliteClient.GetRow(1).Value[3];
  AssertEquals('27182', rv13);
  rv14 := rqliteClient.GetRow(1).Value[4];
  AssertEquals(cBlobVal, rv14);
  BlobVal := StringOf(Base64ToBytes(rv14));
  AssertEquals(cStory, BlobVal);
end;

initialization

  RegisterTest(TFPCRQLiteTest);
end.
