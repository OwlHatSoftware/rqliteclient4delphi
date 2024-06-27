![](https://github.com/OwlHatSoftware/rqliteclient4delphi/blob/main/doc/logo-text.png)
=================

# rqliteClient4Delphi

rqlite is a relational database which combines SQLite's simplicity with the power of a robust, fault-tolerant, distributed system. It's designed for easy deployment and lightweight operation, offering a developer-friendly and operator-centric solution for Linux, macOS, and Windows, as well as various CPU platforms. 

The rqliteClient4Delphi is a library aimed at Delphi developers (in the future hopefully fpc/Lazarus as well). The library is a simple HTTP(S) client that communicates with the rqlite API. It currently uses an Indy implementation as networking solution. But you can easily add another networking library. Checkout the file: urqlite.net.pas, on how to implent it. 

It provides two units (urqlite.client.pas and urqlite.net.pas). The first unit contains the class TRqliteClient with corresponding interface IRqliteClient. The latter has the networking capabilities and must be inserted using constructor injection. like this:

```pascal
rqliteClient := TRqliteClientFactory.CreateInstance(THttpClientFactory.CreateIndyInstance);
  ```

## Using rqliteClient4Delphi 
The parts below give a comprehensive guide on how to setup and use rqlite4Delphi. Take a look at the unit tests for more in depth knowledge.

### Setting up your instance
```pascal
var
  rqliteClient: IRqliteClient;
  
// Create an instance of the Rqlite Client an setup basic parameters
rqliteClient := TRqliteClientFactory.CreateInstance(THttpClientFactory.CreateIndyInstance);
rqliteClient.Hostname := 'localhost';
rqliteClient.Port := 4005;
rqliteClient.Database := 'employees';
  ```  
### Injecting SQL
Injecting SQL is as easy as creating a TStringlist instance and adding the SQL commands to the instance.
Then execute it using: rqliteClient.Execute(Sql, True);. The boolean parameter at the end allows you to do a batch execution of the SQL as if it is a transaction. Blobs have to be converted to a HexValue, like this: HexValue(TEncoding.UTF8.GetBytes(sStory)).

```pascal
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
  ```
  
### Retrieving data 
You can retrieve your data by using an SQL select statement. Like the example below:

```pascal
  Sql := TStringList.Create;
  
  Sql.Add('SELECT * FROM foo');
  rqliteClient.Query(Sql);
  ```

The *columnnames* can be retrieved like this:

```pascal
  cn1 := rqliteClient.GetColumnNames[0];
  cn2 := rqliteClient.GetColumnNames[1];
  ```

And the *columntypes*..:

```pascal
  ct1 := rqliteClient.GetColumnTypes[0];
  ct2 := rqliteClient.GetColumnTypes[1];
  ```

And the *row/column values* like:

```pascal
  rv10 := rqliteClient.GetRow(0).Value[0];
  rv11 := rqliteClient.GetRow(0).Value[1];
  rv20 := rqliteClient.GetRow(1).Value[0];
  rv21 := rqliteClient.GetRow(1).Value[1];
  ```
### Collaboration
Please feel free to suggest additions/improvements to this library. If you have any proposals contact me through the rqlite slack channel, or start a discussion on the GitHub page.

Best regards, Jacco Uijlenhoet