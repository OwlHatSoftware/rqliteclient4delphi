![](https://github.com/OwlHatSoftware/rqliteclient4delphi/blob/main/doc/logo-text.png)
=================

# rqliteClient4Delphi

rqlite is a relational database which combines SQLite's simplicity with the power of a robust, fault-tolerant, distributed system. It's designed for easy deployment and lightweight operation, offering a developer-friendly and operator-centric solution for Linux, macOS, and Windows, as well as various CPU platforms. 

The rqliteClient4Delphi is a library aimed at Delphi developers (in the future hopefully fpc/Lazarus as well). The library is a simple HTTP(S) client that communicates with the rqlite API.

It provides two units (urqlite.client.pas and urqlite.net.pas). The first unit contains the class TRqliteClient with corresponding interface IRqliteClient. The latter has the networking capabilities and must be inserted using constructor injection. like this:

```Delphi
rqliteClient := TRqliteClientFactory.CreateInstance(THttpClientFactory.CreateIndyInstance);
  ```

## Using rqliteClient4Delphi 
The instance gives the opportunity to 

### Setting up your instance
```Delphi
var
  rqliteClient: IRqliteClient;
  
// Create an instance of the Rqlite Client an setup basic parameters
rqliteClient := TRqliteClientFactory.CreateInstance(THttpClientFactory.CreateIndyInstance);
rqliteClient.Hostname := 'localhost';
rqliteClient.Port := 4005;
rqliteClient.Database := 'employees';
  ```  
