program democontainer;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

uses
  {$ifdef FPC}
  {$ifdef UNIX}
    cthreads, cmem,
  {$endif UNIX}
  //Interfaces,
  SysUtils,
  Dos,
  laz_synapse,
  //indylaz,
  {$else FPC}
  //TODO: Delphi implementation
  {$endif FPC}
  Horse,
  Horse.Logger,
  Horse.Logger.Provider.Console,
  Horse.Core.Param.Field,
  urqlite.net,
  urqlite.client,
  urqlite.monitor, urqlite.net.synapse, urqlite.net.indy
  ;

const
  cNodesURI = 'http://%s:%d/nodes?timeout=10s';
  cStatusURI = 'http://%s:%d/status?pretty';

var
  //Env variables to be set in docker file
  RQLITE_HOSTNAME: string;
  RQLITE_PORT: integer;

  procedure GetPing(Req: THorseRequest; Res: THorseResponse);
  begin
    Res.Send('Pong');
  end;

  procedure GetRQLiteStatus(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
  var
    HttpClient: IHttpClient;
  begin
    HttpClient := THttpClientFactory.CreateSynapseInstance;
    try
      {$IFDEF DEBUG}
      Res.Send(HttpClient.Get(Format(cStatusURI,['localhost', 4001])));
      {$ELSE}
      Res.Send(HttpClient.Get(Format(cStatusURI,[GetEnv('RQLITE-HOSTNAME'), StrToInt(GetEnv('RQLITE-PORT'))])));
      {$ENDIF}
    finally
      HttpClient := nil;
    end;
  end;

  procedure GetRQLiteNodes(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
  var
    HttpClient: IHttpClient;
  begin
    HttpClient := THttpClientFactory.CreateSynapseInstance;
    try
      {$IFDEF DEBUG}
      Res.Send(HttpClient.Get(Format(cNodesURI,['localhost', 4001])));
      {$ELSE}
      Res.Send(HttpClient.Get(Format(cNodesURI,[GetEnv('RQLITE-HOSTNAME'), StrToInt(GetEnv('RQLITE-PORT'))])));
      {$ENDIF}
    finally
      HttpClient := nil;
    end;
  end;


begin
  // Here you will define the logprovider that will be used.
  THorseLoggerManager.RegisterProvider(THorseLoggerProviderConsole.New());

  // It's necessary to add the middleware to Horse:
  THorse.Use(THorseLoggerManager.HorseCallback);

  // Endpoints
  THorse.Get('/ping', @GetPing);
  THorse.Get('/rqlite/status', @GetRQLiteStatus);
  THorse.Get('/rqlite/nodes', @GetRQLiteNodes);

  //Host and Port
  THorse.Host := 'localhost';
  THorse.Port := 8080;
  THorse.Listen;
end.
