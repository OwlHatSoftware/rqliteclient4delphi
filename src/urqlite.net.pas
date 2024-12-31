unit urqlite.net;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

// ****************************************************************************
// This unit contains the networking part of the RQLiteClient4Delphi
// The HttpClientFactory class contains the avalable networking libraries you
// can use, currently available:
// - Indy
// -.....
// ****************************************************************************
// Created by: OwlHatSoftware
// License: GNU General Public License v3.0
// ****************************************************************************

interface

uses
  {$IFnDEF FPC}
  System.Classes
  {$ELSE}
  Classes
  {$ENDIF};

type
  IHttpClient = interface
    ['{C059D872-3B5B-4A77-B9BE-BC5188363D87}']
    function Get(const URI: string): string;
    function Post(const URI: string; const AData: TStream): string;
    function Delete(const URI: string; const AData: TStream): string;
  end;

  { THttpClientFactory }

  THttpClientFactory = class
    class function CreateIndyInstance: IHttpClient;
    class function CreateSynapseInstance: IHttpClient;
  end;

implementation


uses urqlite.net.indy, urqlite.net.synapse;

{ THttpClientFactory }

class function THttpClientFactory.CreateIndyInstance: IHttpClient;
begin
  Result := TIndyHttpClient.Create;
end;

class function THttpClientFactory.CreateSynapseInstance: IHttpClient;
begin
  result := TSynapseHttpClient.Create;
end;

end.
