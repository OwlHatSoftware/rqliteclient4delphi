unit urqlite.net;
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
  System.Classes;

type
  IHttpClient = interface
    ['{C059D872-3B5B-4A77-B9BE-BC5188363D87}']
    function Get(const URI: string): string;
    function Post(const URI: string; const AData: TStream): string;
  end;

  THttpClientFactory = class
    class function CreateIndyInstance: IHttpClient;
  end;

implementation

uses
  System.SysUtils,
  IdHttp,
  IdURI;

type
  TIndyHttpClient = class(TInterfacedObject, IHttpClient)
  private
    FHTTPClient: TIdHTTP;
    function Get(const URI: string): string;
    function Post(const URI: string; const AData: TStream): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TIndyHttpClient }

constructor TIndyHttpClient.Create;
begin
  FHTTPClient := TIdHTTP.Create;
  FHTTPClient.Request.CustomHeaders.Values['Content-Type'] := 'application/json';
end;

destructor TIndyHttpClient.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TIndyHttpClient.Get(const URI: string): string;
var
  response: string;
begin
  result := '{"results": []}';
  response := FHTTPClient.Get(TIdURI.UrlEncode(URI));
  if response = '' then
    raise Exception.Create('No HTTP response!');
  result := response;
end;

function TIndyHttpClient.Post(const URI: string; const AData: TStream): string;
var
  response: string;
begin
  result := '{"results": []}';
  response := FHTTPClient.Post(TIdURI.UrlEncode(URI), AData);
  if response = '' then
    raise Exception.Create('No HTTP response!');
  result := response;
end;

{ THttpClientFactory }

class function THttpClientFactory.CreateIndyInstance: IHttpClient;
begin
  result := TIndyHttpClient.Create;
end;

end.
