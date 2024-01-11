unit urqlite.net;

interface

uses
  System.Classes;

type
  IHttpClient = interface
    ['{C059D872-3B5B-4A77-B9BE-BC5188363D87}']
    function Get(const URI: string): string;
    function Post(const URI: string; const AData: TStream): string;
  end;

  TNetHttpClientFactory = class
    class function CreateInstance: IHttpClient;
  end;

implementation

uses
  System.SysUtils,
  IdHttp,
  IdURI;

type
  TNetHttpClient = class(TInterfacedObject, IHttpClient)
  private
    FHTTPClient: TIdHTTP;
    function Get(const URI: string): string;
    function Post(const URI: string; const AData: TStream): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TNetHttpClient }

constructor TNetHttpClient.Create;
begin
  FHTTPClient := TIdHTTP.Create;
  FHTTPClient.Request.CustomHeaders.Values['Content-Type'] := 'application/json';
end;

destructor TNetHttpClient.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TNetHttpClient.Get(const URI: string): string;
var
  response: string;
begin
  result := '{"results": []}';
  response := FHTTPClient.Get(TIdURI.UrlEncode(URI));
  if response = '' then
    raise Exception.Create('No HTTP response!');
//  if response.StatusCode >= 400 then
//    raise Exception.Create(Format('Http Error: %d, %s', [response.StatusCode, response.StatusText]));
  result := response;//.ContentAsString(TEncoding.UTF8);
end;

function TNetHttpClient.Post(const URI: string; const AData: TStream): string;
var
  response: string;
begin
  result := '{"results": []}';
  response := FHTTPClient.Post(TIdURI.UrlEncode(URI), AData);
  if response = '' then
    raise Exception.Create('No HTTP response!');
//  if response.StatusCode >= 400 then
//    raise Exception.Create(Format('Http Error: %d, %s', [response.StatusCode, response.StatusText]));
  result := response;//.ContentAsString(TEncoding.UTF8);
end;

{ TNetHttpClientFactory }

class function TNetHttpClientFactory.CreateInstance: IHttpClient;
begin
  result := TNetHttpClient.Create;
end;

end.
