unit urqlite.net.indy;

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
  System.Classes,
  System.SysUtils,
  {$ELSE}
  Classes,
  SysUtils,
  {$ENDIF}
  IdGlobalProtocols,
  IdHttp,
  IdURI,
  urqlite.net;

type

  { TIdHTTPAccess }

  TIdHTTPAccess = class(TIdHTTP)
    function Delete(AURL: string; ASource: TStream): string; overload;
  end;

  { TIndyHttpClient }

  TIndyHttpClient = class(TInterfacedObject, IHttpClient)
  private
    FHTTPClient: TIdHTTPAccess;
    function Get(const URI: string): string;
    function Post(const URI: string; const AData: TStream): string;
    function Delete(const URI: string; const AData: TStream): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TIdHTTPAccess }

function TIdHTTPAccess.Delete(AURL: string; ASource: TStream
  ): string;
var
  LResponse: TMemoryStream;
begin
  LResponse := TMemoryStream.Create;
  try
    DoRequest('DELETE', AURL, ASource, LResponse, []);
    LResponse.Position := 0;
    Result := ReadStringAsCharset(LResponse, Response.Charset{$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF});
    // TODO: if the data is XML, add/update the declared encoding to 'UTF-16LE'...
  finally
    FreeAndNil(LResponse);
  end;
end;

  { TIndyHttpClient }

constructor TIndyHttpClient.Create;
begin
  FHTTPClient := TIdHTTPAccess.Create;
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
  Result := '{"results": []}';
  response := FHTTPClient.Get(TIdURI.UrlEncode(URI));
  if FHTTPClient.ResponseCode <> 200 then
    raise Exception.Create(Format('Received invalid responsecode: %d',[FHTTPClient.ResponseCode]));
  if response = '' then
    raise Exception.Create('No content!');
  Result := response;
end;

function TIndyHttpClient.Post(const URI: string; const AData: TStream): string;
var
  response: string;
begin
  Result := '{"results": []}';
  response := FHTTPClient.Post(TIdURI.UrlEncode(URI), AData);
  if FHTTPClient.ResponseCode <> 200 then
    raise Exception.Create(Format('Received invalid responsecode: %d',[FHTTPClient.ResponseCode]));
  if response = '' then
    raise Exception.Create('No content!');
  Result := response;
end;

function TIndyHttpClient.Delete(const URI: string; const AData: TStream): string;
var
  response: string;
begin
  Result := '{"results": []}';
  response := FHTTPClient.Delete(TIdURI.UrlEncode(URI), AData);
  if FHTTPClient.ResponseCode <> 200 then
    raise Exception.Create(Format('Received invalid responsecode: %d',[FHTTPClient.ResponseCode]));
  if response = '' then
    raise Exception.Create('No content!');
  Result := response;
end;

end.

