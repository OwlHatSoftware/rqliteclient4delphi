unit urqlite.net.synapse;

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
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
  urqlite.net,
  httpsend,
  synacode;

type

  { TSynapseHttpClient }

  TSynapseHttpClient = class(TInterfacedObject, IHttpClient)
  private
    function Get(const URI: string): string;
    function Post(const URI: string; const AData: TStream): string;
    function Delete(const URI: string; const AData: TStream): string;
  end;

implementation


{ TSynapseHttpClient }

function TSynapseHttpClient.Get(const URI: string): string;
var
  SL: TStringList;
  S: string;
begin
  Result := '';
  SL := TStringList.Create;
  try
    if HttpGetText(EncodeURL(URI), SL) then
    begin
      S := SL.Text;
      S  := StringReplace(S, #13, '',[rfReplaceAll]);
      Result := UTF8Encode(S)
    end
    else
      raise Exception.Create(Format('Unable to GET from: %s!', [EncodeURL(URI)]));
  finally
  end;
end;

function TSynapseHttpClient.Post(const URI: string; const AData: TStream): string;
var
  HTTPClient: THTTPSend;
  SS: TStringStream;
begin
  Result := '';
  HTTPClient := THTTPSend.Create;
  try
    //HTTPClient.Document.CopyFrom(AData, 0);
    HTTPClient.Document.LoadFromStream(AData);
    HTTPClient.MimeType := 'application/json';
    if HTTPClient.HTTPMethod('POST', EncodeURL(URI)) then
    begin
      HTTPClient.Document.Position := 0;
      SS := TStringStream.Create;
      try
        SS.LoadFromStream(HTTPClient.Document);
        Result := UTF8Encode(SS.DataString);
      finally
        SS.Free;
      end;
      //HTTPClient.Document.WriteAnsiString(Result);
    end
    else
      raise Exception.Create(Format('Unable to POST data to: %s!', [EncodeURL(URI)]));
  finally
    HTTPClient.Free;
  end;
end;

function TSynapseHttpClient.Delete(const URI: string; const AData: TStream): string;
var
  HTTPClient: THTTPSend;
begin
  Result := '';
  HTTPClient := THTTPSend.Create;
  try
    HTTPClient.Document.LoadFromStream(AData);
    HTTPClient.MimeType := 'application/json';
    if HTTPClient.HTTPMethod('DELETE', EncodeURL(URI)) then
    begin
      HTTPClient.Document.Position := 0;
      HTTPClient.Document.WriteAnsiString(Result);
    end
    else
      raise Exception.Create(Format('Unable to DELETE data from: %s!',
        [EncodeURL(URI)]));
  finally
    HTTPClient.Free;
  end;
end;

end.
