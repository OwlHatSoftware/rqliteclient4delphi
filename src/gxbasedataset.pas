unit gxbasedataset;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{*******************************************************************************
 This source code implements a base class for deriving custom datasets.
 It is described in the article "Writing Custom Datasets" which used to be
 available at http://www.gexperts.org/CustomDS.html. But the page seems to
 be removed.

 GExperts Inc.
 www.gexperts.org

 FPC modifications and additional improvements made by Alexander Todorov.
 e-mail: alexx.todorov@gmail.com
 Updated for FPC and D10 Seattle and up as part of the Simple Middleware 
 Framework
 available at ....
 
 *****************************************************************************
 *                                                                           *
 *  Since I could not contact the original author and found that this source *
 *  is used in other TDataset implementations (see TjanXMLDataset)           *
 *  it is distributed under modified Library GNU General Public License!     *
 *  See the file COPYING included in this distribution,                      *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
*******************************************************************************}

interface

uses Classes, SysUtils, DateUtils, Variants, Forms, StrUtils,
     {$IFDEF FPC} DB {$ELSE} System.WideStrUtils, Data.DB{$ENDIF};

resourcestring
  FT_NOT_SUPPORTED = 'Field type not supported';

const
  (*** UNSUPPORTED FIELD TYPES ***)
  UNSUPPORTED = [ftBlob, ftMemo, ftGraphic,
    // <--- BLOBS ARE NOT TESTED BUT ITS SUPPOSED TO WORK
    ftFmtMemo, {ftUnknown,} ftWord, ftBytes, ftVarBytes, {ftAutoInc,}
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    ftWideString, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob,
    ftOraClob, ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd];
  (*****************************************************************************
    To implement support for specific field type several thing must be done :
    1) add ftXXX to GetDataSize - this returns how many bytes are used to
       store a single value from the given data type. Most cases this is SizeOf(TypeXXX)
    2) add ftXXX to GetFieldOffset - this returns the field offset in the buffer structure.
       this is a sum from all fields preceding a particular field of type XXX
    3) add ftXXX to BufferToRecord / RecordToBuffer - these two procedures interact with
       the internal buffer system and the higher-level variant system.
       N.B. since these procedures call GetFieldData / SetFieldData the actual data
       must be fitted in a Variant.
    4) add ftXXX to GetFieldData / SetFieldData - these functions interact with the
       internal buffer system.
    5) remove ftXXX from UNSUPPORTED set
  *****************************************************************************)
//The NEXTGEN compiler directive is added but implementation of it is work in progress

type

  PRecordInfo = ^TRecordInfo;

  TRecordInfo = record
    RecordID: Pointer;
    Bookmark: Pointer;
    BookMarkFlag: TBookmarkFlag;
  end;

  { TGXBaseDataset }

  TGXBaseDataset = class(TDataset)
  private
    FisOpen: boolean;
    FStartCalculated: integer;
    FBufferMap: TStringList;
    procedure FillBufferMap;
    function RecordFilter: boolean;
  protected   {My simplified methods to override}
    {$IFDEF VER2_0_1}
    {$WARNING This is a dirty hack! Be careful. NOT tested!}
    (* TDataset.TempBuffer is not present in FPC 2.0.1. It is present in older and newer releases *)
    (* It is implemented by TGXBaseDataSet for compatibility *)
    function TempBuffer: PChar;
    {$ENDIF}
    function DoOpen: boolean; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoDeleteRecord; virtual;
    procedure DoCreateFieldDefs; virtual; abstract;
    function GetFieldValue(Field: TField): variant; virtual; abstract;
    procedure SetFieldValue(Field: TField; Value: variant); virtual; abstract;
    procedure GetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    procedure SetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    //Called before and after getting a set of field values
    procedure DoBeforeGetFieldValue; virtual; abstract;
    procedure DoAfterGetFieldValue; virtual; abstract;
    procedure DoBeforeSetFieldValue(Inserting: boolean); virtual; abstract;
    procedure DoAfterSetFieldValue(Inserting: boolean); virtual; abstract;
    //Handle buffer ID
    function AllocateRecordID: Pointer; virtual; abstract;
    procedure DisposeRecordID(Value: Pointer); virtual; abstract;
    procedure GotoRecordID(Value: Pointer); virtual; abstract;
    //BookMark functions
    function GetBookMarkSize: integer; virtual; abstract;
    procedure DoGotoBookmark(ABookmark: Pointer); virtual; abstract;
    procedure AllocateBookMark(RecordID: Pointer; ABookmark: Pointer); virtual; abstract;
    //Navigation methods
    procedure DoFirst; virtual; abstract;
    procedure DoLast; virtual; abstract;
    function Navigate(GetMode: TGetMode): TGetResult; virtual; abstract;
    procedure SetFiltered(Value: boolean); override;
    //Internal isOpen property
    property isOpen: boolean read FisOpen;
    { TGXBaseDataset Internal functions that can be overriden if needed }
    function GetDataSize: integer; virtual;
    function GetFieldOffset(Field: TField): integer; virtual;
    {$IFDEF NEXTGEN}
    procedure AllocateBLOBPointers(Buffer: TRecBuf); virtual;
    procedure FreeBlobPointers(Buffer: TRecBuf); virtual;
    procedure FreeRecordPointers(Buffer: TRecBuf); virtual;
    procedure InternalInitRecord(Buffer: TRecBuf); overload; override;
    function AllocRecBuf: TRecBuf; override;
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); overload; override;
    procedure ClearCalcFields(Buffer: NativeInt); overload; override;
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; overload; override;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); overload; override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    {$ELSE}
    procedure AllocateBLOBPointers(Buffer: TRecordBuffer); virtual;
    procedure FreeBlobPointers(Buffer: TRecordBuffer); virtual;
    procedure FreeRecordPointers(Buffer: TRecordBuffer); virtual;
    procedure InternalInitRecord(Buffer: TRecordBuffer); overload;
      override; deprecated 'Use overloaded method instead';
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: boolean): TGetResult; overload; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); overload;
      override; deprecated 'Use overloaded method instead';
    procedure ClearCalcFields(Buffer: TRecordBuffer); overload; override;
      deprecated 'Use overloaded method instead';
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
      overload; override; deprecated 'Use overloaded method instead';
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
      overload; override; deprecated 'Use overloaded method instead';
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
      overload; override; deprecated 'Use overloaded method instead';
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
      overload; override; deprecated 'Use overloaded method instead';
    function GetActiveRecordBuffer: TRecordBuffer; virtual;
    procedure BufferToRecord(Buffer: TRecordBuffer); virtual;
    procedure RecordToBuffer(Buffer: TRecordBuffer); virtual;
    {$ENDIF NEXTGEN}
    function GetRecordSize: word; override;
    procedure InternalInsert; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalEdit; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalScroll;
    procedure InternalAddRecord(Buffer: Pointer; bAppend: boolean); override;
    function IsCursorOpen: boolean; override;
    function GetCanModify: boolean; override;
    {$IFNDEF FPC}
    {.$IFDEF VER300}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): boolean;
      overload; override;
    {.$ENDIF VER300}
    {$ENDIF !FPC}
    function GetFieldData(Field: TField; Buffer: Pointer): boolean; override;
    {$IFNDEF FPC}
    {.$IFDEF VER300}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    {.$ENDIF VER300}
    {$ENDIF !FPC}
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
  public
    procedure OpenDataSet;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGXBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TGXBaseDataSet;
    FMode: TBlobStreamMode;
    FModified: boolean;
    FOpened: boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
  end;

implementation

//var OldTimeSeparator, OldDateSeparator : Char;

// DateTime support function, translates a TDateTime type to TDateTimeRec
function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: Result.Date := TimeStamp.Date;
    ftTime: Result.Time := TimeStamp.Time;
    else
      Result.DateTime := TimeStampToMSecs(TimeStamp);
  end;
end;

// DateTime support function, translates a TDateTimeRec type to TDateTime
function NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
var
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftDate:
    begin
      TimeStamp.Time := 0;
      TimeStamp.Date := Data.Date;
    end;
    ftTime:
    begin
      TimeStamp.Time := Data.Time;
      TimeStamp.Date := DateDelta;
    end;
    else
      try
        TimeStamp := MSecsToTimeStamp(
{$IFDEF FPC}Int64{$ENDIF}
          (Data.DateTime));//Gets here
      except
        TimeStamp.Time := 0;
        TimeStamp.Date := 0;
      end;
  end;
  Result := TimeStampToDateTime(TimeStamp);
end;

{ TGXBaseDataset }

{$IFDEF VER2_0_1}
function TGXBaseDataset.TempBuffer : PChar;
begin
  Result := ActiveBuffer;
end;
{$ENDIF}

constructor TGXBaseDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FisOpen := False;
  FBufferMap := TStringList.Create;
end;

destructor TGXBaseDataset.Destroy;
begin
  if Active then
    Close;
  FBufferMap.Free;
  inherited Destroy;
end;

procedure TGXBaseDataset.FillBufferMap;
var
  Index: integer;
begin
  FBufferMap.Clear;
  for Index := 0 to FieldCount - 1 do
    FBufferMap.Add(Fields[Index].FieldName);
end;

procedure TGXBaseDataset.InternalOpen;
begin
  if DoOpen then
  begin
    OpenDataSet;
  end;
end;

function TGXBaseDataset.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, GetRecordSize);
  FillChar(Result^, GetRecordSize, 0);
  AllocateBlobPointers(Result);
end;

procedure TGXBaseDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeRecordPointers(Buffer);
  FreeMem(Buffer, GetRecordSize);
end;

procedure TGXBaseDataset.FreeRecordPointers(Buffer: TRecordBuffer);
begin
  FreeBlobPointers(Buffer);
  DisposeRecordID(PRecordInfo(Buffer + GetDataSize)^.RecordID);
  if PRecordInfo(Buffer + GetDataSize)^.BookMark <> nil then
  begin
    FreeMem(PRecordInfo(Buffer + GetDataSize)^.BookMark);
    PRecordInfo(Buffer + GetDataSize)^.BookMark := nil;
  end;
end;

procedure TGXBaseDataset.AllocateBLOBPointers(Buffer: TRecordBuffer);
var
  Index: integer;
  Offset: integer;
  Stream: TMemoryStream;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].IsBlob then
    begin
      Offset := GetFieldOffset(Fields[Index]);
      Stream := TMemoryStream.Create;
      Move(Pointer(Stream), (Buffer + Offset)^, SizeOf(Pointer));
    end;
end;

procedure TGXBaseDataset.FreeBlobPointers(Buffer: TRecordBuffer);
var
  Index: integer;
  Offset: integer;
  FreeObject: TObject;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].IsBlob then
    begin
      Offset := GetFieldOffset(Fields[Index]);
      Move((Buffer + Offset)^, Pointer(FreeObject), SizeOf(Pointer));
      if Assigned(FreeObject) then
        FreeObject.Free;
      FreeObject := nil;
      Move(Pointer(FreeObject), (Buffer + Offset)^, SizeOf(Pointer));
    end;
end;

procedure TGXBaseDataset.InternalInitFieldDefs;
begin
  DoCreateFieldDefs;
end;

procedure TGXBaseDataset.ClearCalcFields(Buffer: TRecordBuffer);
begin
  FillChar(Buffer[FStartCalculated], CalcFieldsSize, 0);
end;

function TGXBaseDataset.GetActiveRecordBuffer: TRecordBuffer;
begin
  case State of
    dsBrowse: if isEmpty then
        Result := nil
      else
        Result := TRecordBuffer(ActiveBuffer);
    dsCalcFields: Result := TRecordBuffer(CalcBuffer);
    dsFilter: Result := TRecordBuffer(TempBuffer);
    dsEdit, dsInsert: Result := TRecordBuffer(ActiveBuffer);
    else
      Result := nil;
  end;
end;

function TGXBaseDataset.GetCanModify: boolean;
begin
  Result := False;
end;

function TGXBaseDataset.RecordFilter: boolean;
var
  SaveState: TDataSetState;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
  begin
    SaveState := SetTempState(dsFilter);
    try
      RecordToBuffer(TRecordBuffer(TempBuffer));
      OnFilterRecord(Self, Result);
    except
      InternalHandleException;
    end;
    RestoreState(SaveState);
  end;
end;

function TGXBaseDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: boolean): TGetResult;
var
  localAccept: boolean;
begin
  localAccept := True;
  repeat
    Result := Navigate(GetMode);
    if (Result = grOk) then
    begin
      if Filtered then
        localAccept := RecordFilter;

      if localAccept then
      begin
        RecordToBuffer(Buffer);
        ClearCalcFields(Buffer);
        GetCalcFields(Buffer);
      end;
    end
    else if (Result = grError) and DoCheck then
      DatabaseError('No Records');
  until localAccept or (Result in [grEOF, grBOF]);
end;

function TGXBaseDataset.GetRecordSize: word;
var
  tmp: word;
begin
  tmp := GetDataSize + SizeOf(TRecordInfo);
  FStartCalculated := tmp;
  Result := tmp + CalcFieldsSize;
end;

function TGXBaseDataset.GetDataSize: integer;
var
  Index: integer;
  LDataType: TFieldType;
begin
  Result := 0;
  for Index := 0 to FieldCount - 1 do
  begin
    LDataType := Fields[Index].DataType;
    case LDataType of
      ftString, ftWideString: Result := Result + Fields[Index].DataSize + 1;
      //Leave space for terminating null
      //ftWideString: Result := Result + Round((FieldbyName(FBufferMap[Index]).Size +1) / 2);
      ftAutoInc, ftInteger, ftSmallInt: Result := Result + SizeOf(integer);
      ftLargeInt: Result := Result + SizeOf(LargeInt);
      ftDate, ftDateTime, ftTime: Result := Result + SizeOf(TDateTime);
      ftFloat, ftCurrency, ftBCD: Result := Result + SizeOf(double);
      ftBoolean: Result := Result + SizeOf(wordbool);
      ftUnknown: Result := 0;
      else
        if Fields[Index].IsBlob then
          Result := Result + SizeOf(Pointer)
        else if (Fields[Index].DataType in UNSUPPORTED) then
          raise Exception.Create('TGXBaseDataset.GetDataSize - ' + FT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGXBaseDataset.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  DoClose;
  FisOpen := False;
end;

procedure TGXBaseDataset.InternalDelete;
begin
  DoDeleteRecord;
end;

procedure TGXBaseDataset.InternalEdit;
begin
  if GetActiveRecordBuffer <> nil then
    InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TGXBaseDataset.InternalFirst;
begin
  DoFirst;
end;

procedure TGXBaseDataset.InternalScroll;
begin
  if GetActiveRecordBuffer <> nil then
    InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TGXBaseDataset.InternalHandleException;
begin
  {.$IFNDEF FPC}
  Application.HandleException(Self);
  {.$ENDIF}
end;

{This is called by the TDataset to initialize an already existing buffer.
We cannot just fill the buffer with 0s since that would overwrite our BLOB pointers.
Therefore we free the blob pointers first, then fill the buffer with zeros, then
reallocate the blob pointers}

procedure TGXBaseDataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FreeRecordPointers(Buffer);
  FillChar(Buffer^, GetRecordSize, 0);
  AllocateBlobPointers(Buffer);
end;

procedure TGXBaseDataset.InternalInsert;
begin

end;

procedure TGXBaseDataset.InternalLast;
begin
  DoLast;
end;

procedure TGXBaseDataset.InternalPost;
begin
  if FisOpen then
  begin
    DoBeforeSetFieldValue(State = dsInsert);
    BufferToRecord(GetActiveRecordBuffer);
    DoAfterSetFieldValue(State = dsInsert);
  end;
end;

procedure TGXBaseDataset.InternalAddRecord(Buffer: Pointer; bAppend: boolean);
begin
  if bAppend then
    InternalLast;
  DoBeforeSetFieldValue(True);
  BufferToRecord(Buffer);
  DoAfterSetFieldValue(True);
end;

procedure TGXBaseDataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  GotoRecordID(PRecordInfo(Buffer + GetDataSize)^.RecordID);
end;

function TGXBaseDataset.IsCursorOpen: boolean;
begin
  Result := FisOpen;
end;

function TGXBaseDataset.GetFieldOffset(Field: TField): integer;
var
  Index, FPos: integer;
begin
  Result := 0;
  FPos := FBufferMap.Indexof(Field.FieldName);
  for Index := 0 to FPos - 1 do
    case FieldByName(FBufferMap[Index]).DataType of
      ftString, ftWideString: Inc(Result, FieldByName(FBufferMap[Index]).DataSize + 1);
      //ftWideString: inc(Result, Round((FieldbyName(FBufferMap[Index]).Size + 1) / 2)); //widestring has 2bytes per character
      ftAutoInc, ftInteger, ftSmallInt: Inc(Result, SizeOf(integer));
      ftLargeInt: Inc(Result, SizeOf(LargeInt));
      ftDate, ftDateTime, ftTime: Inc(Result, SizeOf(TDateTime));
      ftFloat, ftBCD, ftCurrency: Inc(Result, SizeOf(double));
      ftBoolean: Inc(Result, SizeOf(wordbool));
      ftUnknown: Inc(Result, SizeOf(integer));
      else
        if Fields[Index].IsBlob then Result := Result + SizeOf(Pointer)
        else if (FieldByName(FBufferMap[Index]).DataType in UNSUPPORTED) then
          raise Exception.Create(
            'TGXBaseDataset.GetFieldOffset - ' + FT_NOT_SUPPORTED);
    end;
end;

procedure TGXBaseDataset.BufferToRecord(Buffer: TRecordBuffer);
var
  TempStr: ansistring;
  TempWStr: WideString;
  TempInt: integer;
  TempLInt: LargeInt;
  TempDouble: double;
  TempBool: wordbool;
  TempDateTime: TDateTime;
  Offset: integer;
  Index: integer;
  Stream: TStream;
begin
  for Index := 0 to FieldCount - 1 do
  begin
    Offset := GetFieldOffset(Fields[Index]);
    case Fields[Index].DataType of
      ftString:
      begin
        TempStr := pansichar(Buffer + Offset);
        SetFieldValue(Fields[Index], TempStr);
      end;
      ftWideString:
      begin
        TempWStr := pwidechar(Buffer + Offset);
        SetFieldValue(Fields[Index], TempWStr);
      end;
      ftAutoInc, ftInteger, ftSmallInt:
      begin
        Move((Buffer + Offset)^, TempInt, SizeOf(integer));
        SetFieldValue(Fields[Index], TempInt);
      end;
      ftLargeInt:
      begin
        Move((Buffer + Offset)^, TempLInt, SizeOf(LargeInt));
        SetFieldValue(Fields[Index], TempLInt);
      end;
      ftFloat, ftBCD, ftCurrency:
      begin
        Move((Buffer + Offset)^, TempDouble, SizeOf(double));
        SetFieldValue(Fields[Index], TempDouble);
      end;
      ftDateTime, ftDate, ftTime:
      begin
        Move((Buffer + Offset)^, TempDateTime, SizeOf(TDateTime));
        SetFieldValue(Fields[Index], TempDateTime);
      end;
      ftBoolean:
      begin
        Move((Buffer + Offset)^, TempBool, SizeOf(wordbool));
        SetFieldValue(Fields[Index], BoolToStr(TempBool));
      end;
      ftUnknown:
      begin
        Move((Buffer + Offset)^, TempInt, SizeOf(integer));
        SetFieldValue(Fields[Index], TempInt);
      end;
      else
        if Fields[Index].IsBlob then
        begin
          Move((Buffer + Offset)^, Pointer(Stream), sizeof(Pointer));
          Stream.Position := 0;
          SetBlobField(Fields[Index], Stream);
        end
        else if (Fields[Index].DataType in UNSUPPORTED) then
          raise Exception.Create(
            'TGXBaseDataset.BufferToRecord - ' + FT_NOT_SUPPORTED);
    end;
  end;
end;

procedure TGXBaseDataset.RecordToBuffer(Buffer: TRecordBuffer);
var
  Value: variant;
  TempStr: ansistring;
  TempWStr: WideString;
  TempInt: integer;
  TempLInt: LargeInt;
  TempDouble: double;
  TempBool: wordbool;
  TempDateTime: TDateTime;
  Offset: integer;
  Index: integer;
  Stream: TStream;
  LFieldType: TFieldType;
begin
  with PRecordInfo(Buffer + GetDataSize)^ do
  begin
    BookmarkFlag := bfCurrent;
    RecordID := AllocateRecordID;
    if GetBookMarkSize > 0 then
    begin
      if BookMark = nil then
        GetMem(BookMark, GetBookMarkSize);
      AllocateBookMark(RecordID, BookMark);
    end
    else
      BookMark := nil;
  end;
  DoBeforeGetFieldValue;
  for Index := 0 to FieldCount - 1 do
  begin
    if not Fields[Index].IsBlob then
      Value := GetFieldValue(Fields[Index]);
    Offset := GetFieldOffset(Fields[Index]);
    LFieldType := Fields[Index].DataType;
    case LFieldType of
      ftString:
      begin
        TempStr := Value;
        //if length(TempStr) > Fields[Index].Size then
        //System.Delete(TempStr, Fields[Index].Size, length(TempStr) - Fields[Index].Size);
        StrLCopy(pansichar(Buffer + Offset), pansichar(TempStr), length(TempStr));
        //StrCopy(PAnsiChar(Buffer + Offset), PAnsiChar(TempStr));
      end;
      ftWideString:
      begin
        TempWStr := Value;
            {$IFDEF FPC}
            //if length(TempWStr) > Fields[Index].Size then
              //System.Delete(TempWStr, Fields[Index].Size, length(TempWStr) - Fields[Index].Size);
            StrLCopy(PWideChar(Buffer + Offset), PWideChar(TempWStr), length(TempWStr));
            {$ELSE}
        WStrPLCopy(pwidechar(Buffer + Offset), pwidechar(TempWStr),
          length(TempWStr));
            {$ENDIF}
      end;
      ftAutoInc, ftInteger, ftSmallInt:
      begin
        TempInt := Value;
        Move(TempInt, (Buffer + Offset)^, SizeOf(TempInt));
      end;
      ftLargeInt:
      begin
            {$IFDEF USE_STR_TO_INT64}
            TempLInt := StrToInt64(Value);
            {$ELSE}
        TempLInt := Value;
            {$ENDIF}
        Move(TempLInt, (Buffer + Offset)^, SizeOf(TempLInt));
      end;
      ftFloat, ftBCD, ftCurrency:
      begin
            {$IFDEF USE_STR_TO_FLOAT}
            TempDouble := StrToFloat(Value);
            {$ELSE}
        TempDouble := Value;
            {$ENDIF}
        Move(TempDouble, (Buffer + Offset)^, SizeOf(TempDouble));
      end;
      ftDateTime, ftDate, ftTime:
      begin  // convert Variant (string) to TDateTime and write it to buffer
        try
          TempDateTime := StrToFloat(VarToStr(Value));
        except
          on E: EConvertError do
            TempDateTime := 0;
        end;
        Move(TempDateTime, (Buffer + Offset)^, SizeOf(TempDateTime));
      end;
      ftBoolean:
      begin
        TempBool := StrToBool(Value);
        Move(TempBool, (Buffer + Offset)^, SizeOf(TempBool));
      end;
      ftUnknown:
      begin
        TempInt := -1;
        Move(TempInt, (Buffer + Offset)^, SizeOf(TempInt));
      end
      else
        if Fields[Index].IsBlob then
        begin
          Move((Buffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
          Stream.Size := 0;
          Stream.Position := 0;
          GetBlobField(Fields[Index], Stream);
        end
        else if (Fields[Index].DataType in UNSUPPORTED) then
          raise Exception.Create(
            'TGXBaseDataset.RecordToBuffer - ' + FT_NOT_SUPPORTED);
    end;
  end;
  DoAfterGetFieldValue;
end;

procedure TGXBaseDataset.DoDeleteRecord;
begin
  //Nothing in base class
end;

//Move data from record buffer to field
function TGXBaseDataset.GetFieldData(Field: TField; Buffer: Pointer): boolean;
var
  RecBuffer: TRecordBuffer;
  Offset: integer;
  TempDouble: double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: wordbool;
  TempDateTime: TDateTime;
  TempString: ansistring;
  TempWString: WideString;
  StrL: integer;
begin
  Result := False;
  if not FisOpen then exit;
  RecBuffer := GetActiveRecordBuffer;
  //if RecBuffer = nil then exit;
  {if Buffer = nil then
    begin
    //Dataset checks if field is null by passing a nil buffer
    //Tell it is not null by passing back a result of True
      Result := True;
      exit;
    end; }
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    Inc(RecBuffer, FStartCalculated + Field.Offset);
      {$IFDEF FPC}
      if (RecBuffer[0] = #0) or (Buffer = nil) then
        exit
      else
        Move(RecBuffer[1], Buffer^, Field.DataSize);
      {$ELSE}
    //if (RecBuffer[0] = 0) or (Buffer = nil) then
    Move(RecBuffer, Buffer^, Field.DataSize);
      {$ENDIF}
  end
  else // not fkCalculated or fkLookup
  begin
    Offset := GetFieldOffset(Field);
    case Field.DataType of
      ftAutoInc, ftInteger: Move((RecBuffer + Offset)^, integer(Buffer^), SizeOf(integer));
      ftLargeInt: Move((RecBuffer + Offset)^, LargeInt(Buffer^), SizeOf(LargeInt));
      ftBoolean:
      begin
        Move((RecBuffer + Offset)^, TempBool, SizeOf(wordbool));
        Move(TempBool, wordbool(Buffer^), SizeOf(wordbool));
      end;
      ftString:
      begin
        TempString := pansichar(RecBuffer + Offset);
        StrL := Length(TempString);
        StrLCopy(pansichar(Buffer), pansichar(TempString), StrL);
        //StrCopy(PAnsiChar(Buffer), PAnsiChar(TempString));
      end;
      ftWideString:
      begin
        TempWString := pwidechar(RecBuffer + Offset);
        StrL := Length(TempWString);
          {$IFDEF FPC}
          StrLCopy(PWideChar(Buffer), PWideChar(TempWString), StrL);
          {$ELSE}
        WStrPLCopy(pwidechar(Buffer), pwidechar(TempWString), StrL);
          {$ENDIF}
        //StrCopy(PWideChar(Buffer), PWideChar(TempWString));
      end;
      ftCurrency, ftFloat: Move((RecBuffer + Offset)^, double(Buffer^),
          sizeof(double));
      ftDateTime, ftDate, ftTime:
      begin
        Move((RecBuffer + Offset)^, TempDateTime, SizeOf(TDateTime));
        Data := DateTimeToNative(Field.DataType, TempDateTime);
        Move(Data, Buffer^, sizeof(TDateTimeRec));
      end
      else
        if (not Field.IsBlob) and (Field.DataType in UNSUPPORTED) then
          raise Exception.Create('TGXBaseDataset.GetFieldData - ' +
            FT_NOT_SUPPORTED);
    end;// case
  end;
  Result := True;
end;

{$IFNDEF FPC}
{.$IFDEF VER300}
//For D10 there is this overloaded method which is triggered
//this calls the deprecated method to achieve backwards compatibility
function TGXBaseDataset.GetFieldData(Field: TField; var Buffer: TValueBuffer): boolean;
begin
  Result := GetFieldData(Field, TRecordBuffer(Buffer));
end;

{.$ENDIF VER300}
{$ENDIF !FPC}

//Move data from field to record buffer
procedure TGXBaseDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  Offset: integer;
  RecBuffer: TRecordBuffer;
  TempDouble: double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: wordbool;
  TempDateTime: TDateTime;
  TempString: ansistring;
  TempWString: WideString;
  StrL: integer;
begin
  if not Active then
    exit;
  RecBuffer := GetActiveRecordBuffer;
  if RecBuffer = nil then
    exit;
  if Buffer = nil then
    exit;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    Inc(RecBuffer, FStartCalculated + Field.Offset);
    boolean(RecBuffer[0]) := (Buffer <> nil);
    if boolean(RecBuffer[0]) then
      Move(Buffer^, RecBuffer[1], Field.DataSize);
  end
  else
  begin
    Offset := GetFieldOffset(Field);
    case Field.DataType of
      ftInteger: Move(integer(Buffer^), (RecBuffer + Offset)^, SizeOf(integer));
      ftLargeInt: Move(LargeInt(Buffer^), (RecBuffer + Offset)^, SizeOf(LargeInt));
      ftBoolean:
      begin
        Move(wordbool(Buffer^), TempBool, sizeof(wordbool));
        Move(TempBool, (RecBuffer + Offset)^, sizeof(wordbool));
      end;
      ftString:
      begin
        TempString := pansichar(Buffer);
        StrL := Length(TempString);
        StrLCopy(pansichar(RecBuffer + Offset), pansichar(TempString), StrL);
      end;
      ftWideString:
      begin
        TempWString := pwidechar(Buffer);
        StrL := Length(TempWString);
            {$IFDEF FPC}
            StrLCopy(PWideChar(RecBuffer + Offset), PWideChar(TempWString), StrL);
            {$ELSE}
        WStrPLCopy(pwidechar(RecBuffer + Offset), pwidechar(TempWString), StrL);
            {$ENDIF}
      end;
      ftDateTime, ftDate, ftTime:
      begin
        Data := TDateTimeRec(Buffer^);
        TempDateTime := NativeToDateTime(Field.DataType, Data);
        Move(TempDateTime, (RecBuffer + Offset)^, SizeOf(TDateTime));
      end;
      ftFloat, ftCurrency: Move(double(Buffer^), (RecBuffer + Offset)^,
          SizeOf(double));
      else
        if (not Field.IsBlob) and (Field.DataType in UNSUPPORTED) then
          raise Exception.Create('TGXBaseDataset.SetFieldData - ' +
            FT_NOT_SUPPORTED);
    end; // case
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange,
{$IFDEF FPC}Ptrint(Field){$ELSE}
      nativeint(Field)
{$ENDIF}
      );
end;

{$IFNDEF FPC}
{.$IFDEF VER300}
//For D10 there is this overloaded method which is triggered
//this calls the deprecated method to achieve backwards compatibility
procedure TGXBaseDataset.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  SetFieldData(Field, TRecordBuffer(Buffer));
end;

{.$ENDIF VER300}
{$ENDIF !FPC}

procedure TGXBaseDataset.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if BookMarkSize > 0 then
    AllocateBookMark(PRecordInfo(Buffer + GetDataSize)^.RecordID, Data);
  //     Data := PRecordInfo(Buffer + GetDataSize)^.Bookmark;
end;

function TGXBaseDataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecordInfo(Buffer + GetDataSize)^.BookMarkFlag;
end;

procedure TGXBaseDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if PRecordInfo(Buffer + GetDataSize)^.BookMark = nil then
    GetMem(PRecordInfo(Buffer + GetDataSize)^.BookMark, GetBookMarkSize);
  Move(PRecordInfo(Buffer + GetDataSize)^.BookMark^, Data, GetBookMarkSize);
end;

procedure TGXBaseDataset.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer + GetDataSize)^.BookMarkFlag := Value;
end;

procedure TGXBaseDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  DoGotoBookMark(ABookMark);
end;

function TGXBaseDataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := TGXBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TGXBaseDataset.OpenDataSet;
begin
  BookmarkSize := GetBookMarkSize;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(True);
  FisOpen := True;
  FillBufferMap;
end;

//************************** TGXBlobStream ***************************************

constructor TGXBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FMode := Mode;
  FDataSet := FField.DataSet as TGXBaseDataset;
  if Mode <> bmWrite then
    LoadBlobData;
end;

destructor TGXBlobStream.Destroy;
begin
  if FModified then
    SaveBlobData;
  inherited Destroy;
end;

function TGXBlobStream.Read(var Buffer; Count: longint): longint;
begin
  Result := inherited Read(Buffer, Count);
  FOpened := True;
end;

function TGXBlobStream.Write(const Buffer; Count: longint): longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

procedure TGXBlobStream.LoadBlobData;
var
  Stream: TMemoryStream;
  Offset: integer;
  RecBuffer: TRecordBuffer;
begin
  Self.Size := 0;
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
  begin
    Offset := FDataset.GetFieldOffset(FField);
    Move((RecBuffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
    Self.CopyFrom(Stream, 0);
  end;
  Position := 0;
end;

procedure TGXBlobStream.SaveBlobData;
var
  Stream: TMemoryStream;
  Offset: integer;
  RecBuffer: TRecordBuffer;
begin
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
  begin
    Offset := FDataset.GetFieldOffset(FField);
    Move((RecBuffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
    Stream.Size := 0;
    Stream.CopyFrom(Self, 0);
    Stream.Position := 0;
  end;
  FModified := False;
end;

procedure TGXBaseDataset.SetFiltered(Value: boolean);
begin
  inherited;
  First;
end;

initialization

  //OldTimeSeparator := TimeSeparator;
  //OldDateSeparator := DateSeparator;

  //if TimeSeparator <> ':' then
  //   TimeSeparator := ':';

  //if DateSeparator <> '-' then
  //   DateSeparator := '-';

finalization

  //TimeSeparator := OldTimeSeparator;
  //DateSeparator := OldDateSeparator;

end.
