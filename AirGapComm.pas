unit AirGapComm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;
const
     // messages from read/write threads
     PWM_GOTCOMMDATA = WM_USER + 1;
     PWM_RECEIVEERROR = WM_USER + 2;
     PWM_REQUESTHANGUP = WM_USER + 3;
     PWM_MODEMSTATECHANGE = WM_USER + 4;
     PWM_SENDDATAEMPTY = WM_USER + 5;
    type
    ECommsError = class( Exception );
    TReceiveDataEvent = procedure(Sender: TObject; Buffer: Pointer;
                                  BufferLength: Word) of object;
    TReceiveErrorEvent = procedure(Sender: TObject; EventMask : DWORD) of object;
    TModemStateChangeEvent = procedure(Sender: TObject; ModemEvent : DWORD) of object;
    TSendDataEmptyEvent = procedure(Sender: TObject) of object;

    TDllFunc=function():Byte;stdcall;
    TDllWriteFunc=function(Buffer: Pointer; len:Word):Byte;stdcall;
    TDllReadFunc=function( Buffer: PByte;  len:PWORD):Byte;stdcall;
const
     //
     // Modem Event Constant
     //
     ME_CTS = 1;
     ME_DSR = 2;
     ME_RING = 4;
     ME_RLSD = 8;
type
    TReadThread = class( TThread )
    protected
      procedure Execute; override;
    public
      hCommFile:          THandle;
      hCloseEvent:        THandle;
      hComm32Window:      THandle;

      DelayEvent: THandle; {事件对象的句柄}


      function SetupCommEvent( lpOverlappedCommEvent: POverlapped;
                               var lpfdwEvtMask: DWORD ): Boolean;
      function SetupReadEvent( lpOverlappedRead: POverlapped;
                               lpszInputBuffer: LPSTR; dwSizeofBuffer: DWORD;
                               var lpnNumberOfBytesRead: DWORD ): Boolean;
      function HandleCommEvent( lpOverlappedCommEvent: POverlapped;
                                var lpfdwEvtMask: DWORD; fRetrieveEvent: Boolean ): Boolean;
      function HandleReadEvent( lpOverlappedRead: POverlapped;
                                lpszInputBuffer: LPSTR; dwSizeofBuffer: DWORD;
                                var lpnNumberOfBytesRead: DWORD ): Boolean;
      function HandleReadData( lpszInputBuffer: LPCSTR; dwSizeofBuffer: DWORD ): Boolean;
      function ReceiveData( lpNewString: LPSTR; dwSizeofNewString: DWORD ): BOOL;
      function ReceiveError( EvtMask : DWORD ): BOOL;
      function ModemStateChange( ModemEvent : DWORD ) : BOOL;
      procedure PostHangupCall;
    end;


    TWriteThread = class( TThread )
    protected
      procedure Execute; override;
      function HandleWriteData( pDataToWrite: PChar; dwNumberOfBytesToWrite: DWORD): Boolean;
    public
      hCommFile:          THandle;
      hCloseEvent:        THandle;
      hComm32Window:      THandle;
      pFSendDataEmpty:    ^Boolean;
      procedure PostHangupCall;
    end;


type
  TAirGap_Com = class(TComponent)
  private
    { Private declarations }
      ReadThread:         TReadThread;
      WriteThread:        TWriteThread;
      hCommFile:          THandle;
      hCloseEvent:        THandle;
      FHWnd:              THandle;
      FSendDataEmpty:     Boolean;            // True if send buffer become empty
      
      FOnReceiveData:     TReceiveDataEvent;
      FOnRequestHangup:   TNotifyEvent;
      FOnReceiveError:    TReceiveErrorEvent;
      FOnModemStateChange:TModemStateChangeEvent;
      FOnSendDataEmpty: TSendDataEmptyEvent;




      procedure CommWndProc( var msg: TMessage );
      procedure _SetCommState;
      procedure _SetCommTimeout;

  protected
    { Protected declarations }

      procedure CloseReadThread;
      procedure CloseWriteThread;
      procedure ReceiveData(Buffer: PChar; BufferLength: Word);
      procedure ReceiveError( EvtMask : DWORD );
      procedure ModemStateChange( ModemEvent : DWORD );
      procedure RequestHangup;
      procedure _SendDataEmpty;
  public
    { Public declarations }
     property Handle: THandle read hCommFile;
      property SendDataEmpty : Boolean read FSendDataEmpty;
      constructor Create( AOwner: TComponent ); override;
      destructor Destroy; override;
      procedure StartComm;
      procedure StopComm;
      function WriteCommData( pDataToWrite: PChar; dwSizeofDataToWrite: Word ): Boolean;
      function GetModemState : DWORD;
  published
    { Published declarations }

      property OnReceiveData: TReceiveDataEvent
               read FOnReceiveData write FOnReceiveData;
      property OnReceiveError: TReceiveErrorEvent
               read FOnReceiveError write FOnReceiveError;
      property OnModemStateChange: TModemStateChangeEvent
               read FOnModemStateChange write FOnModemStateChange;
      property OnRequestHangup: TNotifyEvent
               read FOnRequestHangup write FOnRequestHangup;
      property OnSendDataEmpty: TSendDataEmptyEvent
               read FOnSendDataEmpty write FOnSendDataEmpty;
  end;

  var
  TfOpen,TfClose:TDllFunc;
      tfWrite:TDllWriteFunc;
      tfRead:TDllReadFunc;
  TpOpen,TpClose,TpWrte,TpRead:TFarProc;

  const
// This is the message posted to the WriteThread
// When we have something to write.
     PWM_COMMWRITE = WM_USER+1;

// Default size of the Input Buffer used by this code.
//     INPUTBUFFERSIZE = 2048;
       INPUTBUFFERSIZE = 4096;
function Wireless_Open():Byte;cdecl;  external 'USB_RF_DLL_V3.dll';
function Wireless_Close():Byte; cdecl;  external 'USB_RF_DLL_V3.dll';
function Wireless_Send(Buffer: Pointer; len:Word):Byte; cdecl;  external 'USB_RF_DLL_V3.dll';
function Wireless_Receive( Buffer: PByte;  len:PWORD):Byte; cdecl;  external 'USB_RF_DLL_V3.dll';


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AirGap', [TAirGap_Com]);
end;

{ TReadThread }

procedure TReadThread.Execute;
var
   szInputBuffer: array[0..INPUTBUFFERSIZE-1] of Char;
   nNumberOfBytesRead:    DWORD;
   FunResule:Byte;
   lpszPostedBytes: LPSTR;
   lpszInputBuffer: LPCSTR;
begin
       // Keep looping until we break out.
      CloseHandle(DelayEvent); {如果已经创建过}
      DelayEvent := CreateEvent(nil, True, True, nil);
      ResetEvent(DelayEvent);
     while True do
     begin
          if WAIT_TIMEOUT <> WaitForSingleObject(hCloseEvent,0) then
            Exit;

            FunResule:=tfRead(@szInputBuffer,@nNumberOfBytesRead) ;
            if FunResule <> 0 then
            begin
              raise   ECommsError.Create( 'Read error ' ); //ECommsError.Create( 'This serial port already opened' );

            end;
            if nNumberOfBytesRead = 0  then
            begin
               WaitForSingleObject(DelayEvent,500) ;
            end
            else
            begin
              lpszPostedBytes := PAnsiChar( LocalAlloc( LPTR, nNumberOfBytesRead+1 ) );
              lpszInputBuffer:=PAnsiChar(@szInputBuffer);
              if lpszPostedBytes = nil{NULL} then
                      begin
                           // Out of memory

                           PostHangupCall;
                           Exit
                      end;

                     Move( lpszInputBuffer^, lpszPostedBytes^, nNumberOfBytesRead );
                     lpszPostedBytes[nNumberOfBytesRead] := #0;
                   if  not( ReceiveData( lpszPostedBytes, nNumberOfBytesRead )) then
                   Exit;
                   WaitForSingleObject(DelayEvent,200) ;
            end;

     end; {while True}

end;

function TReadThread.HandleCommEvent(lpOverlappedCommEvent: POverlapped;
  var lpfdwEvtMask: DWORD; fRetrieveEvent: Boolean): Boolean;
begin

end;

function TReadThread.HandleReadData(lpszInputBuffer: LPCSTR;
  dwSizeofBuffer: DWORD): Boolean;
begin

end;

function TReadThread.HandleReadEvent(lpOverlappedRead: POverlapped;
  lpszInputBuffer: LPSTR; dwSizeofBuffer: DWORD;
  var lpnNumberOfBytesRead: DWORD): Boolean;
begin

end;

function TReadThread.ModemStateChange(ModemEvent: DWORD): BOOL;
begin

end;

procedure TReadThread.PostHangupCall;
begin
   PostMessage( hComm32Window, PWM_REQUESTHANGUP, 0, 0 )
end;

function TReadThread.ReceiveData(lpNewString: LPSTR;
  dwSizeofNewString: DWORD): BOOL;
begin
  Result := False;

     if not PostMessage( hComm32Window, PWM_GOTCOMMDATA,
                         WPARAM(dwSizeofNewString), LPARAM(lpNewString) ) then
        PostHangupCall
     else
         Result := True
end;

function TReadThread.ReceiveError(EvtMask: DWORD): BOOL;
begin
     Result := False;

     if not PostMessage( hComm32Window, PWM_RECEIVEERROR, 0, LPARAM(EvtMask) ) then
        PostHangupCall
     else
         Result := True
end;

function TReadThread.SetupCommEvent(lpOverlappedCommEvent: POverlapped;
  var lpfdwEvtMask: DWORD): Boolean;
begin

end;

function TReadThread.SetupReadEvent(lpOverlappedRead: POverlapped;
  lpszInputBuffer: LPSTR; dwSizeofBuffer: DWORD;
  var lpnNumberOfBytesRead: DWORD): Boolean;
begin

end;

{ TWriteThread }

procedure TWriteThread.Execute;
var
   msg:   TMsg;
   dwHandleSignaled:      DWORD;
   CompleteOneWriteRequire : Boolean;
   label
     EndWriteThread;
begin
  CompleteOneWriteRequire := True;


  // This is the main loop.  Loop until we break out.
     while True do
     begin
          if not PeekMessage( msg, 0, 0, 0, PM_REMOVE ) then
          begin
               // If there are no messages pending, wait for a message or
               // the CloseEvent.

               pFSendDataEmpty^ := True;

               if CompleteOneWriteRequire then
               begin
                    if not PostMessage( hComm32Window, PWM_SENDDATAEMPTY, 0, 0 ) then
                    begin
                         PostHangupCall;
                         goto EndWriteThread
                    end
               end;

               CompleteOneWriteRequire := False;
          end;

          // Make sure the CloseEvent isn't signaled while retrieving messages.
          if WAIT_TIMEOUT <> WaitForSingleObject(hCloseEvent,0) then
             goto EndWriteThread;

          // Process the message.
          // This could happen if a dialog is created on this thread.
          // This doesn't occur in this sample, but might if modified.
          if msg.hwnd <> 0{NULL} then
          begin
               TranslateMessage(msg);
               DispatchMessage(msg);
               Continue
          end;

          // Handle the message.
          case msg.message of
          PWM_COMMWRITE:  // New string to write to Comm port.
          begin
               // Write the string to the comm port.  HandleWriteData
               // does not return until the whole string has been written,
               // an error occurs or until the CloseEvent is signaled.
               if not HandleWriteData( PChar(msg.lParam), DWORD(msg.wParam) ) then
               begin
                    // If it failed, either we got a signal to end or there
                    // really was a failure.

                    LocalFree( HLOCAL(msg.lParam) );
                    goto EndWriteThread
               end;

               CompleteOneWriteRequire := True;
               // Data was sent in a LocalAlloc()d buffer.  Must free it.
               LocalFree( HLOCAL(msg.lParam) )
          end
          end
     end; {main loop}
     EndWriteThread:
     pFSendDataEmpty^ := True;

end;

function TWriteThread.HandleWriteData( pDataToWrite: PChar; dwNumberOfBytesToWrite: DWORD): Boolean;
var
   dwLastError,

   dwNumberOfBytesWritten,
   dwWhereToStartWriting,

   dwHandleSignaled:       DWORD;
   HandlesToWaitFor: array[0..1] of THandle;
   FuncResule:Byte;
begin
     Result := False;

     dwNumberOfBytesWritten := 0;
     dwWhereToStartWriting := 0; // Start at the beginning.

     HandlesToWaitFor[0] := hCloseEvent;


     // Keep looping until all characters have been written.
     repeat
           // Start the overlapped I/O.
           FuncResule:=tfWrite(@pDataToWrite[ dwWhereToStartWriting ],dwNumberOfBytesToWrite) ;
           if FuncResule <> 1 then
           begin
              dwNumberOfBytesWritten:=  dwNumberOfBytesToWrite;
              Result := True;
           end;
           // Some data was written.  Make sure it all got written.

           Dec( dwNumberOfBytesToWrite, dwNumberOfBytesWritten );
           Inc( dwWhereToStartWriting, dwNumberOfBytesWritten )
     until (dwNumberOfBytesToWrite <= 0);  // Write the whole thing!

     // Wrote the whole string.


end;

procedure TWriteThread.PostHangupCall;
begin
    PostMessage( hComm32Window, PWM_REQUESTHANGUP, 0, 0 )
end;

{ TAirGap_Com }

procedure TAirGap_Com.CloseReadThread;
begin
  if ReadThread <> nil then
     begin
          // Signal the event to close the worker threads.
          SetEvent( hCloseEvent );

          // Wait 10 seconds for it to exit.  Shouldn't happen.
          if (WaitForSingleObject(ReadThread.Handle, 10000) = WAIT_TIMEOUT) then
             ReadThread.Terminate;
          ReadThread.Free;
          ReadThread := nil
     end
end;

procedure TAirGap_Com.CloseWriteThread;
begin
   // If it exists...
     if WriteThread <> nil then
     begin
          // Signal the event to close the worker threads.
          SetEvent(hCloseEvent);

          FSendDataEmpty := True;

          // Wait 10 seconds for it to exit.  Shouldn't happen.
          if WaitForSingleObject( WriteThread.Handle, 10000 ) = WAIT_TIMEOUT then
             WriteThread.Terminate;
          WriteThread.Free;
          WriteThread := nil
     end
end;

procedure TAirGap_Com.CommWndProc(var msg: TMessage);
begin
   case msg.msg of
          PWM_GOTCOMMDATA:
          begin
               ReceiveData( PChar(msg.LParam), msg.WParam );
               LocalFree( msg.LParam )
          end;
          PWM_RECEIVEERROR:    ReceiveError( msg.LParam );
          PWM_MODEMSTATECHANGE:ModemStateChange( msg.LParam );
          PWM_REQUESTHANGUP:   RequestHangup;
          PWM_SENDDATAEMPTY:   _SendDataEmpty
     end
end;

constructor TAirGap_Com.Create(AOwner: TComponent);
begin
    inherited Create( AOwner );
    ReadThread := nil;
    WriteThread := nil;
    hCommFile := 0;
    hCloseEvent := 0;
    FSendDataEmpty := True;
    

//    FReadIntervalTimeout         := 100;
//    FReadTotalTimeoutMultiplier  := 0;
//    FReadTotalTimeoutConstant    := 0;
//    FWriteTotalTimeoutMultiplier := 0;
//    FWriteTotalTimeoutConstant   := 0;

     if not (csDesigning in ComponentState) then
        FHWnd := AllocateHWnd(CommWndProc)
end;

destructor TAirGap_Com.Destroy;
begin
if not (csDesigning in ComponentState) then
        DeallocateHWnd(FHwnd);

     inherited Destroy;
end;

function TAirGap_Com.GetModemState: DWORD;
begin

end;

procedure TAirGap_Com.ModemStateChange(ModemEvent: DWORD);
begin
   if Assigned(FOnModemStateChange) then
        FOnModemStateChange( self, ModemEvent )
end;

procedure TAirGap_Com.ReceiveData(Buffer: PChar; BufferLength: Word);
begin
   if Assigned(FOnReceiveData) then
        FOnReceiveData( self, Buffer, BufferLength )
end;

procedure TAirGap_Com.ReceiveError(EvtMask: DWORD);
begin
   if Assigned(FOnReceiveError) then
        FOnReceiveError( self, EvtMask )
end;

procedure TAirGap_Com.RequestHangup;
begin
    if Assigned(FOnRequestHangup) then
        FOnRequestHangup( Self )
end;


procedure TAirGap_Com.StartComm;
var
FunReturn:Byte;
begin
   if (hCommFile <> 0)then
        raise  ECommsError.Create( 'The port is already open' ); //ECommsError.Create( 'This serial port already opened' );
  hCommFile:=LoadLibrary('USB_RF_DLL_V3.dll'); {装载DLL}
  if hCommFile<=0 then
  raise  ECommsError.Create( 'load dll error' ); //ECommsError.Create( 'This serial port already opened' );


     TpOpen:=GetProcAddress(hCommFile,PChar('Wireless_Open'));
    if not( TpOpen<>nil ) then
    begin
         raise ECommsError.Create( 'open com error'+#13#10+
                                  'Open func error' );
    end ;


    TpClose:=GetProcAddress(hCommFile,PChar('Wireless_Close'));
    if not(TpClose<>nil) then
    begin
     raise ECommsError.Create( 'open com error'+#13#10+
                                  'Close func error' );
    end;


    TpWrte:=GetProcAddress(hCommFile,PChar('Wireless_Send'));
    if not(TpWrte<>nil) then
    begin
     raise ECommsError.Create( 'open com error'+#13#10+
                                  'Send func error' );

    end;
    TpRead:=GetProcAddress(hCommFile,PChar('Wireless_Receive'));
    if  not(TpRead<>nil) then
    begin
     raise ECommsError.Create( 'open com error'+#13#10+
                                  'Receive func error' );
    end;


    TfOpen:=TDllFunc(TpOpen);
    TfClose:=TDllFunc(TpClose);
    tfWrite:=TDllWriteFunc(TpWrte);
    tfRead:=TDllReadFunc(TpRead);
    FunReturn:=  TfOpen();
    if FunReturn <> 0 then
     raise ECommsError.Create( 'open com error'+#13#10+
                                  'Open com error' );

    FSendDataEmpty := True;
    // Create the event that will signal the threads to close.
    hCloseEvent := CreateEvent( nil, True, False, nil );
    if hCloseEvent = 0 then
     begin
          CloseHandle( hCommFile );
          hCommFile := 0;
          raise ECommsError.Create( 'can not create event' )//ECommsError.Create( 'Unable to create event' )
     end;

     // Create the Read thread.
     try
        ReadThread := TReadThread.Create( True {suspended} );
     except
           ReadThread := nil;
           CloseHandle( hCloseEvent );
           CloseHandle( hCommFile );
           hCommFile := 0;
           raise ECommsError.Create( 'can not create Read Thread' )//ECommsError.Create( 'Unable to create read thread' )
     end;
     ReadThread.hCommFile := hCommFile;
     ReadThread.hCloseEvent := hCloseEvent;
     ReadThread.hComm32Window := FHWnd;

     // Comm threads should have a higher base priority than the UI thread.
     // If they don't, then any temporary priority boost the UI thread gains
     // could cause the COMM threads to loose data.
     ReadThread.Priority := tpHighest;

     // Create the Write thread.
     try
        WriteThread := TWriteThread.Create( True {suspended} );
     except
           CloseReadThread;
           WriteThread := nil;
           CloseHandle( hCloseEvent );
           CloseHandle( hCommFile );
           hCommFile := 0;
           raise ECommsError.Create( '无法创建写线程' )//ECommsError.Create( 'Unable to create write thread' )
     end;
     WriteThread.hCommFile := hCommFile;
     WriteThread.hCloseEvent := hCloseEvent;
     WriteThread.hComm32Window := FHWnd;
     WriteThread.pFSendDataEmpty := @FSendDataEmpty;

     WriteThread.Priority := tpHigher;

     ReadThread.Resume;
     WriteThread.Resume;
     // Everything was created ok.  Ready to go!

end;

procedure TAirGap_Com.StopComm;
var
FunReturn: Byte;
begin
    // No need to continue if we're not communicating.
     if hCommFile = 0 then
        Exit;

     // Close the threads.
     CloseReadThread;
     CloseWriteThread;

     // Not needed anymore.
     CloseHandle( hCloseEvent );
     FunReturn:= TfClose();
     if FunReturn <> 0 then
     raise ECommsError.Create( 'close com error');
     // Now close the comm port handle.
     FreeLibrary(hCommFile);
     hCommFile := 0
end;

function TAirGap_Com.WriteCommData(pDataToWrite: PChar;
  dwSizeofDataToWrite: Word): Boolean;
var
   Buffer: Pointer;
begin
     if (WriteThread <> nil) and (dwSizeofDataToWrite <> 0) then
     begin
          Buffer := Pointer(LocalAlloc( LPTR, dwSizeofDataToWrite+1 ));
          Move( pDataToWrite^, Buffer^, dwSizeofDataToWrite );
          if PostThreadMessage( WriteThread.ThreadID, PWM_COMMWRITE,
                                WPARAM(dwSizeofDataToWrite), LPARAM(Buffer) ) then
          begin
               FSendDataEmpty := False;
               Result := True;
               Exit
          end
     end;

     Result := False
end;

procedure TAirGap_Com._SendDataEmpty;
begin
   if Assigned(FOnSendDataEmpty) then
        FOnSendDataEmpty( self )
end;

procedure TAirGap_Com._SetCommState;
begin

end;

procedure TAirGap_Com._SetCommTimeout;
begin

end;

end.
