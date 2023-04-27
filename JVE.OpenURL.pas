(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.OpenURL;

interface

{$DEFINE FULL_VERSION}

uses {$IF Defined(MSWINDOWS)} Winapi.Windows, {$ENDIF} System.Classes,
  {$IF CompilerVersion >= 27} System.Messaging, {$ELSE} FMX.Messages,{$ENDIF}
  System.SysUtils, FMX.Consts, JVE.Messaging, JVE.Utils,
  {$IF Defined(FULL_VERSION)} JVE.Actions, {$ENDIF}
  {$IF CompilerVersion >= 26} FMX.Graphics {$ENDIF}, System.StrUtils;

{$IF not Defined(FULL_VERSION)}
// Analytics-limited component suite contains this file, but does not contain
// the JVE.Actions.pas, thus these defines allow the file to compile.
type
  TJVEAction = class end;
  IJVEExecutable = interface end;
{$ENDIF}

type
  [ComponentPlatformsAttribute($000B945F)]
  TJVEOpenURL = class(TComponent, IJVEExecutable)
  private
    FURL: String;
  protected
    // IJVEExecutable support
    procedure Open(Sender: TJVEAction); overload;
  public
    // Opens the URL.
    function Open: Boolean; overload;
    // If can NOT, Open will definitely fail. But true value does not guarantee success.
    // The Sender parameter is for IJVEExecutable support and should not be used.
    function CanOpen(Sender: TJVEAction = nil): Boolean;

    // These are helpers: as above, but class functions
    class function OpenURL(const URL: String): Boolean; overload;
    class function CanOpenURL(const URL: String): Boolean; overload;

    // Checks whether the platforms supports specified URL scheme
    class function SchemeSupported(Scheme: String): Boolean;
    // This function makes strings safe for URL parameters, for example
    // "Check %" becomes "Check+%25".
    class function URLEncode(const Value: String): String; static;

    // Downloads the content of the provided URL. Only tested with HTTP and HTTPS.
    // Optionally additional headers may be provided. An error message could be
    // optionally returned. If an error occurs, returns nil.
    // This function downloads the file completely into a memory stream. As such
    // it should not be used on a potentially large files. It also blocks the calling
    // thread until it finished, so calling it on a secondary thread is recommended.
    class function DownloadURL(URL: String; Headers: TStrings; PostData: TMemoryStream; var Error: String): TMemoryStream; overload; static;
    class function DownloadURL(URL: String; Headers: TStrings; var Error: String): TMemoryStream; overload; static;
    class function DownloadURL(URL: String; Headers: TStrings = nil): TMemoryStream; overload; inline; static;
    class function DownloadURL(URL: String; var Error: String): TMemoryStream; overload; inline; static;

    // Downloads the content of the provided URL (same as above), returning a string.
    class function DownloadString(URL: String; Headers: TStrings; PostData: String; var Error: String): String; overload; static;
    class function DownloadString(URL: String; Headers: TStrings; var Error: String): String; overload; static;
    class function DownloadString(URL: String; Headers: TStrings = nil): String; overload; inline; static;
    class function DownloadString(URL: String; var Error: String): String; overload; inline; static;

    // The following functions are convenience wrappers to download a picture.
    // The download is thread-blocking, it should thus be normally executed
    // in a secondary thread (notice, don't use the bitmap in a secondary thread).
    // The result of these calls is an object, which should be freed after use.
    class function DownloadBitmap(URL: String; Headers: TStrings; var Error: String): TBitmap; overload; static;
    class function DownloadBitmap(URL: String; Headers: TStrings = nil): TBitmap; overload; inline; static;
    class function DownloadBitmap(URL: String; var Error: String): TBitmap; overload; inline; static;
  published
    property URL: String read FURL write FURL;
  end;

  TBitmapHelper = class helper for TBitmap
    // The following functions are convenience wrappers to download a picture.
    // The download is thread-blocking, it should thus be normally executed
    // in a secondary thread.
    procedure Download(URL: String; Headers: TStrings; var Error: String); overload;
    procedure Download(URL: String; Headers: TStrings = nil); overload; inline;
    procedure Download(URL: String; var Error: String); overload; inline;
  end;

implementation

uses
  {$IF Defined(IOS)} Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CocoaTypes;
  {$ELSEIF Defined(MACOS)} Macapi.ObjectiveC, Macapi.AppKit,
  Macapi.Foundation, Macapi.CocoaTypes;
  {$ELSEIF Defined(ANDROID)} AndroidApi.JNI, AndroidApi.JNIBridge,
  AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.App,
  AndroidApi.JNI.JavaTypes, AndroidApi.JNI.Net, FMX.Surfaces,
    {$IF CompilerVersion >= 27} AndroidApi.Helpers,{$ENDIF} FMX.Helpers.Android, FMX.Types;
  {$ELSEIF Defined(MSWINDOWS)} Winapi.ShellAPI, System.Win.Registry, System.Math; {$ENDIF}

{$REGION 'Native Libraries Import Code'}
{$IFDEF MSWINDOWS}
const
  // WinHTTP.dll
  winhttpdll = 'winhttp.dll';

  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NO_PROXY      = 1;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY   = 3;

  WINHTTP_NO_PROXY_NAME             = Nil;
  WINHTTP_NO_PROXY_BYPASS           = Nil;

  WINHTTP_NO_REFERER                = Nil;
  WINHTTP_DEFAULT_ACCEPT_TYPES      = Nil;

  WINHTTP_NO_ADDITIONAL_HEADERS     = Nil;
  WINHTTP_NO_REQUEST_DATA           = Nil;

  WINHTTP_NO_CLIENT_CERT_CONTEXT    = Nil;

  WINHTTP_ADDREQ_INDEX_MASK         = $0000FFFF;
  WINHTTP_ADDREQ_FLAGS_MASK         = $FFFF0000;
  WINHTTP_ADDREQ_FLAG_ADD_IF_NEW    = $10000000;
  WINHTTP_ADDREQ_FLAG_ADD           = $20000000;
  WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA     = $40000000;
  WINHTTP_ADDREQ_FLAG_COALESCE_WITH_SEMICOLON = $01000000;
  WINHTTP_ADDREQ_FLAG_COALESCE      = WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA;
  WINHTTP_ADDREQ_FLAG_REPLACE       = $80000000;

  // flags for WinHttpOpenRequest()
  WINHTTP_FLAG_BYPASS_PROXY_CACHE   = $00000100;  // add "pragma: no-cache" request header
  WINHTTP_FLAG_REFRESH              = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
  WINHTTP_FLAG_SECURE               = $00800000;  // use SSL if applicable (HTTPS)
  WINHTTP_QUERY_FLAG_NUMBER         = $20000000;
  // flag for WinHttpOpen()
  WINHTTP_FLAG_ASYNC                = $10000000;  // this session is asynchronous (where supported)

  // flags for WinHttpCrackUrl() and WinHttpCreateUrl()
  ICU_ESCAPE                        = $80000000;  // (un)escape URL characters
  ICU_NO_ENCODE                     = $20000000;  // Don't convert unsafe characters to escape sequence
  ICU_DECODE                        = $10000000;  // Convert %XX escape sequences to characters
  ICU_NO_META                       = $08000000;  // Don't convert .. etc. meta path sequences
  ICU_ENCODE_SPACES_ONLY            = $04000000;  // Encode spaces only
  ICU_BROWSER_MODE                  = $02000000;  // Special encode/decode rules for browser
  ICU_ENCODE_PERCENT                = $00001000;  // Encode any percent (ASCII25)

  // maximum field lengths (from WinInet)
  WINHTTP_MAX_HOST_NAME_LENGTH = 256;
  {WINHTTP_MAX_USER_NAME_LENGTH = 128;
  WINHTTP_MAX_PASSWORD_LENGTH = 128;}
  WINHTTP_MAX_PATH_LENGTH = 2048;
  WINHTTP_MAX_SCHEME_LENGTH = 32;      // longest protocol name length
  WINHTTP_MAX_URL_LENGTH = (WINHTTP_MAX_SCHEME_LENGTH + Length('://') + 1 + WINHTTP_MAX_PATH_LENGTH);

  // security flags
  SECURITY_FLAG_IGNORE_UNKNOWN_CA           = $00000100;
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID    = $00002000;  // expired X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID      = $00001000;  // bad common name in X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE     = $00000200;

  WINHTTP_OPTION_SECURITY_FLAGS             = 31;
  WINHTTP_OPTION_CLIENT_CERT_CONTEXT        = 47;
  WINHTTP_OPTION_CLIENT_CERT_ISSUER_LIST    = 94;
  WINHTTP_OPTION_SECURITY_CERTIFICATE_STRUCT= 32;

  WINHTTP_DEFAULT_PORT       = 0;      // use the protocol-specific default
  WINHTTP_DEFAULT_HTTP_PORT  = 80;     //    "     "  HTTP   "
  WINHTTP_DEFAULT_HTTPS_PORT = 443;    //    "     "  HTTPS  "
//----------------------------------------------------------------------
  // WinHTTP error codes
  WINHTTP_ERROR_BASE                    = 12000;
  ERROR_WINHTTP_OUT_OF_HANDLES          = WINHTTP_ERROR_BASE + 1;
  ERROR_WINHTTP_TIMEOUT                 = WINHTTP_ERROR_BASE + 2;
  ERROR_WINHTTP_INTERNAL_ERROR          = WINHTTP_ERROR_BASE + 4;
  ERROR_WINHTTP_INVALID_URL             = WINHTTP_ERROR_BASE + 5;
  ERROR_WINHTTP_UNRECOGNIZED_SCHEME     = WINHTTP_ERROR_BASE + 6;
  ERROR_WINHTTP_NAME_NOT_RESOLVED       = WINHTTP_ERROR_BASE + 7;
  ERROR_WINHTTP_INVALID_OPTION          = WINHTTP_ERROR_BASE + 9;
  ERROR_WINHTTP_OPTION_NOT_SETTABLE     = WINHTTP_ERROR_BASE + 11;
  ERROR_WINHTTP_SHUTDOWN                = WINHTTP_ERROR_BASE + 12;

  ERROR_WINHTTP_LOGIN_FAILURE           = WINHTTP_ERROR_BASE + 15;
  ERROR_WINHTTP_OPERATION_CANCELLED     = WINHTTP_ERROR_BASE + 17;
  ERROR_WINHTTP_INCORRECT_HANDLE_TYPE   = WINHTTP_ERROR_BASE + 18;
  ERROR_WINHTTP_INCORRECT_HANDLE_STATE  = WINHTTP_ERROR_BASE + 19;
  ERROR_WINHTTP_CANNOT_CONNECT          = WINHTTP_ERROR_BASE + 29;
  ERROR_WINHTTP_CONNECTION_ERROR        = WINHTTP_ERROR_BASE + 30;
  ERROR_WINHTTP_RESEND_REQUEST          = WINHTTP_ERROR_BASE + 32;

  ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = WINHTTP_ERROR_BASE + 44;

 // WinHttpRequest Component errors
  ERROR_WINHTTP_CANNOT_CALL_BEFORE_OPEN = WINHTTP_ERROR_BASE + 100;
  ERROR_WINHTTP_CANNOT_CALL_BEFORE_SEND = WINHTTP_ERROR_BASE + 101;
  ERROR_WINHTTP_CANNOT_CALL_AFTER_SEND  = WINHTTP_ERROR_BASE + 102;
  ERROR_WINHTTP_CANNOT_CALL_AFTER_OPEN  = WINHTTP_ERROR_BASE + 103;

  // HTTP API errors
  ERROR_WINHTTP_HEADER_NOT_FOUND        = WINHTTP_ERROR_BASE + 150;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE = WINHTTP_ERROR_BASE + 152;
  ERROR_WINHTTP_INVALID_QUERY_REQUEST   = WINHTTP_ERROR_BASE + 154;
  ERROR_WINHTTP_HEADER_ALREADY_EXISTS   = WINHTTP_ERROR_BASE + 155;
  ERROR_WINHTTP_REDIRECT_FAILED         = WINHTTP_ERROR_BASE + 156;

  // additional WinHttp API error codes
  ERROR_WINHTTP_AUTO_PROXY_SERVICE_ERROR  = WINHTTP_ERROR_BASE + 178;
  ERROR_WINHTTP_BAD_AUTO_PROXY_SCRIPT     = WINHTTP_ERROR_BASE + 166;
  ERROR_WINHTTP_UNABLE_TO_DOWNLOAD_SCRIPT = WINHTTP_ERROR_BASE + 167;

  ERROR_WINHTTP_NOT_INITIALIZED           = WINHTTP_ERROR_BASE + 172;
  ERROR_WINHTTP_SECURE_FAILURE            = WINHTTP_ERROR_BASE + 175;

  ERROR_WINHTTP_SECURE_CERT_DATE_INVALID  = WINHTTP_ERROR_BASE + 37;
  ERROR_WINHTTP_SECURE_CERT_CN_INVALID    = WINHTTP_ERROR_BASE + 38;
  ERROR_WINHTTP_SECURE_INVALID_CA         = WINHTTP_ERROR_BASE + 45;
  ERROR_WINHTTP_SECURE_CERT_REV_FAILED    = WINHTTP_ERROR_BASE + 57;
  ERROR_WINHTTP_SECURE_CHANNEL_ERROR      = WINHTTP_ERROR_BASE + 157;
  ERROR_WINHTTP_SECURE_INVALID_CERT       = WINHTTP_ERROR_BASE + 169;
  ERROR_WINHTTP_SECURE_CERT_REVOKED       = WINHTTP_ERROR_BASE + 170;
  ERROR_WINHTTP_SECURE_CERT_WRONG_USAGE   = WINHTTP_ERROR_BASE + 179;

  ERROR_WINHTTP_AUTODETECTION_FAILED      = WINHTTP_ERROR_BASE + 180;
  ERROR_WINHTTP_HEADER_COUNT_EXCEEDED     = WINHTTP_ERROR_BASE + 181;
  ERROR_WINHTTP_HEADER_SIZE_OVERFLOW      = WINHTTP_ERROR_BASE + 182;
  ERROR_WINHTTP_CHUNKED_ENCODING_HEADER_SIZE_OVERFLOW
                                          = WINHTTP_ERROR_BASE + 183;
  ERROR_WINHTTP_RESPONSE_DRAIN_OVERFLOW   = WINHTTP_ERROR_BASE + 184;

  WINHTTP_ERROR_LAST = WINHTTP_ERROR_BASE + 184;

  // constant strings for WinHTTP specific error messages, used with WinHttpSysErrorMessage.
  // not localized
  err_12001 = 'Out of handles.';
  err_12002 = 'Time out.';
  err_12004 = 'Internal error.';
  err_12005 = 'Invalid URL.';
  err_12006 = 'Unrecognized Scheme.';
  err_12007 = 'Name not resolved.';
  err_12009 = 'Invalid option.';
  err_12011 = 'Option not settable.';
  err_12012 = 'Shutdown.';
  err_12015 = 'Login failure.';
  err_12017 = 'Operation cancelled.';
  err_12018 = 'Incorrect handle type.';
  err_12019 = 'Incorrect handle state.';
  err_12029 = 'Can not connect.';
  err_12030 = 'Connection error.';
  err_12032 = 'Resend request.';
  err_12044 = 'Client auth cert needed.';
  err_12100 = 'Can not call before open.';
  err_12101 = 'Can not call before send.';
  err_12102 = 'Can not call after send.';
  err_12103 = 'Can not call after open.';
  err_12150 = 'Header not found.';
  err_12152 = 'Invalid Server Response.';
  err_12154 = 'Invalid query request.';
  err_12155 = 'Header already exists.';
  err_12156 = 'Redirect failed.';
  err_12178 = 'Auto proxy service error.';
  err_12166 = 'Bad auto proxy script.';
  err_12167 = 'Unable to download Script.';
  err_12172 = 'Not initialized.';
  err_12175 = 'Secure Failure.';
  err_12037 = 'Secure Cert Date Invalid.';
  err_12038 = 'Secure Cert CN Invalid.';
  err_12045 = 'Secure Invalid CA.';
  err_12057 = 'Secure Cert Rev Failed.';
  err_12157 = 'Secure Channel Error.';
  err_12169 = 'Secure Invalid Cert.';
  err_12170 = 'Secure Cert Revoked.';
  err_12179 = 'Secure Cert Wrong Usage.';
  err_12180 = 'Auto Detection Failed.';
  err_12181 = 'Header Count Exceeded.';
  err_12182 = 'Header Size Overflow.';
  err_12183 = 'Chunked Encoding Header Size Overflow.';
  err_12184 = 'Response Drain Overflow.';
//-----------------------------------------------------------------------------

  // HTTP Response Status Codes:
  HTTP_STATUS_CONTINUE =  100; // OK to continue with request
  HTTP_STATUS_SWITCH_PROTOCOLS = 101; // server has switched protocols in upgrade header

  HTTP_STATUS_OK = 200; // request completed
  HTTP_STATUS_CREATED = 201; // object created, reason = new URI
  HTTP_STATUS_ACCEPTED = 202; // async completion (TBS)
  HTTP_STATUS_PARTIAL = 203; // partial completion
  HTTP_STATUS_NO_CONTENT = 204; // no info to return
  HTTP_STATUS_RESET_CONTENT = 205; // request completed, but clear form
  HTTP_STATUS_PARTIAL_CONTENT = 206; // partial GET fulfilled
  HTTP_STATUS_WEBDAV_MULTI_STATUS = 207; // WebDAV Multi-Status

  HTTP_STATUS_AMBIGUOUS = 300; // server couldn't decide what to return
  HTTP_STATUS_MOVED = 301; // object permanently moved
  HTTP_STATUS_REDIRECT = 302; // object temporarily moved
  HTTP_STATUS_REDIRECT_METHOD = 303; // redirection w/ new access method
  HTTP_STATUS_NOT_MODIFIED = 304; // if-modified-since was not modified
  HTTP_STATUS_USE_PROXY = 305; // redirection to proxy, location header specifies proxy to use
  HTTP_STATUS_REDIRECT_KEEP_VERB = 307; // HTTP/1.1: keep same verb

  HTTP_STATUS_BAD_REQUEST = 400; // invalid syntax
  HTTP_STATUS_DENIED = 401; // access denied
  HTTP_STATUS_PAYMENT_REQ = 402; // payment required
  HTTP_STATUS_FORBIDDEN = 403; // request forbidden
  HTTP_STATUS_NOT_FOUND = 404; // object not found
  HTTP_STATUS_BAD_METHOD = 405; // method is not allowed
  HTTP_STATUS_NONE_ACCEPTABLE = 406; // no response acceptable to client found
  HTTP_STATUS_PROXY_AUTH_REQ = 407; // proxy authentication required
  HTTP_STATUS_REQUEST_TIMEOUT = 408; // server timed out waiting for request
  HTTP_STATUS_CONFLICT = 409; // user should resubmit with more info
  HTTP_STATUS_GONE = 410; // the resource is no longer available
  HTTP_STATUS_LENGTH_REQUIRED = 411; // the server refused to accept request w/o a length
  HTTP_STATUS_PRECOND_FAILED = 412; // precondition given in request failed
  HTTP_STATUS_REQUEST_TOO_LARGE = 413; // request entity was too large
  HTTP_STATUS_URI_TOO_LONG = 414; // request URI too long
  HTTP_STATUS_UNSUPPORTED_MEDIA = 415; // unsupported media type
  HTTP_STATUS_RETRY_WITH = 449; // retry after doing the appropriate action.

  HTTP_STATUS_SERVER_ERROR = 500; // internal server error
  HTTP_STATUS_NOT_SUPPORTED = 501; // required not supported
  HTTP_STATUS_BAD_GATEWAY = 502; // error response received from gateway
  HTTP_STATUS_SERVICE_UNAVAIL = 503; // temporarily overloaded
  HTTP_STATUS_GATEWAY_TIMEOUT = 504; // timed out waiting for gateway
  HTTP_STATUS_VERSION_NOT_SUP = 505; // HTTP version not supported

  HTTP_STATUS_FIRST = HTTP_STATUS_CONTINUE;
  HTTP_STATUS_LAST = HTTP_STATUS_VERSION_NOT_SUP;

  WINHTTP_OPTION_SECURE_PROTOCOLS = 84;
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL2 = $00000008;
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 = $00000020;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1 = $00000080;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;
  WINHTTP_FLAG_SECURE_PROTOCOL_ALL = WINHTTP_FLAG_SECURE_PROTOCOL_SSL2 or
                                             WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 or
                                             WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;

//-----------------------------------------------------------------------------

type
  HInternet = Pointer;
  INTERNET_SCHEME = (INTERNET_SCHEME_FILLER, INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS);
  INTERNET_PORT = Word;
  URL_COMPONENTS = record
    dwStructSize: DWord;      // size of this structure. Used in version check
    lpszScheme: LPWSTR;       // pointer to scheme name
    dwSchemeLength: DWord;    // length of scheme name
    nScheme: INTERNET_SCHEME; // enumerated scheme type (if known)
    lpszHostName: LPWSTR;     // pointer to host name
    dwHostNameLength: DWord;  // length of host name
    nPort: INTERNET_PORT;     // converted port number
    lpszUserName: LPWSTR;     // pointer to user name
    dwUserNameLength: DWord;  // length of user name
    lpszPassword: LPWSTR;     // pointer to password
    dwPasswordLength: DWord;  // length of password
    lpszUrlPath: LPWSTR;      // pointer to URL-path
    dwUrlPathLength: DWord;   // length of URL-path
    lpszExtraInfo: LPWSTR;    // pointer to extra information (e.g. ?foo or #foo)
    dwExtraInfoLength: DWord; // length of extra information
  end;
  LPURL_COMPONENTS = ^URL_COMPONENTS;

  function WinHttpOpen(pwszUserAgent: PWideChar; dwAccessType: DWORD; pwszProxyName,
    pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall; external winhttpdll;
  function WinHttpConnect(hSession: HINTERNET; pswzServerName: PWideChar;
    nServerPort: INTERNET_PORT; dwReserved: DWORD): HINTERNET; stdcall; external winhttpdll;
  function WinHttpOpenRequest(hConnect: HINTERNET; pwszVerb: PWideChar;
    pwszObjectName: PWideChar; pwszVersion: PWideChar; pwszReferer: PWideChar;
    ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall; external winhttpdll;
  function WinHttpCloseHandle(hInternet: HINTERNET): BOOL; stdcall; external winhttpdll;
  function WinHttpAddRequestHeaders(hRequest: HINTERNET; pwszHeaders: PWideChar; dwHeadersLength: DWORD;
    dwModifiers: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpSendRequest(hRequest: HINTERNET; pwszHeaders: PWideChar;
    dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD; dwTotalLength: DWORD;
    dwContext: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpReceiveResponse(hRequest: HINTERNET; lpReserved: Pointer): BOOL; stdcall; external winhttpdll;
  function WinHttpQueryHeaders(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: PWideChar;
    lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpReadData(hRequest: HINTERNET; lpBuffer: Pointer; dwNumberOfBytesToRead: DWORD;
    var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpQueryDataAvailable(hRequest: HINTERNET; var lpdwNumberOfBytesAvailable: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpSetOption(hInternet: HINTERNET; dwOption: DWORD; lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpQueryOption(hInternet: HINTERNET; dwOption: DWORD; var lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpWriteData(hRequest: HINTERNET; lpBuffer: Pointer; dwNumberOfBytesToWrite: DWORD;
    var lpdwNumberOfBytesWritten: DWORD): BOOL; stdcall; external winhttpdll;
  function WinHttpCheckPlatform(): BOOL; stdcall; external winhttpdll;
  //function WinHttpCrackUrl(pwszUrl: LPCWSTR; dwUrlLength, dwFlags: DWORD; lpUrlComponents: LPURL_COMPONENTS): BOOL; stdcall; external winhttpdll;
  function WinHttpCrackUrl(pwszURL: LPCWSTR; dwUrlLength, dwFlags: DWord;
      var URLComponents: URL_COMPONENTS): BOOL; stdcall; external winhttpdll;

//   SysErrorMessage does not return word values for the WinHTTP errors. This will.
// SysErrorMessage is still called because WinHTTP will return *SOME* standard
// Windows API errors.
// Implementation by Glenn9999 from http://www.tek-tips.com/faqs.cfm?fid=7493
function WinHttpSysErrorMessage(ErrorCode: Cardinal): String;
begin
  if (ErrorCode >= WINHTTP_ERROR_BASE) and (ErrorCode <= WINHTTP_ERROR_LAST) then
    case ErrorCode of
      ERROR_WINHTTP_OUT_OF_HANDLES:           Result := err_12001;
      ERROR_WINHTTP_TIMEOUT:                  Result := err_12002;
      ERROR_WINHTTP_INTERNAL_ERROR:           Result := err_12004;
      ERROR_WINHTTP_INVALID_URL:              Result := err_12005;
      ERROR_WINHTTP_UNRECOGNIZED_SCHEME:      Result := err_12006;
      ERROR_WINHTTP_NAME_NOT_RESOLVED:        Result := err_12007;
      ERROR_WINHTTP_INVALID_OPTION:           Result := err_12009;
      ERROR_WINHTTP_OPTION_NOT_SETTABLE:      Result := err_12011;
      ERROR_WINHTTP_SHUTDOWN:                 Result := err_12012;
      ERROR_WINHTTP_LOGIN_FAILURE:            Result := err_12015;
      ERROR_WINHTTP_OPERATION_CANCELLED:      Result := err_12017;
      ERROR_WINHTTP_INCORRECT_HANDLE_TYPE:    Result := err_12018;
      ERROR_WINHTTP_INCORRECT_HANDLE_STATE:   Result := err_12019;
      ERROR_WINHTTP_CANNOT_CONNECT:           Result := err_12029;
      ERROR_WINHTTP_CONNECTION_ERROR:         Result := err_12030;
      ERROR_WINHTTP_RESEND_REQUEST:           Result := err_12032;
      ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED:  Result := err_12044;
      ERROR_WINHTTP_CANNOT_CALL_BEFORE_OPEN:  Result := err_12100;
      ERROR_WINHTTP_CANNOT_CALL_BEFORE_SEND:  Result := err_12101;
      ERROR_WINHTTP_CANNOT_CALL_AFTER_SEND:   Result := err_12102;
      ERROR_WINHTTP_CANNOT_CALL_AFTER_OPEN:   Result := err_12103;
      ERROR_WINHTTP_HEADER_NOT_FOUND:         Result := err_12150;
      ERROR_WINHTTP_INVALID_SERVER_RESPONSE:  Result := err_12152;
      ERROR_WINHTTP_INVALID_QUERY_REQUEST:    Result := err_12154;
      ERROR_WINHTTP_HEADER_ALREADY_EXISTS:    Result := err_12155;
      ERROR_WINHTTP_REDIRECT_FAILED:          Result := err_12156;
      ERROR_WINHTTP_AUTO_PROXY_SERVICE_ERROR: Result := err_12178;
      ERROR_WINHTTP_BAD_AUTO_PROXY_SCRIPT:    Result := err_12166;
      ERROR_WINHTTP_UNABLE_TO_DOWNLOAD_SCRIPT: Result := err_12167;
      ERROR_WINHTTP_NOT_INITIALIZED:          Result := err_12172;
      ERROR_WINHTTP_SECURE_FAILURE:           Result := err_12175;
      ERROR_WINHTTP_SECURE_CERT_DATE_INVALID: Result := err_12037;
      ERROR_WINHTTP_SECURE_CERT_CN_INVALID:   Result := err_12038;
      ERROR_WINHTTP_SECURE_INVALID_CA:        Result := err_12045;
      ERROR_WINHTTP_SECURE_CERT_REV_FAILED:   Result := err_12057;
      ERROR_WINHTTP_SECURE_CHANNEL_ERROR:     Result := err_12157;
      ERROR_WINHTTP_SECURE_INVALID_CERT:      Result := err_12169;
      ERROR_WINHTTP_SECURE_CERT_REVOKED:      Result := err_12170;
      ERROR_WINHTTP_SECURE_CERT_WRONG_USAGE:  Result := err_12179;
      ERROR_WINHTTP_AUTODETECTION_FAILED:     Result := err_12180;
      ERROR_WINHTTP_HEADER_COUNT_EXCEEDED:    Result := err_12181;
      ERROR_WINHTTP_HEADER_SIZE_OVERFLOW:     Result := err_12182;
      ERROR_WINHTTP_CHUNKED_ENCODING_HEADER_SIZE_OVERFLOW:
                                              Result := err_12183;
      ERROR_WINHTTP_RESPONSE_DRAIN_OVERFLOW:  Result := err_12184;
    else
      Result := 'Unspecified WinHTTP error.';
    end
  else
    Result := SysErrorMessage(ErrorCode);
end;
{$ENDIF}

{$IFDEF ANDROID}
type
  JURLConnection      = interface;  // java.net.URLConnection
  JHttpURLConnection  = interface;  // java.net.HttpURLConnection
  JURL                = interface;  // java.net.URL

  JURLConnectionClass = interface(JObjectClass)
  ['{5E9817B4-2DBD-48D9-B702-8E039503AEAE}']
  end;

  [JavaSignature('java/net/URLConnection')]
  JURLConnection = interface(JObject)
  ['{2ECDC807-25F0-4FE3-A1D6-4B1EBB90B6AE}']
    {Methods}
    procedure connect; cdecl;
    procedure setAllowUserInteraction(newValue: Boolean); cdecl;
    procedure setDoInput(newValue: Boolean); cdecl;
    procedure setDoOutput(newValue: Boolean); cdecl;
    procedure setConnectTimeout(timeoutMillis: Integer); cdecl;
    procedure setReadTimeout(timeoutMillis: Integer); cdecl;
    procedure setRequestProperty(key, value: JString); cdecl;
    procedure addRequestProperty(field: JString; newValue: JString); cdecl;
    function getInputStream: JInputStream; cdecl;
    function getOutputStream: JOutputStream; cdecl;
    function getContentEncoding: JString; cdecl;
    function getHeaderField(name: JString): JString; cdecl;
  end;
  TJURLConnection = class(TJavaGenericImport<JURLConnectionClass, JURLConnection>) end;

  JHttpURLConnectionClass = interface(JURLConnectionClass)
  ['{5850F622-ED4F-415E-B3A5-FABBEC857E14}']
    {Property Methods}
    function _GetHTTP_ACCEPTED: Integer;
    function _GetHTTP_BAD_GATEWAY: Integer;
    function _GetHTTP_BAD_METHOD: Integer;
    function _GetHTTP_BAD_REQUEST: Integer;
    function _GetHTTP_CLIENT_TIMEOUT: Integer;
    function _GetHTTP_CONFLICT: Integer;
    function _GetHTTP_CREATED: Integer;
    function _GetHTTP_ENTITY_TOO_LARGE: Integer;
    function _GetHTTP_FORBIDDEN: Integer;
    function _GetHTTP_GATEWAY_TIMEOUT: Integer;
    function _GetHTTP_GONE: Integer;
    function _GetHTTP_INTERNAL_ERROR: Integer;
    function _GetHTTP_LENGTH_REQUIRED: Integer;
    function _GetHTTP_MOVED_PERM: Integer;
    function _GetHTTP_MOVED_TEMP: Integer;
    function _GetHTTP_MULT_CHOICE: Integer;
    function _GetHTTP_NO_CONTENT: Integer;
    function _GetHTTP_NOT_ACCEPTABLE: Integer;
    function _GetHTTP_NOT_AUTHORITATIVE: Integer;
    function _GetHTTP_NOT_FOUND: Integer;
    function _GetHTTP_NOT_IMPLEMENTED: Integer;
    function _GetHTTP_NOT_MODIFIED: Integer;
    function _GetHTTP_OK: Integer;
    function _GetHTTP_PARTIAL: Integer;
    function _GetHTTP_PAYMENT_REQUIRED: Integer;
    function _GetHTTP_PRECON_FAILED: Integer;
    function _GetHTTP_PROXY_AUTH: Integer;
    function _GetHTTP_REQ_TOO_LONG: Integer;
    function _GetHTTP_RESET: Integer;
    function _GetHTTP_SEE_OTHER: Integer;
    function _GetHTTP_SERVER_ERROR: Integer;
    function _GetHTTP_USE_PROXY: Integer;
    function _GetHTTP_UNAUTHORIZED: Integer;
    function _GetHTTP_UNSUPPORTED_TYPE: Integer;
    function _GetHTTP_UNAVAILABLE: Integer;
    function _GetHTTP_VERSION: Integer;

    {Read-only Properties}
    // 2XX: generally "OK"
    // 3XX: relocation/redirect
    // 4XX: client error
    // 5XX: server error

    //Numeric status code, 202: Accepted
    property HTTP_ACCEPTED: Integer read _GetHTTP_ACCEPTED;
    //Numeric status code, 502: Bad Gateway
    property HTTP_BAD_GATEWAY: Integer read _GetHTTP_BAD_GATEWAY;
    // Numeric  status code, 405: Bad Method
    property HTTP_BAD_METHOD: Integer read _GetHTTP_BAD_METHOD;
    // Numeric  status code, 400: Bad Request
    property HTTP_BAD_REQUEST: Integer read _GetHTTP_BAD_REQUEST;
    // Numeric  status code, 408: Client Timeout
    property HTTP_CLIENT_TIMEOUT: Integer read _GetHTTP_CLIENT_TIMEOUT;
    // Numeric  status code, 409: Conflict
    property HTTP_CONFLICT: Integer read _GetHTTP_CONFLICT;
    // Numeric  status code, 201: Created
    property HTTP_CREATED: Integer read _GetHTTP_CREATED;
    // Numeric  status code, 413: Entity too large
    property HTTP_ENTITY_TOO_LARGE: Integer read _GetHTTP_ENTITY_TOO_LARGE;
    // Numeric  status code, 403: Forbidden
    property HTTP_FORBIDDEN: Integer read _GetHTTP_FORBIDDEN;
    // Numeric  status code, 504: Gateway timeout
    property HTTP_GATEWAY_TIMEOUT: Integer read _GetHTTP_GATEWAY_TIMEOUT;
    // Numeric  status code, 410: Gone
    property HTTP_GONE: Integer read _GetHTTP_GONE;
    // Numeric  status code, 500: Internal error
    property HTTP_INTERNAL_ERROR: Integer read _GetHTTP_INTERNAL_ERROR;
    // Numeric  status code, 411: Length required
    property HTTP_LENGTH_REQUIRED: Integer read _GetHTTP_LENGTH_REQUIRED;
    // Numeric  status code, 301 Moved permanently
    property HTTP_MOVED_PERM: Integer read _GetHTTP_MOVED_PERM;
    // Numeric  status code, 302: Moved temporarily
    property HTTP_MOVED_TEMP: Integer read _GetHTTP_MOVED_TEMP;
    // Numeric  status code, 300: Multiple choices
    property HTTP_MULT_CHOICE: Integer read _GetHTTP_MULT_CHOICE;
    // Numeric  status code, 204: No content
    property HTTP_NO_CONTENT: Integer read _GetHTTP_NO_CONTENT;
    // Numeric  status code, 406: Not acceptable
    property HTTP_NOT_ACCEPTABLE: Integer read _GetHTTP_NOT_ACCEPTABLE;
    // Numeric  status code, 203: Not authoritative
    property HTTP_NOT_AUTHORITATIVE: Integer read _GetHTTP_NOT_AUTHORITATIVE;
    // Numeric  status code, 404: Not found
    property HTTP_NOT_FOUND: Integer read _GetHTTP_NOT_FOUND;
    // Numeric  status code, 501: Not implemented
    property HTTP_NOT_IMPLEMENTED: Integer read _GetHTTP_NOT_IMPLEMENTED;
    // Numeric  status code, 304: Not modified
    property HTTP_NOT_MODIFIED: Integer read _GetHTTP_NOT_MODIFIED;
    // Numeric  status code, 200: OK
    property HTTP_OK: Integer read _GetHTTP_OK;
    // Numeric  status code, 206: Partial
    property HTTP_PARTIAL: Integer read _GetHTTP_PARTIAL;
    // Numeric  status code, 402: Payment required
    property HTTP_PAYMENT_REQUIRED: Integer read _GetHTTP_PAYMENT_REQUIRED;
    // Numeric  status code, 412: Precondition failed
    property HTTP_PRECON_FAILED: Integer read _GetHTTP_PRECON_FAILED;
    // Numeric  status code, 407: Proxy authentication required
    property HTTP_PROXY_AUTH: Integer read _GetHTTP_PROXY_AUTH;
    // Numeric  status code, 414: Request too long
    property HTTP_REQ_TOO_LONG: Integer read _GetHTTP_REQ_TOO_LONG;
    // Numeric  status code, 205: Reset
    property HTTP_RESET: Integer read _GetHTTP_RESET;
    // Numeric  status code, 303: See other
    property HTTP_SEE_OTHER: Integer read _GetHTTP_SEE_OTHER;
    // Numeric  status code, 500: Internal error
    // @deprecated Use {@link #HTTP_INTERNAL_ERROR} instead.
    property HTTP_SERVER_ERROR: Integer read _GetHTTP_SERVER_ERROR;
    // Numeric  status code, 305: Use proxy.
    //
    // <p>Like Firefox and Chrome, this class doesn't honor this response code.
    // Other implementations respond to this status code by retrying the request
    // using the HTTP proxy named by the response's Location header field.
    property HTTP_USE_PROXY: Integer read _GetHTTP_USE_PROXY;
    // Numeric  status code, 401: Unauthorized
    property HTTP_UNAUTHORIZED: Integer read _GetHTTP_UNAUTHORIZED;
    // Numeric  status code, 415: Unsupported type
    property HTTP_UNSUPPORTED_TYPE: Integer read _GetHTTP_UNSUPPORTED_TYPE;
    // Numeric  status code, 503: Unavailable
    property HTTP_UNAVAILABLE: Integer read _GetHTTP_UNAVAILABLE;
    // Numeric status code, 505: Version not supported
    property HTTP_VERSION: Integer read _GetHTTP_VERSION;
  end;

  [JavaSignature('java/net/HttpURLConnection')]
  JHttpURLConnection = interface(JURLConnection)
  ['{6FF4A4B0-A79E-4852-8938-30C61E279F52}']
    {Methods}
    procedure disconnect; cdecl;
    procedure setRequestMethod(method: JString); cdecl;
    function getResponseCode: Integer; cdecl;
    function getResponseMessage: JString; cdecl;
    function getRequestMethod(): JString; cdecl;
    function getErrorStream: JInputStream; cdecl;
  end;
  TJHttpURLConnection = class(TJavaGenericImport<JHttpURLConnectionClass, JHttpURLConnection>) end;

  JURLClass = interface(JObjectClass)
  ['{246FE844-A842-44F5-B0A3-43A8C5ED8091}']
      {Constructors}
    function init(spec: JString): JURL; cdecl; overload;
    function init(context: JURL; spec: JString): JURL; cdecl; overload;
  //function init(context: JURL; spec: JString; handler: JURLStreamHandler): JURL; cdecl; overload;
  end;

  [JavaSignature('java/net/URL')]
  JURL = interface(JObject)
  ['{1C4C1873-65AE-4722-8EEF-36BBF423C9C5}']
    {Methods}
    function openConnection: JURLConnection; cdecl;
  end;
  TJURL = class(TJavaGenericImport<JURLClass, JURL>) end;
{$ENDIF}
{$ENDREGION}

{ TJVEOpenURL }

function TJVEOpenURL.CanOpen(Sender: TJVEAction = nil): Boolean;
begin
  Result := CanOpenURL(FURL);
end;

function TJVEOpenURL.Open: Boolean;
begin
  Result := OpenURL(FURL);
end;

class function TJVEOpenURL.CanOpenURL(const URL: String): Boolean;
{$IFDEF MSWINDOWS}
var
  Scheme: String;
  Index: Integer;
{$ENDIF}
begin
{$IF Defined(IOS)}
  Result := SharedUIApplication.canOpenURL(
    TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(URL))));
{$ELSEIF Defined(MACOS)}
  Result := Assigned(TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace).
    URLForApplicationToOpenURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(URL)))));
{$ELSEIF Defined(ANDROID)}
  Result := (TJIntent.Create.setAction(TJIntent.JavaClass.ACTION_VIEW).setData(
    StrToJURI(URL)).resolveActivity(GetSharedActivity.getPackageManager) <> nil)
{$ELSEIF Defined(MSWINDOWS)}
  if URL = '' then
    Exit(False);
  // Honestly trying to make 0-based and 1-based strings compatible.
  // Whose bright idea was it to make future strings 0-based?!
  if not (AnsiChar(URL[Low(URL)]) in ['a'..'z', 'A'..'Z']) then
    Exit(True); // No scheme, yet we don't want to report an error here
  Scheme := URL[Low(URL)];

  for Index := Low(URL) + 1 to High(URL) do
    if URL[Index] = ':' then
      Exit((Length(Scheme) = 1) or SchemeSupported(Scheme))
    else if AnsiChar(URL[Index]) in ['a'..'z', 'A'..'Z', '0'..'9', '+', '-', '.'] then
      Scheme := Scheme + URL[Index]
    else
      Exit(True); // No scheme
  Result := True;
{$ENDIF}
end;

class function TJVEOpenURL.SchemeSupported(Scheme: String): Boolean;
{$IF Defined(MACOS) OR Defined(ANDROID)}
begin
  Result := CanOpenURL(Scheme + '://test');
{$ELSEIF Defined(MSWINDOWS)}
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    Exit(Registry.OpenKeyReadOnly(Scheme) and Registry.KeyExists('URL Protocol'));
  finally
    FreeAndNil(Registry);
  end;
{$ENDIF}
end;

procedure TJVEOpenURL.Open(Sender: TJVEAction);
begin
  Open;
end;

class function TJVEOpenURL.OpenURL(const URL: String): Boolean;
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVETrackEvent.Create('Browser', 'External', URL, 0, acJVEOpenURL));
{$IF Defined(IOS)}
  Result := SharedUIApplication.openURL(
    TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(URL))));
{$ELSEIF Defined(MACOS)}
  Result := TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace).
    openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(URL))));
{$ELSEIF Defined(ANDROID)}
  try
    GetSharedActivity.startActivity(TJIntent.Create.setAction(
      TJIntent.JavaClass.ACTION_VIEW).setData(StrToJURI(URL)));
    Result := True;
  except
    on e: Exception do
      Result := False
  end;
{$ELSEIF Defined(MSWINDOWS)}
  Result := ShellExecute(0, 'OPEN', PChar(URL), '', '', SW_SHOWNORMAL) > 32;
{$ENDIF}
end;

class function TJVEOpenURL.URLEncode(Const Value: String): String;
var
  Ch: Byte;
begin
  Result := '';
  for Ch in TEncoding.UTF8.GetBytes(Value) do
    if Ch in [Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_'),
      Ord('0')..Ord('9'), Ord('*'), Ord('-'), Ord('.')] then
        Result := Result + Char(Ch)
      else
        Result := Result + '%' + IntToHex(Ord(Ch), 2);
end;

{$IFDEF MACOS}
type
  // Redeclared due to a bug in sendSynchronousRequest function's official definition
  // in some versions (2nd param there is by value, rather than by reference).
  NSURLConnectionClass = interface(NSObjectClass)
    function sendSynchronousRequest(request: NSURLRequest;
      returningResponse: PPointer; error: PPointer): NSData; cdecl;
  end;
  NSURLConnection = interface(NSObject) end;
  TNSURLConnection = class(TOCGenericImport<NSURLConnectionClass, NSURLConnection>)  end;
{$ENDIF}

// Downloads URL content and returns it in TMemoryStream, accepts Custom Headers (optional).
// Uses GET method (unless PostData is specified). Returns Nil with a localized error description in Error if error occured.
//
// URL must be in a canonicalized form;
// Headers shouldn't contain a CR/LF code;
// Note: in Windows implementation not all errors are localized; specifically WinHttp errors are returned in English.
//
// We could always use Indy, but we only need 1 function and we need SSL.
// It's much easier to use a native call than to solve the linking issue.
class function TJVEOpenURL.DownloadURL(URL: String; Headers: TStrings; PostData: TMemoryStream; var Error: String): TMemoryStream;
var
{$IF Defined(MACOS)}
  URLRequest: NSMutableURLRequest;
  Response: Pointer;
  ErrorPtr: Pointer;
  Data: NSData;
{$ELSEIF Defined(ANDROID)}
  Connection: JHttpURLConnection;
  Input: JInputStream;
  JByteBuffer: TJavaArray<Byte>;
  Size: Integer;
{$ELSEIF Defined(MSWINDOWS)}
  dwSize, dwDownloaded, dwOptions: DWORD;
  bResults: BOOL;
  hSession, hConnect, hRequest: HINTERNET;
  ByteBuffer: TBytes;
  Host, UrlPath: String;
  UrlComponents: URL_COMPONENTS;  // a structure that would contain the
                                  // individual components of the URL
  Header: String;
{$ENDIF}
  I: Integer;
const
  BYTE_BUFFER_SIZE = 512;
begin
{$IF Defined(MACOS)}
  Result := nil;
  Response := nil;
  ErrorPtr := nil;
  URLRequest := TNSMutableURLRequest.Wrap(TNSMutableURLRequest.Alloc.
    initWithURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(URL)))));
  URLRequest.setHTTPShouldHandleCookies(True);

  if PostData <> nil then
  begin
    URLRequest.setHTTPMethod(ToNSSTR('POST'));
    URLRequest.setHTTPBody(TNSData.Wrap(TNSData.OCClass.dataWithBytes(PostData.Memory, PostData.Size)));
  end;

  if Assigned(Headers) then
    for I := 0 to Headers.Count - 1 do
      URLRequest.addValue(ToNSSTR(Headers.ValueFromIndex[I]), ToNSSTR(Headers.Names[I]));

  Data := TNSURLConnection.OCClass.sendSynchronousRequest(URLRequest, @Response, @ErrorPtr);

  if ErrorPtr = nil then
  begin
    I := TNSHTTPURLResponse.Wrap(Response).statusCode;
    if I = 200 then
    begin
      Result := TMemoryStream.Create;
      if (Data <> nil) and (Data.length <> 0) then
        Result.Write(Data.bytes^, Data.length);
    end else
      Error := FromNSSTR(TNSHTTPURLResponse.OCClass.localizedStringForStatusCode(I))
  end else
    Error := FromNSSTR(TNSError.Wrap(ErrorPtr).localizedDescription);
{$ELSEIF Defined(ANDROID)}
  try
    Connection := TJHttpURLConnection.Wrap((TJURL.JavaClass.init(
      StringToJString(URL)).openConnection as ILocalObject).GetObjectID);
    Connection.setAllowUserInteraction(False);
    Connection.setDoOutput(PostData <> nil);

    if Assigned(Headers) then
      for I := 0 to Headers.Count - 1 do
        Connection.addRequestProperty(StringToJString(Headers.Names[I]),
          StringToJString(Headers.ValueFromIndex[I]));

    if PostData <> nil then
    begin
      Connection.setRequestMethod(StringToJString('POST'));

      JByteBuffer := TJavaArray<Byte>.Create(PostData.Size);
      for I := 0 to PostData.Size - 1 do
        JByteBuffer.Items[I] := PByteArray(PostData.Memory)[I];
      Connection.getOutputStream.write(JByteBuffer);
      FreeAndNil(JByteBuffer);
    end else
    begin
      Connection.connect;
      if Connection.getResponseCode <> TJHttpURLConnection.JavaClass.HTTP_OK then
      begin
        Error := JStringToString(Connection.getResponseMessage);
        Exit(nil);
      end;
    end;

    Input := Connection.getInputStream;
    Error := '';

    // initialize buffers
    Result := TMemoryStream.Create;
    Result.Position := 0;
    JByteBuffer := TJavaArray<Byte>.Create(BYTE_BUFFER_SIZE);

    // retrieve data
    repeat
      Size := Input.read(JByteBuffer, 0, BYTE_BUFFER_SIZE);
      // Note: Java bytes are signed and Delphi bytes are not.
      // However, we don't have to worry about that here. Interesting.
      if Size > 0 then
        Result.WriteData(JByteBuffer.Data, Size);
    until Size <= 0;
  except on e: Exception do
    begin
      Error := e.Message;
      Result := nil;
    end;
  end;

  if Assigned(Input) then
    try
      Input.close;
    except
    end;
  if Assigned(Connection) then
    Connection.disconnect;

{$ELSEIF Defined(MSWINDOWS)}
  dwSize := 0;
  dwDownloaded := 0;
  hSession := nil;
  hConnect := nil;
  hRequest := nil;
  Error := String.Empty;
  Result := nil;

  // Split up the URL

  // Initialize the URL_COMPONENTS structure
  ZeroMemory(@UrlComponents, SizeOf(UrlComponents));
  with UrlComponents do
  begin
    dwStructSize := SizeOf(UrlComponents);
    // tell InternetCrackUrl() that we are only interested in
    // the host and the path, and to use separate buffers for them
    dwHostNameLength := WINHTTP_MAX_HOST_NAME_LENGTH;
    SetLength(Host, dwHostNameLength);
    lpszHostName := PChar(Host);  // point lpszHostName to a Host buffer

    dwUrlPathLength := WINHTTP_MAX_PATH_LENGTH;
    SetLength(UrlPath, dwUrlPathLength);
    lpszUrlPath := PChar(UrlPath);  // point lpszUrlPath to an UrlPath buffer
  end;

  // Crack the URL
  bResults := WinHttpCrackUrl(PChar(URL), Min(WINHTTP_MAX_URL_LENGTH, URL.Length),
    0, UrlComponents);

  if bResults then
  begin
    // Trim garbage
    SetLength(UrlPath, UrlComponents.dwUrlPathLength);
    SetLength(Host, UrlComponents.dwHostNameLength);

    // Obtain a session handle
    hSession := WinHttpOpen('WinHTTP User Agent /1.0', WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
      WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  end;

  // Specify a HTTP Server
  if bResults and (hSession <> nil) then
  begin
    hConnect := WinHttpConnect(hSession, PChar(Host), UrlComponents.nPort, 0);
    dwOptions := WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1 or
      WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
    WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
      @dwOptions, sizeof(dwOptions));
  end;

  // Create a HTTP request handle
  if hConnect <> nil then
    hRequest := WinHttpOpenRequest(hConnect, PWideChar(IfThen(PostData = nil, 'GET', 'POST')),
      PChar(UrlPath), nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES,
      IfThen(UrlComponents.nScheme = INTERNET_SCHEME_HTTPS, WINHTTP_FLAG_SECURE, 0));

  if hRequest <> nil then
  begin
    // Add headers
    if Assigned(Headers) then
    begin
      if Headers.Count > 0 then
        Header := Headers.Names[0] + ': ' + Headers.ValueFromIndex[0];
      for I := 1 to Headers.Count - 1 do
        Header := Header + sLineBreak + Headers.Names[I] + ': ' + Headers.ValueFromIndex[I];

      bResults := WinHttpAddRequestHeaders(hRequest, PWideChar(WideString(Header)),
        Length(Header), WINHTTP_ADDREQ_FLAG_ADD);
    end;

    // Send a request
    if bResults then
      if PostData = nil then
        bResults := WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0)
      else
        bResults := WinHttpSendRequest(hRequest, nil, 0,
          PostData.Memory, PostData.Size, PostData.size, 0);

    if (not bResults) and (GetLastError = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then
    begin
      // Say "I don't have a certificate. Yet let me to connect."
      if (not bResults) then
        bResults := WinHttpSetOption(hRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0);

      // Now resend the request
      if bResults then
        bResults := WinHttpSendRequest(hRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0, WINHTTP_NO_REQUEST_DATA, 0, 0, 0);
    end;

    // End the request
    if bResults then
      bResults := WinHttpReceiveResponse(hRequest, nil);

    if bResults then
    begin
      // Init buffers
      Result := TMemoryStream.Create;
      Result.Position := 0;
      ByteBuffer := TBytes.Create();

      // Keep checking for data until there is nothing left
      repeat
        dwSize := 0;
        if not WinHttpQueryDataAvailable(hRequest, &dwSize) then
        begin // Error!
          bResults := False;
          Break;
        end;

        // Allocate space for the buffer
        SetLength(ByteBuffer, dwSize);

        if not Assigned(ByteBuffer) and (dwSize > 0) then
        begin
          Error := 'Out of memory error.';
          dwSize := 0;
          FreeAndNil(Result);
        end else
        begin
          // Read the data
          if not WinHttpReadData(hRequest, ByteBuffer, dwSize, &dwDownloaded) then
          begin // Error
            bResults := False;
            Break;
          end else
            Result.WriteData(ByteBuffer, dwDownloaded);  // Ok
        end;
      until dwSize <= 0;
      if bResults then
        SetLength(ByteBuffer, 0);
    end;
  end;

  // Report any errors
  if not bResults then
  begin
    Error := WinHttpSysErrorMessage(GetLastError);
    FreeAndNil(Result);
  end;

  // Close open handles
  if hRequest <> Nil then
    WinHttpCloseHandle(hRequest);
  if hConnect <> Nil then
    WinHttpCloseHandle(hConnect);
  if hSession <> Nil then
    WinHttpCloseHandle(hSession);
{$ENDIF}
  if Result <> nil then
    Result.Seek(0, TSeekOrigin.soBeginning);
end;

class function TJVEOpenURL.DownloadURL(URL: String; Headers: TStrings; var Error: String): TMemoryStream;
begin
  Result := DownloadURL(URL, Headers, nil, Error);
end;

class function TJVEOpenURL.DownloadURL(URL: String; Headers: TStrings = nil): TMemoryStream;
var
  Ignore: String;
begin
  Result := DownloadURL(URL, Headers, nil, Ignore);
end;

class function TJVEOpenURL.DownloadURL(URL: String; var Error: String): TMemoryStream;
begin
  Result := DownloadURL(URL, nil, nil, Error);
end;

class function TJVEOpenURL.DownloadString(URL: String; Headers: TStrings; PostData: String; var Error: String): String;
var
  Stream: TStream;
  Buffer: TBytes;
  Encoding: TEncoding;
  BOMLength: Integer;
  Post: TMemoryStream;
begin
  if PostData <> '' then
  begin
    Post := TMemoryStream.Create;
    Buffer := TEncoding.UTF8.GetBytes(PostData);
    Post.Write(Buffer, Length(Buffer));
    Post.Seek(0, soBeginning);
  end else
    Post := nil;

  Stream := DownloadURL(URL, Headers, Post, Error);
  FreeAndNil(Post);
  if Stream = nil then
    Exit('');

  try
    SetLength(Buffer, Stream.Size);
    Stream.ReadBuffer(Buffer, Stream.Size);

    Encoding := nil;
    BOMLength := TEncoding.GetBufferEncoding(Buffer, Encoding);
    Result := Encoding.GetString(Buffer, BOMLength, Length(Buffer) - BOMLength);
  finally
    FreeAndNil(Stream);
  end;
end;

class function TJVEOpenURL.DownloadString(URL: String; Headers: TStrings; var Error: String): String;
begin
  Result := DownloadString(URL, Headers, '', Error);
end;

class function TJVEOpenURL.DownloadString(URL: String; Headers: TStrings = nil): String;
var
  Ignore: String;
begin
  Result := DownloadString(URL, Headers, '', Ignore);
end;

class function TJVEOpenURL.DownloadString(URL: String; var Error: String): String;
begin
  Result := DownloadString(URL, nil, '', Error);
end;

// This is a copy from FMX.Graphics.Android.pas, but including a bug fix.
{$IFDEF ANDROID}
function IsGIFStream(const Stream: TStream): Boolean;
const
  IDCharCount = 3;
var
  PrevPosition: Int64;
  Builder: TStringBuilder;
  I: Integer;
  Value: Char;
begin
  if (Stream = nil) or (Stream.Size < IDCharCount) then
    Exit(False);

Value := #0; // <- This is the bug fix in this function
  PrevPosition := Stream.Position;
  try
    Builder := TStringBuilder.Create(IDCharCount);
    try
      for I := 0 to 2 do
      begin
        Stream.ReadBuffer(Value, 1);
        Builder.Append(Value);
      end;

      Result := SameText(Builder.ToString, 'GIF');
    finally
      FreeAndNil(Builder);
    end;
  finally
    Stream.Position := PrevPosition;
  end;
end;
{$ENDIF}

function DecodeImage(var Error: String; Stream: TMemoryStream): TBitmap;
{$IFDEF ANDROID}
var
  Surf: TBitmapSurface;
  TempArray: TJavaArray<Byte>;
  Movie: JMovie;
  Bitmap: JBitmap;
  Canvas: JCanvas;
{$ENDIF}
begin
  try
    Result := TBitmap.Create;
// Standard implementation does not correctly recognize GIF files, which fail as a result.
{$IFDEF ANDROID}
    if IsGIFStream(Stream) then
    begin
      TempArray := TJavaArray<Byte>.Create(Stream.Size - Stream.Position);
      Stream.ReadBuffer(TempArray.Data^, TempArray.Length);

      Movie := TJMovie.JavaClass.decodeByteArray(TempArray, 0, TempArray.Length);

      Surf := TBitmapSurface.Create;
      try
        Bitmap := TJBitmap.JavaClass.createBitmap(Movie.width, Movie.height, TJBitmap_Config.JavaClass.ARGB_8888);
        try
          Canvas := TJCanvas.JavaClass.init(Bitmap);
          try
            Movie.setTime(0);
            Movie.draw(Canvas, 0, 0);
          finally
            Canvas := nil;
          end;

          JBitmapToSurface(Bitmap, Surf);
        finally
          Bitmap.recycle;
        end;
        Result.Assign(Surf);
      finally
        FreeAndNil(Surf);
      end;
    end else
{$ENDIF}
    Result.LoadFromStream(Stream);
  except
    Error := SBitmapLoadingFailed;
    FreeAndNil(Result);
  end;
end;

class function TJVEOpenURL.DownloadBitmap(URL: String; Headers: TStrings; var Error: String): TBitmap;
var
  Stream: TMemoryStream;
  Output: TBitmap;
  OutError: String;
begin
  Stream := DownloadURL(URL, Headers, Error);
  if Error <> '' then
    Exit(nil);
  if (Stream = nil) or (Stream.Size = 0) then
  begin
    FreeAndNil(Stream);
    Error := SInvalidStreamFormat;
    Exit(nil);
  end;

  OutError := '';
  // We need to synchronize because of a bug in the Delphi's bitmap factory
  // which forces us to create a bitmap within the context of the main thread.
  if TThread.CurrentThread.ThreadID = MainThreadID then
    Output := DecodeImage(OutError, Stream)
  else
    TThread.Synchronize(TThread.CurrentThread, procedure
    begin
      Output := DecodeImage(OutError, Stream);
    end);

  FreeAndNil(Stream);
  Error := OutError;
  Result := Output;
end;

class function TJVEOpenURL.DownloadBitmap(URL: String; Headers: TStrings = nil): TBitmap;
var
  Ignore: String;
begin
  Result := DownloadBitmap(URL, Headers, Ignore);
end;

class function TJVEOpenURL.DownloadBitmap(URL: String; var Error: String): TBitmap;
begin
  Result := DownloadBitmap(URL, nil, Error);
end;

{ TBitmapHelper }

procedure TBitmapHelper.Download(URL: String; Headers: TStrings; var Error: String);
var
  Stream: TMemoryStream;
  Output: TBitmap;
  OutError: String;
begin
  Stream := TJVEOpenURL.DownloadURL(URL, Headers, Error);
  if Error <> '' then
    Exit;
  if (Stream = nil) or (Stream.Size = 0) then
  begin
    FreeAndNil(Stream);
    Error := SInvalidStreamFormat;
    Exit;
  end;

  OutError := '';
  // We need to synchronize because of a bug in the Delphi's bitmap factory
  // which forces us to create a bitmap within the context of the main thread.
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    Output := DecodeImage(OutError, Stream);
    Assign(Output);
    FreeAndNil(Output);
  end else
    TThread.Synchronize(TThread.CurrentThread, procedure
    begin
      Output := DecodeImage(OutError, Stream);
      Assign(Output);
      FreeAndNil(Output);
    end);

  FreeAndNil(Stream);
  Error := OutError;
end;

procedure TBitmapHelper.Download(URL: String; Headers: TStrings = nil);
var
  Ignore: String;
begin
  Download(URL, Headers, Ignore);
end;

procedure TBitmapHelper.Download(URL: String; var Error: String);
begin
  Download(URL, nil, Error);
end;

end.


