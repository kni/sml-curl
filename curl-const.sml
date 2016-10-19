signature CURL_CONST =
sig
  (* perl -ne 'print "  $1: int\n" if /(val\s+\w+\s+)=/' curl-const.sml *)
  val CURLE_OK : int
  val CURL_GLOBAL_ALL : int
  val CURLE_FAILED_INIT     : int
  val CURLE_COULDNT_CONNECT : int
  val CURLOPT_URL             : int
  val CURLOPT_FOLLOWLOCATION  : int
  val CURLOPT_MAXREDIRS       : int
  val CURLOPT_COOKIEFILE      : int
  val CURLOPT_USERAGENT       : int
  val CURLOPT_TIMEOUT         : int
  val CURLOPT_PROXY           : int
  val CURLOPT_VERBOSE         : int
  val CURLOPT_ACCEPT_ENCODING : int
  val CURLOPT_HTTPHEADER      : int
  val CURLOPT_WRITEHEADER     : int
  val CURLOPT_POST            : int
  val CURLOPT_HEADERDATA      : int
  val CURLOPT_WRITEDATA       : int
  val CURLOPT_HEADERFUNCTION  : int
  val CURLOPT_WRITEFUNCTION   : int
  val CURLMSG_DONE : int
  val CURL_SOCKET_TIMEOUT : int
  val CURL_POLL_IN        : int
  val CURL_POLL_OUT       : int
  val CURL_POLL_INOUT     : int
  val CURL_CSELECT_IN     : int
  val CURL_CSELECT_OUT    : int
  val CURLMOPT_SOCKETFUNCTION : int
  val CURLMOPT_TIMERFUNCTION  : int
  val CURLINFO_EFFECTIVE_URL : int
end

structure CurlConst : CURL_CONST =
struct

  val CURLE_OK = 0

  val CURL_GLOBAL_ALL = 3

  val CURLE_FAILED_INIT     = 2
  val CURLE_COULDNT_CONNECT = 7


  val CURLOPT_URL             = 10002
  val CURLOPT_FOLLOWLOCATION  = 52
  val CURLOPT_MAXREDIRS       = 68
  val CURLOPT_COOKIEFILE      = 10031
  val CURLOPT_USERAGENT       = 10018
  val CURLOPT_TIMEOUT         = 13
  val CURLOPT_PROXY           = 10004
  val CURLOPT_VERBOSE         = 41
  val CURLOPT_ACCEPT_ENCODING = 10102

  val CURLOPT_HTTPHEADER      = 10023
  val CURLOPT_WRITEHEADER     = 10029

  val CURLOPT_POST            = 47

  val CURLOPT_HEADERDATA      = 10029
  val CURLOPT_WRITEDATA       = 10001
  val CURLOPT_HEADERFUNCTION  = 20079
  val CURLOPT_WRITEFUNCTION   = 20011



  val CURLMSG_DONE = 1

  val CURL_SOCKET_TIMEOUT = ~1

  val CURL_POLL_IN        = 1
  val CURL_POLL_OUT       = 2
  val CURL_POLL_INOUT     = 3
  val CURL_CSELECT_IN     = 1
  val CURL_CSELECT_OUT    = 2


  val CURLMOPT_SOCKETFUNCTION = 20001
  val CURLMOPT_TIMERFUNCTION  = 20004

  val CURLINFO_EFFECTIVE_URL = 1048577

end
