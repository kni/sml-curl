(* perl -ne 'print "  $1: int\n" if /(val\s+\w+\s+)=/' curl-const.sml *)

signature CURL_CONST =
sig
  val CURLE_OK : int
  val CURL_GLOBAL_ALL : int
  val CURLE_FAILED_INIT     : int
  val CURLE_COULDNT_CONNECT : int
  val CURLE_OPERATION_TIMEDOUT : int
  val CURLOPT_URL             : int
  val CURLOPT_FOLLOWLOCATION  : int
  val CURLOPT_SSL_VERIFYPEER  : int
  val CURLOPT_SSL_VERIFYHOST  : int
  val CURLOPT_FORBID_REUSE    : int
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
  val CURL_POLL_REMOVE    : int
  val CURL_CSELECT_IN     : int
  val CURL_CSELECT_OUT    : int
  val CURLMOPT_SOCKETFUNCTION : int
  val CURLMOPT_TIMERFUNCTION  : int
  val CURLINFO_EFFECTIVE_URL : int
end
