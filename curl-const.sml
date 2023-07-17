structure CurlConst : CURL_CONST =
struct

  val CURLE_OK = 0

  val CURL_GLOBAL_ALL = 3

  val CURLE_FAILED_INIT     = 2
  val CURLE_COULDNT_CONNECT = 7
  val CURLE_OPERATION_TIMEDOUT = 28


  val CURLOPT_URL             = 10002
  val CURLOPT_FOLLOWLOCATION  = 52
  val CURLOPT_SSL_VERIFYPEER  = 64
  val CURLOPT_SSL_VERIFYHOST  = 81
  val CURLOPT_FORBID_REUSE    = 75
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
