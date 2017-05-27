open CurlHTTP
fun main_handle () = Curl.withCurl ( fn () => 
    let
      val _ = print ((Curl.version ()) ^ "\n")

      fun onHead (is_success, status, reason, new_url, headers, redirects) = (
          print ("onHead: " ^ (Int.toString status) ^ " (" ^ reason ^ ")\n");
          true
        )

      fun onBody body = ( print ("size=" ^ (Int.toString(String.size body)) ^ "\n"); true )

      val opt = [
          (* (HttpVerbose true), *)
          (HttpFollowlocation true),
          (HttpSSLVerifyPeer false),
          (HttpSSLVerifyHost false),
          (HttpMaxRedirs 5),
          (HttpCookieFile ""),
          (HttpUserAgent "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"),
          (* (HttpProxy "127.0.0.1:3128"), *)
          (HttpTimeout 30),
          (HttpAcceptEncoding "gzip"),
          (HttpHeaders [("Accept", "*/*"), ("Accept-Language", "*")])

          (* 
          , (HttpOnHead onHead)
          , (HttpOnBody onBody)
          *)
        ]

      fun cb(is_success, status, reason, new_url, body, headers, redirects) =
        print ((Int.toString status) ^ " (" ^ reason ^ ") " ^ new_url ^ " BodySize=" ^ (Int.toString(String.size body)) ^ "\n")
    in
      doHttp NONE "https://www.google.com/" opt cb
    end
  )


fun main () = main_handle () handle exc => print ("function main raised an exception: " ^ exnMessage exc ^ "\n")
