open CurlHTTP
fun main_handle () = Curl.withCurl ( fn () => 
    let
      val _ = print ((Curl.version ()) ^ "\n")

      val opt = [
          (* (HttpVerbose true), *)
          (HttpFollowlocation true),
          (HttpMaxRedirs 5),
          (HttpCookieFile ""),
          (HttpUserAgent "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"),
          (* (HttpProxy "127.0.0.1:3128"), *)
          (HttpTimeout 30),
          (HttpAcceptEncoding "gzip"),
          (HttpHeaders [("Accept", "*/*"), ("Accept-Language", "*")])
        ]

      fun cb(is_success, status, reason, new_url, body, headers, redirects) =
        print ((Int.toString status) ^ " (" ^ reason ^ ") " ^ new_url ^ "\n")
    in
      doHttp NONE "https://www.google.com/" opt cb
    end
  )


fun main () = main_handle () handle exc => print ("function main raised an exception: " ^ exnMessage exc ^ "\n")
