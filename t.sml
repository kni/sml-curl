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
          (HttpFollowLocation true),
          (HttpSSLVerifyPeer false),
          (HttpSSLVerifyHost false),
          (* (HttpForbidReuse true), *)
          (HttpMaxRedirs 5),
          (HttpCookieFile ""),
          (HttpUserAgent "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"),
          (* (HttpProxy "127.0.0.1:3128"), *)
          (HttpTimeout 30),
          (HttpAcceptEncoding "gzip"),
          (HttpHeaders [("Accept", "*/*"), ("Accept-Language", "*")]),
          (HttpMaxSize (5 * 1025 * 1024))

          (*
          , (HttpOnHead onHead)
          , (HttpOnBody onBody)
          *)
        ]

      fun getHeader h [] = "-"
        | getHeader h ((n,v)::hs) = if h = n then v else getHeader h hs

      fun printResult (is_success, status, reason, new_url, body, headers, redirects) =
          print ((Int.toString status) ^ " (" ^ reason ^ ") " ^ new_url ^
            " BodySize=" ^ (Int.toString(String.size body)) ^
            " Protocol=" ^ (getHeader "Protocol" headers) ^
            " Connection=" ^ (getHeader "connection" headers) ^
            "\n"
          )

    in
      printResult (doHttp "https://www.google.com/" opt);

      let
        val curl = Curl.Easy.init ()
      in
        printResult (doHttpCurl curl "https://www.bing.com/" opt);
        printResult (doHttpCurl curl "https://www.yahoo.com/" opt);
        printResult (doHttpCurl curl "http://sml-family.org/" opt);
        Curl.Easy.cleanup curl
      end
    end
  )


fun main () = main_handle () handle exc => print ("function main raised an exception: " ^ exnMessage exc ^ "\n")
