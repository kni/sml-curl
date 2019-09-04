open CurlHTTP
fun main_handle () = Curl.withCurl ( fn () =>
    let
      val _ = print ((Curl.version ()) ^ "\n")

      open Curl
      open Ev

      val ev = evInit ()

      val ev_continue = ref 0

      val timer_id = evTimerNew ev
      val _ = evTimerAdd ev (timer_id, Time.fromSeconds(5 * 60), fn () => (print "END timeout\n" ; ev_continue := 0 ));

      val multi = Multi.init () (* ToDo val withCurlMulti : (multi -> unit) -> init *)
      val curl_ev = CurlEv.curl_ev ev multi

      val urls = ref [
          "https://www.google.com/",
          "http://sml-family.org/",
          "http://www.polyml.org/",
          "http://www.mlton.org/",
          "http://melsman.github.io/mlkit",
          "http://www.mpi-sws.org/~rossberg/hamlet/",
          "http://www.smlnj.org/",
          "http://www.pllab.riec.tohoku.ac.jp/smlsharp/",
          "http://manticore.cs.uchicago.edu/"
        ]

      val opt = [
          (* (HttpVerbose true), *)
          (HttpFollowLocation true),
          (HttpSSLVerifyPeer false),
          (HttpSSLVerifyHost false),
          (HttpMaxRedirs 5),
          (HttpCookieFile ""),
          (HttpUserAgent "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"),
          (* (HttpProxy "127.0.0.1:3128"), *)
          (HttpTimeout 30),
          (HttpAcceptEncoding "gzip"),
          (HttpHeaders [("Accept", "*/*"), ("Accept-Language", "*")])
        ]

      fun cb add_request url (is_success, status, reason, new_url, body, headers, redirects) =
        (
        ev_continue := (!ev_continue) - 1;
        (
        if is_success
        then print ("Answer:  " ^ (Int.toString status) ^ " (" ^ reason ^ ") " ^ new_url ^ "\n")
        else print ("Answer:  " ^ (Int.toString status) ^ " (" ^ reason ^ ") " ^     url ^ "\n")
        );
        add_request ()
        )


     fun add_request () =
        case (!urls) of
             [] => ()
           | (u::us) => (
             urls := us;
             ev_continue := (!ev_continue) + 1;
             print ("Request: " ^ u ^ "\n");
             doHttpEv curl_ev u opt (cb add_request u)
           )

   in
     add_request ();
     add_request ();
     add_request ();
     add_request ();
     add_request ();
     while (!ev_continue) > 0 do evWait ev (SOME (Time.fromSeconds 3));
     Multi.cleanup multi;
     ()
   end

  )


fun main () = main_handle () handle exc => print ("function main raised an exception: " ^ exnMessage exc ^ "\n")
