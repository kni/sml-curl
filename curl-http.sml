structure CurlHTTP :
sig
  datatype HttpOpt = HttpVerbose            of bool
                   | HttpFollowLocation     of bool
                   | HttpSSLVerifyPeer      of bool
                   | HttpSSLVerifyHost      of bool
                   | HttpForbidReuse        of bool
                   | HttpMaxRedirs          of int
                   | HttpCookieFile         of string
                   | HttpUserAgent          of string
                   | HttpTimeout            of int
                   | HttpProxy              of string
                   | HttpAcceptEncoding     of string
                   | HttpMaxSize            of int
                   | HttpHeaders            of (string * string) list
                   | HttpOnHead             of (bool * int * string * string * (string * string) list * (string * string) list list) -> bool
                   | HttpOnBody             of string -> bool

  type curlEvAddHandle = Curl.Easy.curl * (Curl.Easy.curl * int -> unit) * int -> unit

  type httpR = bool * int * string * string * string * (string * string) list * (string * string) list list
  type httpCb = httpR -> unit

  val doHttpEvCurl: curlEvAddHandle -> Curl.Easy.curl -> string -> HttpOpt list -> httpCb -> unit

  val doHttpEv: curlEvAddHandle -> string -> HttpOpt list -> httpCb -> unit

  val doHttpCurl: Curl.Easy.curl -> string -> HttpOpt list -> httpR

  val doHttp: string -> HttpOpt list -> httpR

end
=
struct
  open Curl Curl.Const

  datatype HttpOpt = HttpVerbose            of bool
                   | HttpFollowLocation     of bool
                   | HttpSSLVerifyPeer      of bool
                   | HttpSSLVerifyHost      of bool
                   | HttpForbidReuse        of bool
                   | HttpMaxRedirs          of int
                   | HttpCookieFile         of string
                   | HttpUserAgent          of string
                   | HttpTimeout            of int
                   | HttpProxy              of string
                   | HttpAcceptEncoding     of string
                   | HttpMaxSize            of int
                   | HttpHeaders            of (string * string) list
                   | HttpOnHead             of (bool * int * string * string * (string * string) list * (string * string) list list) -> bool
                   | HttpOnBody             of string -> bool


  type curlEvAddHandle = Easy.curl * (Easy.curl * int -> unit) * int -> unit

  type httpR = bool * int * string * string * string * (string * string) list * (string * string) list list
  type httpCb = httpR -> unit

  fun setHttpOpt curl onHead onBody maxSize opt =
    let
      fun doit (HttpVerbose true)         = ( Easy.setopt_int(curl, CURLOPT_VERBOSE, 1)         ; NONE )
        | doit (HttpVerbose false)        = ( Easy.setopt_int(curl, CURLOPT_VERBOSE, 0)         ; NONE )
        | doit (HttpFollowLocation true)  = ( Easy.setopt_int(curl, CURLOPT_FOLLOWLOCATION, 1)  ; NONE )
        | doit (HttpFollowLocation false) = ( Easy.setopt_int(curl, CURLOPT_FOLLOWLOCATION, 0)  ; NONE )
        | doit (HttpSSLVerifyPeer true)   = ( Easy.setopt_int(curl, CURLOPT_SSL_VERIFYPEER, 1)  ; NONE )
        | doit (HttpSSLVerifyPeer false)  = ( Easy.setopt_int(curl, CURLOPT_SSL_VERIFYPEER, 0)  ; NONE )
        | doit (HttpSSLVerifyHost true)   = ( Easy.setopt_int(curl, CURLOPT_SSL_VERIFYHOST, 1)  ; NONE )
        | doit (HttpSSLVerifyHost false)  = ( Easy.setopt_int(curl, CURLOPT_SSL_VERIFYHOST, 0)  ; NONE )
        | doit (HttpForbidReuse true)     = ( Easy.setopt_int(curl, CURLOPT_FORBID_REUSE, 1)    ; NONE )
        | doit (HttpForbidReuse false)    = ( Easy.setopt_int(curl, CURLOPT_FORBID_REUSE, 0)    ; NONE )
        | doit (HttpMaxRedirs v)          = ( Easy.setopt_int(curl, CURLOPT_MAXREDIRS, v)       ; NONE )
        | doit (HttpCookieFile v)         = ( Easy.setopt_str(curl, CURLOPT_COOKIEFILE, v)      ; NONE )
        | doit (HttpUserAgent v)          = ( Easy.setopt_str(curl, CURLOPT_USERAGENT, v)       ; NONE )
        | doit (HttpProxy v)              = ( Easy.setopt_str(curl, CURLOPT_PROXY, v)           ; NONE )
        | doit (HttpTimeout v)            = ( Easy.setopt_int(curl, CURLOPT_TIMEOUT, v)         ; NONE )
        | doit (HttpAcceptEncoding v)     = ( Easy.setopt_str(curl, CURLOPT_ACCEPT_ENCODING, v) ; NONE )
        | doit (HttpMaxSize v)            = ( maxSize := v ; NONE )
        | doit (HttpHeaders l)            = SOME (Easy.setopt_list(curl, CURLOPT_HTTPHEADER, (List.map (fn (n,v) => (n ^ ": " ^ v) ) l)))
        | doit (HttpOnHead f)             = ( onHead := SOME f ; NONE )
        | doit (HttpOnBody f)             = ( onBody := SOME f ; NONE )

      val free_setopt_list = List.foldl (fn (opt, free) => case doit opt of NONE => free | SOME f => f::free) [] opt

      fun free () = List.foldl (fn (f,_) => f ()) () free_setopt_list
    in
      free
    end


  (* val timeout = timeout_option opt *)
  fun timeout_option []      = 5 * 60
    | timeout_option (x::xs) = case x of (HttpTimeout t) => t | _ => timeout_option xs


  fun prepare curl url opt =
    let
      val _ = Easy.setopt_str(curl, CURLOPT_URL, url)
      val _ = Easy.setopt_int(curl, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_1_1)

      val onHead = ref NONE
      val onBody = ref NONE
      val maxSize = ref 0

      val free = setHttpOpt curl onHead onBody maxSize opt


      fun headers_parse headers = (
        let
          val chomp = Substring.dropr (fn c => c = #"\r" orelse c = #"\n")
          val drop_l_space = Substring.dropl (fn c => c = #" ")

          fun split by s =
            let
              val (f, t) = Substring.splitl (fn c => c <> by) s
              val t      = Substring.dropl  (fn c => c = by)  t
            in
              (f, t)
            end

          val split_space    = split #" "
          val split_by_colon = split #":"

          fun header_parse hs =
            let
              val hs = List.map (chomp o Substring.full) hs
              val f  = List.hd hs
              val t  = List.tl hs

              val hh = List.foldl (fn(s,r) =>
                  let
                    val (n, v) = split_by_colon (chomp s)
                    val v = drop_l_space v
                  in
                    (String.map Char.toLower (Substring.string n), Substring.string v)::r
                  end
                ) [] t

              val (version, ft)    = split_space f
              val (status, reason) = split_space ft
            in
              ("Protocol", Substring.string version)::("Status", Substring.string status)::("Reason", Substring.string reason)::hh
            end

          fun doit []      b c = c
            | doit (a::aa) b c = if a = "\r\n" then doit aa [] (header_parse (List.rev(b))::c) else doit aa (a::b) c


          val gh = doit headers [] []
          val h = List.hd gh
          val r = List.tl gh

          fun get_h name hs = case List.find (fn(n,v) => n = name) hs of NONE => NONE | SOME (n, v) => SOME v
          val status = valOf (get_h "Status" h)
          val reason = valOf (get_h "Reason" h)

        in
          case Int.fromString status of
               NONE        => (599, "Status in not number", h, r)
             | SOME status => (status, reason, h, r)
        end
      )

      val headers_ref = ref []
      fun header_cb s = (headers_ref := s::(!headers_ref) ; String.size s)
      val _ = Easy.setopt_cb(curl, CURLOPT_HEADERFUNCTION, header_cb)

      val headers_parsed_ref = ref NONE

      fun on_head s = case (!onHead) of NONE => true | SOME f =>
        let
          val hs = headers_parse (List.rev (!headers_ref))
          val _ = headers_parsed_ref := SOME hs
          val (status, reason, headers, redirects) = hs
          val is_success = status >= 200 andalso status < 300
          val new_url =  Easy.getinfo_str (curl, CURLINFO_EFFECTIVE_URL)
        in
          onHead := NONE;
          f (is_success, status, reason, new_url, headers, redirects) handle exc => false
        end

      val respSize = ref 0
      val body_ref = ref []

      fun write_cb s =
        let
          val size = String.size s
        in
          respSize := (!respSize) + size;
          if (!maxSize) > 0 andalso (!maxSize) < (!respSize) then 0 else
          case on_head s of false  => 0 | true =>
          case (!onBody) of
               NONE   => ( body_ref := s::(!body_ref); size )
             | SOME f => (if f s then size else 0) handle exc => 0
        end

      val _ = Easy.setopt_cb (curl, CURLOPT_WRITEFUNCTION, write_cb)


      fun printHeaders hs = List.foldl (fn (h,r) => (
        print (String.concat ( List.map (fn (n,v) => (n ^ ": " ^ v ^ "\n")) h ));
        print "\n";
        r
        )) 0 hs


      fun finish (curl, result) = (
        if List.null (!headers_ref)
        then
          let
            val reason = Easy.strerror result
          in
            free ();
            (false, 500, reason, "", "", [("Status", "500"), ("Reason", reason)], [])
          end
        else
          let
            val (status, reason, headers, redirects) = case !headers_parsed_ref of NONE => headers_parse (List.rev(!headers_ref)) | SOME hs => hs
            val _ = headers_ref := []
            (* val _ = printHeaders (headers::redirects) *)
            val is_success = status >= 200 andalso status < 300
            val new_url =  Easy.getinfo_str(curl, CURLINFO_EFFECTIVE_URL)
            val body = String.concat (List.rev (!body_ref))
            val _ = body_ref := []
          in
            if result = CURLE_OK
            then (
                free ();
                (is_success, status, reason, new_url, body, headers, redirects)
              )
            else
              let
                val reason = Easy.strerror result
              in
                free ();
                (false, 599, reason, new_url, body, headers, redirects)
              end
          end
        )

    in
      finish
    end



  fun doHttpEvCurl curl_ev curl url opt cb =
    let
      val finish = prepare curl url opt;

      fun finish' (curl, result) = cb (finish (curl, result))
    in
      curl_ev (curl, finish', timeout_option opt)
    end


  fun doHttpEv curl_ev url opt cb =
    let
      val curl = Easy.init ()
    in
      doHttpEvCurl curl_ev curl url opt (fn r => (Easy.cleanup curl; cb r))
    end


  fun doHttpCurl curl url opt =
    let
      val finish = prepare curl url opt;
    in
      finish (curl, Easy.perform curl)
    end


  fun doHttp url opt =
    let
      val curl = Easy.init ()
      val r = doHttpCurl curl url opt
    in
      Easy.cleanup curl;
      r
    end

end
