structure CurlHTTP :
sig
  datatype HttpOpt = HttpVerbose            of bool
                   | HttpFollowlocation     of bool
                   | HttpMaxRedirs          of int
                   | HttpCookieFile         of string
                   | HttpUserAgent          of string
                   | HttpTimeout            of int
                   | HttpProxy              of string
                   | HttpAcceptEncoding     of string
                   | HttpHeaders            of (string * string) list

  type curl_ev_add_handle = Curl.Easy.curl * (Curl.Easy.curl * int -> unit) * int -> unit

  type http_cb = ((bool * int * string * string * string * (string * string) list * (string * string) list list) -> unit)

  val doHttp: curl_ev_add_handle option -> string -> HttpOpt list -> http_cb -> unit
end 
=
struct
  open Curl Curl.Const

  datatype HttpOpt = HttpVerbose            of bool
                   | HttpFollowlocation     of bool
                   | HttpMaxRedirs          of int
                   | HttpCookieFile         of string
                   | HttpUserAgent          of string
                   | HttpTimeout            of int
                   | HttpProxy              of string
                   | HttpAcceptEncoding     of string
                   | HttpHeaders            of (string * string) list


  type curl_ev_add_handle = Easy.curl * (Easy.curl * int -> unit) * int -> unit

  type http_cb = ((bool * int * string * string * string * (string * string) list * (string * string) list list) -> unit)


  fun setHttpOpt curl opt =
    let
      fun doit (HttpVerbose true)         = ( Easy.setopt_int(curl, CURLOPT_VERBOSE, 1)         ; NONE )
        | doit (HttpVerbose false)        = ( Easy.setopt_int(curl, CURLOPT_VERBOSE, 0)         ; NONE )
        | doit (HttpFollowlocation true)  = ( Easy.setopt_int(curl, CURLOPT_FOLLOWLOCATION, 1)  ; NONE )
        | doit (HttpFollowlocation false) = ( Easy.setopt_int(curl, CURLOPT_FOLLOWLOCATION, 0)  ; NONE )
        | doit (HttpMaxRedirs v)          = ( Easy.setopt_int(curl, CURLOPT_MAXREDIRS, v)       ; NONE )
        | doit (HttpCookieFile v)         = ( Easy.setopt_str(curl, CURLOPT_COOKIEFILE, v)      ; NONE )
        | doit (HttpUserAgent v)          = ( Easy.setopt_str(curl, CURLOPT_USERAGENT, v)       ; NONE )
        | doit (HttpProxy v)              = ( Easy.setopt_str(curl, CURLOPT_PROXY, v)           ; NONE )
        | doit (HttpTimeout v)            = ( Easy.setopt_int(curl, CURLOPT_TIMEOUT, v)         ; NONE )
        | doit (HttpAcceptEncoding v)     = ( Easy.setopt_str(curl, CURLOPT_ACCEPT_ENCODING, v) ; NONE )
        | doit (HttpHeaders l)            = SOME (Easy.setopt_list(curl, CURLOPT_HTTPHEADER, (List.map (fn(n,v) => (n ^ ": " ^ v) ) l)))

      val free_setopt_list = List.foldl (fn(opt, free) => case doit opt of NONE => free | SOME f => f::free) [] opt

      val free = fn () => List.foldl (fn(f,_) => (f ())) () free_setopt_list
    in
      free
    end


  (* val timeout = timeout_option opt *)
  fun timeout_option []      = 5 * 60
    | timeout_option (x::xs) = case x of (HttpTimeout t) => t | _ => timeout_option xs
     

  fun doHttp curl_ev url opt cb =
    let
      val curl = Easy.init ()
      
      val _ = Easy.setopt_str(curl, CURLOPT_URL, url)

      val free = setHttpOpt curl opt


      val headers_ref = ref []
      fun header_cb s = (headers_ref := s::(!headers_ref) ; String.size s)
      val _ = Easy.setopt_cb(curl, CURLOPT_HEADERFUNCTION, header_cb)

      val body_ref = ref []
      fun write_cb s = (body_ref := s::(!body_ref); String.size s)
      val _ = Easy.setopt_cb(curl, CURLOPT_WRITEFUNCTION, write_cb)


      fun headers_parse headers =
        let
          val chomp = Substring.dropr  (fn c => c = #"\r" orelse c = #"\n")
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
                    val (n, v) = split_by_colon(chomp s)
                    val v = drop_l_space v
                  in 
                    ((Substring.string(n), Substring.string(v))::r) 
                  end 
                ) [] t

              val (version, ft)    = split_space f
              val (status, reason) = split_space ft
            in
              ("Status", Substring.string status)::("Reason", Substring.string reason)::hh
            end

          fun doit []      b c = c
            | doit (a::aa) b c = if a = "\r\n" then doit aa [] (header_parse(List.rev(b))::c) else doit aa (a::b) c


          val gh = doit headers [] []
          val h = List.hd gh
          val r = List.tl gh

          fun get_h name hs = case List.find (fn(n,v) => n = name) hs of NONE => NONE | SOME (n, v) => SOME v
          val status = valOf(get_h "Status" h)
          val reason = valOf(get_h "Reason" h)

        in
          case Int.fromString status of
               NONE        => (599, "Status in not number", h, r) 
             | SOME status => (status, reason, h, r) 
        end

      fun printHeaders hs = List.foldl ( fn(h,r) => ( print(String.concat( List.map (fn(n,v) => (n ^ ": " ^ v ^ "\n")) h )) ; print("\n"); r) ) 0 hs


      fun finish (curl, result) = (
        if List.null (!headers_ref)
        then
          let
            val reason = Easy.strerror(result)
          in
            Easy.cleanup(curl);
            free ();
            cb(false, 500, reason, "", "", [("Status", "500"), ("Reason", reason)], [])
          end
        else
          let
            val (status, reason, headers, redirects) = headers_parse (List.rev(!headers_ref))
            (* val _ = printHeaders (headers::redirects) *)
            val is_success = status >= 200 andalso status < 300
            val new_url =  Easy.getinfo_str(curl, CURLINFO_EFFECTIVE_URL)
            val body = String.concat (List.rev (!body_ref))
          in
            if result = CURLE_OK
            then (
                Easy.cleanup(curl);
                free ();
                cb(is_success, status, reason, new_url, body, headers, redirects)
              )
            else
              let
                val reason = Easy.strerror(result)
              in
                Easy.cleanup(curl);
                free ();
                cb(false, 599, reason, new_url, body, headers, redirects)
              end
          end
        )
      
    in
      case curl_ev of
          NONE         => ( finish (curl, Easy.perform(curl)) )
        | SOME curl_ev => ( curl_ev(curl, finish, (timeout_option opt)) )
    end
end
