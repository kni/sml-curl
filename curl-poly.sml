signature CURL =
sig
  exception Curl of int
  val withCurl : (unit -> unit) -> unit
  val curl_version : unit -> string

  structure Const : CURL_CONST

  structure Easy :
  sig
    type curl
    val curl_easy_init       : unit -> curl
    val curl_easy_setopt_str : curl * int * string -> int
    val curl_easy_setopt_int : curl * int * int -> int
    val curl_easy_setopt_cb  : curl * int * (string -> int) -> int
    val curl_easy_setopt_list : curl * int * string list -> (unit -> unit)
    val curl_easy_perform    : curl -> int
    val curl_easy_cleanup    : curl -> int
    val curl_easy_getinfo_str : curl * int -> string
    val curl_easy_strerror   : int -> string
  end

  structure Multi :
  sig
    type multi
    type easy
    val easy2int : easy -> LargeInt.int
    val curl_multi_init      : unit -> multi
    val curl_multi_cleanup   : multi -> int
    val curl_multi_setopt_timer_cb  : multi * (multi * int -> int) -> int
    val curl_multi_setopt_socket_cb : multi * (easy *  int * int -> int) -> int

    val curl_multi_add_handle    : multi * easy -> int
    val curl_multi_remove_handle : multi * easy -> int
    val curl_multi_socket_action : multi * int * int -> int
    val curl_multi_info_read     : multi -> (int * easy * int) option
  end

end 

structure Curl : CURL =
struct
  exception Curl of int
  structure Const = CurlConst
  open Const
  open Foreign
  val libcurl = loadLibrary "libcurl.so"

  val curl_global_init    = buildCall1 ((getSymbol libcurl "curl_global_init"),  cInt, cInt)
  val curl_global_cleanup = buildCall1 ((getSymbol libcurl "curl_global_cleanup"),  cVoid, cVoid)

  fun withCurl f = (
      curl_global_init(CURL_GLOBAL_ALL);
      f () handle exc => print (exnMessage exc);
      curl_global_cleanup()
    )

  fun readCString p =
    let
      fun doit p l =
        let
          val w = Memory.get8(p, 0w0)
        in
          if w = 0w0 
          then String.implode (List.rev l)
          else doit (Memory.++(p, 0w1)) ((Byte.byteToChar w)::l)
        end
    in
      doit p []
    end

  val curl_version_ffi = buildCall0 ((getSymbol libcurl "curl_version"), (), cPointer)
  fun curl_version () = let val p = curl_version_ffi () in readCString p end

  structure Easy =
  struct
    type curl = Memory.voidStar

    val curl_easy_init_ffi = buildCall1 ((getSymbol libcurl "curl_easy_init"),  cVoid, cPointer)

    fun curl_easy_init () =
      let 
        val curl = curl_easy_init_ffi ()
      in
        if curl = Memory.null 
        then raise Curl CURLE_FAILED_INIT
        else curl
      end

    val curl_easy_setopt_str  = buildCall3 ((getSymbol libcurl "curl_easy_setopt"),  (cPointer, cInt, cString), cInt)
    val curl_easy_setopt_int  = buildCall3 ((getSymbol libcurl "curl_easy_setopt"),  (cPointer, cInt, cLong), cInt)

    val curl_easy_perform     = buildCall1 ((getSymbol libcurl "curl_easy_perform"),  cPointer, cInt)
    val curl_easy_cleanup     = buildCall1 ((getSymbol libcurl "curl_easy_cleanup"),  cPointer, cInt)

    val curl_easy_setopt_cb_ffi = buildCall3 ((getSymbol libcurl "curl_easy_setopt"),  (cPointer, cInt, cFunction), cInt)

    fun curl_easy_setopt_cb(curl, opt, cb) =
      let
        fun cb_low (ptr, size, nmemb, _) =
          let
            val length = size * nmemb
            val arr = Word8Array.tabulate(length, (fn(i) => Memory.get8(ptr, Word.fromInt i)))
            val s = Byte.bytesToString (Word8Array.vector arr)
          in
            cb s
          end

        val cb_ffi = buildClosure4 (cb_low, (cPointer, cInt, cInt, cVoid), cInt)
      in
        curl_easy_setopt_cb_ffi(curl, opt, cb_ffi)
      end



    val curl_easy_setopt_list_ffi  = buildCall3 ((getSymbol libcurl "curl_easy_setopt"),  (cPointer, cInt, cPointer), cInt)

    val sizeofPointer = #size Foreign.LowLevel.cTypePointer

    val malloc = Memory.malloc o Word.fromInt

    fun curl_easy_setopt_list(curl, opt, l) =
      let
        val l = List.map String.toCString l

        val cnt  = List.length l
        val size = List.foldl (fn (s,size) => size + String.size s) 0 l

        val mem  = malloc(2 * (Word.toInt sizeofPointer) * cnt + size + cnt)

        fun doit []      p = []
          | doit (x::xs) p =
              let 
                open Memory 
                val sp = ++(p, 0w2 * sizeofPointer)
                val np = if List.null xs then null else ++(p, 0w2 * sizeofPointer + Word.fromInt(1 + String.size x))
              in
                setAddress(p, 0w0, sp);
                setAddress(p, 0w1, np);
                Word8Vector.foldli (fn(i, c, r) => (set8(sp, (Word.fromInt i), c) ; r + 1 ) ) 0 (Byte.stringToBytes x);
                set8(sp, (Word.fromInt (String.size x)), 0w0);
                doit xs np
              end

      in
        doit l mem;
        curl_easy_setopt_list_ffi(curl, opt, mem);
        fn () => Memory.free mem
      end



    val curl_easy_getinfo_str_ffi  = buildCall3 ((getSymbol libcurl "curl_easy_getinfo"),  (cPointer, cInt, cStar cPointer), cInt)

    fun curl_easy_getinfo_str(curl, info) = 
      let
        val pp = ref Memory.null
      in
        curl_easy_getinfo_str_ffi(curl, info, pp);
        readCString (!pp)
      end


    val curl_easy_strerror_ffi  = buildCall1 ((getSymbol libcurl "curl_easy_strerror"),  cInt, cPointer)

    val curl_easy_strerror = readCString o curl_easy_strerror_ffi 

  end

  structure Multi =
  struct
    type multi = Memory.voidStar
    type easy = Easy.curl

    val easy2int = SysWord.toLargeInt o Foreign.Memory.voidStar2Sysword

    val curl_multi_init_ffi = buildCall1 ((getSymbol libcurl "curl_multi_init"), cVoid, cPointer)

    fun curl_multi_init () =
      let 
        val multi = curl_multi_init_ffi ()
      in
        if multi = Memory.null 
        then raise Curl CURLE_FAILED_INIT
        else multi
      end

    val curl_multi_cleanup = buildCall1 ((getSymbol libcurl "curl_multi_cleanup"), cPointer, cInt)


    val curl_multi_setopt_timer_cb_ffi = buildCall3 ((getSymbol libcurl "curl_multi_setopt"), (cPointer, cInt, cFunction), cInt)

    fun curl_multi_setopt_timer_cb(multi, cb) =
      let
        fun cb_low (multi, timeout_ms, _) = cb(multi, timeout_ms)
        val cb_ffi = buildClosure3 (cb_low, (cPointer, cLong, cVoid), cInt)
      in
        curl_multi_setopt_timer_cb_ffi(multi, CURLMOPT_TIMERFUNCTION, cb_ffi)
      end

      
    val curl_multi_setopt_socket_cb_ffi = buildCall3 ((getSymbol libcurl "curl_multi_setopt"), (cPointer, cInt, cFunction), cInt)

    fun curl_multi_setopt_socket_cb(multi, cb) =
      let
       fun cb_low(easy, socket, poll, _, _) = cb(easy, socket, poll)
        val cb_ffi = buildClosure5 (cb_low, (cPointer, cInt, cInt, cVoid, cVoid), cInt)
      in
        curl_multi_setopt_socket_cb_ffi(multi, CURLMOPT_SOCKETFUNCTION, cb_ffi)
      end


    val curl_multi_add_handle    = buildCall2 ((getSymbol libcurl "curl_multi_add_handle"),    (cPointer, cPointer), cInt)
    val curl_multi_remove_handle = buildCall2 ((getSymbol libcurl "curl_multi_remove_handle"), (cPointer, cPointer), cInt)


    val curl_multi_socket_action_ffi = buildCall4 ((getSymbol libcurl "curl_multi_socket_action"), (cPointer, cInt, cInt, cStar cInt), cInt)

    fun curl_multi_socket_action(multi, socket, ev_bitmask) =
      let
        val running_handles = ref 0
        val _ = curl_multi_socket_action_ffi(multi, socket, ev_bitmask, running_handles)
      in
        !running_handles
      end


    val curl_multi_info_read_ffi = buildCall2 ((getSymbol libcurl "curl_multi_info_read"), (cPointer, cStar cInt), cPointer)
    val read_msg = #load (breakConversion (cStruct3 (cInt, cPointer, cInt)))

    fun curl_multi_info_read(multi) =
      let
        val msgs_in_queue = ref 0
        val msg_pointer = curl_multi_info_read_ffi(multi, msgs_in_queue)
      in
        if msg_pointer = Memory.null 
        then NONE 
        else SOME (read_msg msg_pointer)
      end

  end

end
