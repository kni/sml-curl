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
  open MLton.Pointer

  val malloc = (_import "malloc": Word.word -> t;) o Word.fromInt
  val free   = _import "free": t -> unit;

  val curl_global_init    = _import "curl_global_init": int -> int;
  val curl_global_cleanup = _import "curl_global_cleanup": unit -> unit;


  fun withCurl f = (
      curl_global_init(CURL_GLOBAL_ALL);
      f () handle exc => print (exnMessage exc);
      curl_global_cleanup()
    )

  fun readCString p =
    let
      val () = ()
      fun doit p l =
        let
          val w = getWord8(p, 0)
        in
          if w = 0w0
          then String.implode (List.rev l)
          else doit (add(p, 0w1)) ((Byte.byteToChar w)::l)
        end
    in
      doit p []
    end


  val curl_version_ffi = _import "curl_version": unit -> t;
  fun curl_version () = let val p = curl_version_ffi () in readCString p end

  val is_64bit = sizeofPointer = 0w8

  structure Easy =
  struct
    type curl = t
    structure H = HashArrayLargeInt

    val hash_size = 100
    val headerfunction_cb_H = H.hash hash_size (* easy_int => cb *)
    val writefunction_cb_H  = H.hash hash_size (* easy_int => cb *)

    val easy2int = C_Size.toLargeInt o C_Pointer.toWord

    val curl_easy_init_ffi = _import "curl_easy_init": unit -> t;

    fun curl_easy_init () =
      let
        val curl = curl_easy_init_ffi ()
      in
        if curl = null
        then raise Curl CURLE_FAILED_INIT
        else curl
      end

    val curl_easy_setopt_str = _import "curl_easy_setopt": t * int * string -> int;


    val curl_easy_setopt_int_32bit = _import "curl_easy_setopt": t * int * Int32.int -> int;
    val curl_easy_setopt_int_64bit = _import "curl_easy_setopt": t * int * Int64.int -> int;

    fun curl_easy_setopt_int (p, opt, v) =
      if is_64bit
      then curl_easy_setopt_int_64bit (p, opt, (Int64.fromInt v) )
      else curl_easy_setopt_int_32bit (p, opt, (Int32.fromInt v) )


    val curl_easy_perform = _import "curl_easy_perform" reentrant: t -> int;


    val curl_easy_cleanup_ffi = _import "curl_easy_cleanup" reentrant: t -> int;
    fun curl_easy_cleanup curl =
      let
        val easy_int = easy2int curl
      in
        H.delete(headerfunction_cb_H, easy_int);
        H.delete(writefunction_cb_H, easy_int);
        curl_easy_cleanup_ffi curl
      end


    val curl_easy_setopt_void = _import "curl_easy_setopt": t * int * t -> int;

    val curl_easy_setopt_cb_ffi = _import "curl_easy_setopt" reentrant: t * int * t -> int;

    val curlopt_headerfunction_cb_export = _export  "curlopt_headerfunction_cb": (t * int * int * t -> int) -> unit;
    val curlopt_headerfunction_cb        = _address "curlopt_headerfunction_cb" public: t;

    val curlopt_writefunction_cb_export  = _export  "curlopt_writefunction_cb": (t * int * int * t -> int) -> unit;
    val curlopt_writefunction_cb         = _address "curlopt_writefunction_cb" public: t;

          fun cb_low_h_f (ptr, size, nmemb, curl) =
            let
              val length = size * nmemb
              val arr = Word8Array.tabulate(length, (fn(i) => getWord8(ptr, i)))
              val s = Byte.bytesToString (Word8Array.vector arr)

              val easy_int = easy2int curl
              val cb = valOf(H.sub(headerfunction_cb_H, easy_int))
            in
              cb s
            end

          fun cb_low_w_f (ptr, size, nmemb, curl) =
            let
              val length = size * nmemb
              val arr = Word8Array.tabulate(length, (fn(i) => getWord8(ptr, i)))
              val s = Byte.bytesToString (Word8Array.vector arr)

              val easy_int = easy2int curl
              val cb = valOf(H.sub(writefunction_cb_H, easy_int))
            in
              cb s
            end

    val _ = curlopt_headerfunction_cb_export cb_low_h_f
    val _ = curlopt_writefunction_cb_export cb_low_w_f

    fun curl_easy_setopt_cb(curl, opt, cb) =
      if opt = CURLOPT_HEADERFUNCTION
      then
        let
          val easy_int = easy2int curl
          val _ = H.update(headerfunction_cb_H, easy_int, cb)
          val _ = curl_easy_setopt_void(curl, CURLOPT_HEADERDATA, curl)
        in
          curl_easy_setopt_cb_ffi(curl, opt, curlopt_headerfunction_cb)
        end

      else
      if opt = CURLOPT_WRITEFUNCTION
      then
        let
          val easy_int = easy2int curl
          val _ = H.update(writefunction_cb_H, easy_int, cb)
          val _ = curl_easy_setopt_void(curl, CURLOPT_WRITEDATA, curl)
        in
          curl_easy_setopt_cb_ffi(curl, opt, curlopt_writefunction_cb)
        end

      else 0




    val curl_easy_setopt_list_ffi  = _import "curl_easy_setopt": t * int * t -> int;

    fun curl_easy_setopt_list(curl, opt, l) =
      let
        val l = List.map String.toCString l

        val cnt  = List.length l
        val size = List.foldl (fn (s,size) => size + String.size s) 0 l

        val mem  = malloc(2 * (Word.toInt sizeofPointer) * cnt + size + cnt)

        fun doit []      p = []
          | doit (x::xs) p =
              let
                val sp = add(p, 0w2 * sizeofPointer)
                val np = if List.null xs then null else add(p, 0w2 * sizeofPointer + Word.fromInt(1 + String.size x))
              in
                setPointer(p, 0, sp);
                setPointer(p, 1, np);
                Word8Vector.foldli (fn(i, c, r) => (setWord8(sp, i, c) ; r + 1 ) ) 0 (Byte.stringToBytes x);
                setWord8(sp, (String.size x), 0w0);
                doit xs np
              end


      in
        doit l mem;
        curl_easy_setopt_list_ffi(curl, opt, mem);
        fn () => free mem
      end



    val curl_easy_getinfo_str_ffi  = _import "curl_easy_getinfo": t * int * t ref -> int;

    fun curl_easy_getinfo_str(curl, info) =
      let
        val pp = ref null
      in
        curl_easy_getinfo_str_ffi(curl, info, pp);
        readCString (!pp)
      end


    val curl_easy_strerror_ffi  = _import "curl_easy_strerror": int -> t;

    val curl_easy_strerror = readCString o curl_easy_strerror_ffi

  end

  structure Multi =
  struct
    type multi = t
    type easy = Easy.curl

    val easy2int = C_Size.toLargeInt o C_Pointer.toWord

    val curl_multi_init_ffi  = _import "curl_multi_init": unit -> t;

    fun curl_multi_init () =
      let
        val multi = curl_multi_init_ffi ()
      in
        if multi = null
        then raise Curl CURLE_FAILED_INIT
        else multi
      end

    val curl_multi_cleanup  = _import "curl_multi_cleanup" reentrant: t -> int;


    val curl_multi_setopt_timer_cb_ffi  = _import "curl_multi_setopt" reentrant: t * int * t -> int;
    val curlmopt_timerfunction_cb_export_32bit = _export  "curlmopt_timerfunction_cb_32bit" : (t * Int32.int * t -> int) -> unit;
    val curlmopt_timerfunction_cb_export_64bit = _export  "curlmopt_timerfunction_cb_64bit" : (t * Int64.int * t -> int) -> unit;
    val curlmopt_timerfunction_cb_32bit        = _address "curlmopt_timerfunction_cb_32bit" public: t;
    val curlmopt_timerfunction_cb_64bit        = _address "curlmopt_timerfunction_cb_64bit" public: t;

    fun curl_multi_setopt_timer_cb(multi, cb) = (
        if is_64bit
        then (
          curlmopt_timerfunction_cb_export_64bit ( fn(multi, timeout_ms, _) => cb(multi, Int64.toInt timeout_ms) );
          curl_multi_setopt_timer_cb_ffi(multi, CURLMOPT_TIMERFUNCTION, curlmopt_timerfunction_cb_64bit)
        )
        else (
          curlmopt_timerfunction_cb_export_32bit ( fn(multi, timeout_ms, _) => cb(multi, Int32.toInt timeout_ms) );
          curl_multi_setopt_timer_cb_ffi(multi, CURLMOPT_TIMERFUNCTION, curlmopt_timerfunction_cb_32bit)
        )
      )


    val curl_multi_setopt_socket_cb_ffi = _import "curl_multi_setopt" reentrant: t * int * t -> int;
    val curlmopt_socketfunction_cb_export = _export "curlmopt_socketfunction_cb": (t * int * int * t * t -> int) -> unit;
    val curlmopt_socketfunction_cb        = _address "curlmopt_socketfunction_cb" public: t;

    fun curl_multi_setopt_socket_cb(multi, cb) = (
        curlmopt_socketfunction_cb_export ( fn(easy, socket, poll, _, _) => cb(easy, socket, poll) );
        curl_multi_setopt_socket_cb_ffi(multi, CURLMOPT_SOCKETFUNCTION, curlmopt_socketfunction_cb)
      )

    val curl_multi_add_handle    = _import "curl_multi_add_handle" reentrant: t * t -> int;
    val curl_multi_remove_handle = _import "curl_multi_remove_handle" reentrant: t * t -> int;


    val curl_multi_socket_action_ffi = _import "curl_multi_socket_action" reentrant: t * int * int * t -> int;

    val running_handles_mem  = malloc(4)

    fun curl_multi_socket_action(multi, socket, ev_bitmask) =
      let
        val _ = setInt32(running_handles_mem, 0, 0)
        val _ = curl_multi_socket_action_ffi(multi, socket, ev_bitmask, running_handles_mem)
      in
        getInt32(running_handles_mem, 0)
      end


    val curl_multi_info_read_ffi = _import "curl_multi_info_read": t * t -> t;

    val msgs_in_queue_mem = malloc(4)

    fun read_msg p =
      let
        val msg    = getInt32(p, 0)   val p = add(p, sizeofPointer) (* not 0w4 because alignment *)
        val easy   = getPointer(p, 0) val p = add(p, sizeofPointer)
        val result = getInt32(p, 0)
    in
      (msg, easy, result)
    end

    fun curl_multi_info_read(multi) =
      let
        val _ = setInt32(msgs_in_queue_mem, 0, 0)
        val msg_pointer = curl_multi_info_read_ffi(multi, msgs_in_queue_mem)
      in
        if msg_pointer = null
        then NONE
        else SOME (read_msg msg_pointer)
      end

  end

end
