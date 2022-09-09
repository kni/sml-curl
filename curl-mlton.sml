structure Curl : CURL =
struct
  exception Curl of int
  structure Const = CurlConst
  open Const
  open MLton.Pointer

  val malloc = (_import "malloc": Word.word -> t;) o Word.fromInt
  val free   = _import "free": t -> unit;

  val global_init    = _import "curl_global_init": int -> int;
  val global_cleanup = _import "curl_global_cleanup": unit -> unit;

  fun init () = global_init(CURL_GLOBAL_ALL)

  val cleanup = global_cleanup

  fun withCurl f = (
      global_init(CURL_GLOBAL_ALL);
      f () handle exc => (global_cleanup (); raise exc);
      global_cleanup ()
    )

  fun readCString p =
    let
      fun len i = if getWord8 (p, i) = 0w0 then i else len (i + 1)
      val length = len 0
      fun getChar i = Byte.byteToChar (getWord8 (p, i))
    in
      CharVector.tabulate(length, getChar)
    end


  val version_ffi = _import "curl_version": unit -> t;
  fun version () = let val p = version_ffi () in readCString p end

  val is_64bit = sizeofPointer = 0w8

  structure Easy =
  struct
    type curl = t
    structure H = HashArrayLargeInt

    val hash_size = 100
    val headerfunction_cb_H = H.hash hash_size (* easy_int => cb *)
    val writefunction_cb_H  = H.hash hash_size (* easy_int => cb *)

    val easy2int = C_Size.toLargeInt o C_Pointer.toWord

    val init_ffi = _import "curl_easy_init": unit -> t;

    fun init () =
      let
        val curl = init_ffi ()
      in
        if curl = null
        then raise Curl CURLE_FAILED_INIT
        else curl
      end


    val setopt_str_ffi = _import "curl_easy_setopt": t * int * string -> int;
    fun setopt_str (curl, opt, str) = setopt_str_ffi (curl, opt, str ^ "\000")


    val setopt_int_32bit = _import "curl_easy_setopt": t * int * Int32.int -> int;
    val setopt_int_64bit = _import "curl_easy_setopt": t * int * Int64.int -> int;

    fun setopt_int (p, opt, v) =
      if is_64bit
      then setopt_int_64bit (p, opt, (Int64.fromInt v) )
      else setopt_int_32bit (p, opt, (Int32.fromInt v) )


    val perform = _import "curl_easy_perform" reentrant: t -> int;


    val cleanup_ffi = _import "curl_easy_cleanup" reentrant: t -> unit;
    fun cleanup curl =
      let
        val easy_int = easy2int curl
      in
        H.delete(headerfunction_cb_H, easy_int);
        H.delete(writefunction_cb_H, easy_int);
        cleanup_ffi curl
      end


    val setopt_void = _import "curl_easy_setopt": t * int * t -> int;

    val setopt_cb_ffi = _import "curl_easy_setopt" reentrant: t * int * t -> int;

    val curlopt_headerfunction_cb_export = _export  "curlopt_headerfunction_cb": (t * int * int * t -> C_Size.t) -> unit;
    val curlopt_headerfunction_cb        = _address "curlopt_headerfunction_cb" public: t;

    val curlopt_writefunction_cb_export  = _export  "curlopt_writefunction_cb": (t * int * int * t -> C_Size.t) -> unit;
    val curlopt_writefunction_cb         = _address "curlopt_writefunction_cb" public: t;

          fun cb_low_h_f (ptr, size, nmemb, curl) =
            let
              val length = size * nmemb
              val arr = Word8Array.tabulate(length, (fn(i) => getWord8(ptr, i)))
              val s = Byte.bytesToString (Word8Array.vector arr)

              val easy_int = easy2int curl
              val cb = valOf(H.sub(headerfunction_cb_H, easy_int))
            in
              C_Size.fromInt (cb s)
            end

          fun cb_low_w_f (ptr, size, nmemb, curl) =
            let
              val length = size * nmemb
              val arr = Word8Array.tabulate(length, (fn(i) => getWord8(ptr, i)))
              val s = Byte.bytesToString (Word8Array.vector arr)

              val easy_int = easy2int curl
              val cb = valOf(H.sub(writefunction_cb_H, easy_int))
            in
              C_Size.fromInt (cb s)
            end

    val _ = curlopt_headerfunction_cb_export cb_low_h_f
    val _ = curlopt_writefunction_cb_export cb_low_w_f

    fun setopt_cb(curl, opt, cb) =
      if opt = CURLOPT_HEADERFUNCTION
      then
        let
          val easy_int = easy2int curl
          val _ = H.update(headerfunction_cb_H, easy_int, cb)
          val _ = setopt_void(curl, CURLOPT_HEADERDATA, curl)
        in
          setopt_cb_ffi(curl, opt, curlopt_headerfunction_cb)
        end

      else
      if opt = CURLOPT_WRITEFUNCTION
      then
        let
          val easy_int = easy2int curl
          val _ = H.update(writefunction_cb_H, easy_int, cb)
          val _ = setopt_void(curl, CURLOPT_WRITEDATA, curl)
        in
          setopt_cb_ffi(curl, opt, curlopt_writefunction_cb)
        end

      else 0




    val setopt_list_ffi  = _import "curl_easy_setopt": t * int * t -> int;

    fun setopt_list(curl, opt, []) = (setopt_list_ffi(curl, opt, null); fn () => ())
      | setopt_list(curl, opt, l) =
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
        setopt_list_ffi(curl, opt, mem);
        fn () => (setopt_list_ffi(curl, opt, null); free mem)
      end



    val getinfo_str_ffi  = _import "curl_easy_getinfo": t * int * t ref -> int;

    fun getinfo_str(curl, info) =
      let
        val pp = ref null
      in
        getinfo_str_ffi(curl, info, pp);
        readCString (!pp)
      end


    val strerror_ffi  = _import "curl_easy_strerror": int -> t;

    val strerror = readCString o strerror_ffi

  end

  structure Multi =
  struct
    type multi = t
    type easy = Easy.curl

    val easy2int = C_Size.toLargeInt o C_Pointer.toWord

    val init_ffi  = _import "curl_multi_init": unit -> t;

    fun init () =
      let
        val multi = init_ffi ()
      in
        if multi = null
        then raise Curl CURLE_FAILED_INIT
        else multi
      end

    val cleanup  = _import "curl_multi_cleanup" reentrant: t -> int;


    val setopt_timer_cb_ffi  = _import "curl_multi_setopt" reentrant: t * int * t -> int;
    val curlmopt_timerfunction_cb_export_32bit = _export  "curlmopt_timerfunction_cb_32bit" : (t * Int32.int * t -> int) -> unit;
    val curlmopt_timerfunction_cb_export_64bit = _export  "curlmopt_timerfunction_cb_64bit" : (t * Int64.int * t -> int) -> unit;
    val curlmopt_timerfunction_cb_32bit        = _address "curlmopt_timerfunction_cb_32bit" public: t;
    val curlmopt_timerfunction_cb_64bit        = _address "curlmopt_timerfunction_cb_64bit" public: t;

    fun setopt_timer_cb(multi, cb) = (
        if is_64bit
        then (
          curlmopt_timerfunction_cb_export_64bit ( fn(multi, timeout_ms, _) => cb(multi, Int64.toInt timeout_ms) );
          setopt_timer_cb_ffi(multi, CURLMOPT_TIMERFUNCTION, curlmopt_timerfunction_cb_64bit)
        )
        else (
          curlmopt_timerfunction_cb_export_32bit ( fn(multi, timeout_ms, _) => cb(multi, Int32.toInt timeout_ms) );
          setopt_timer_cb_ffi(multi, CURLMOPT_TIMERFUNCTION, curlmopt_timerfunction_cb_32bit)
        )
      )


    val setopt_socket_cb_ffi = _import "curl_multi_setopt" reentrant: t * int * t -> int;
    val curlmopt_socketfunction_cb_export = _export "curlmopt_socketfunction_cb": (t * int * int * t * t -> int) -> unit;
    val curlmopt_socketfunction_cb        = _address "curlmopt_socketfunction_cb" public: t;

    fun setopt_socket_cb(multi, cb) = (
        curlmopt_socketfunction_cb_export ( fn(easy, socket, poll, _, _) => cb(easy, socket, poll) );
        setopt_socket_cb_ffi(multi, CURLMOPT_SOCKETFUNCTION, curlmopt_socketfunction_cb)
      )

    val add_handle    = _import "curl_multi_add_handle" reentrant: t * t -> int;
    val remove_handle = _import "curl_multi_remove_handle" reentrant: t * t -> int;


    val socket_action_ffi = _import "curl_multi_socket_action" reentrant: t * int * int * t -> int;

    val running_handles_mem  = malloc(4)

    fun socket_action(multi, socket, ev_bitmask) =
      let
        val _ = setInt32(running_handles_mem, 0, 0)
        val _ = socket_action_ffi(multi, socket, ev_bitmask, running_handles_mem)
      in
        getInt32(running_handles_mem, 0)
      end


    val info_read_ffi = _import "curl_multi_info_read": t * t -> t;

    val msgs_in_queue_mem = malloc(4)

    fun read_msg p =
      let
        val msg    = getInt32(p, 0)   val p = add(p, sizeofPointer) (* not 0w4 because alignment *)
        val easy   = getPointer(p, 0) val p = add(p, sizeofPointer)
        val result = getInt32(p, 0)
    in
      (msg, easy, result)
    end

    fun info_read(multi) =
      let
        val _ = setInt32(msgs_in_queue_mem, 0, 0)
        val msg_pointer = info_read_ffi(multi, msgs_in_queue_mem)
      in
        if msg_pointer = null
        then NONE
        else SOME (read_msg msg_pointer)
      end

  end

end
