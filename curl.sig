signature CURL =
sig
  exception Curl of int
  val withCurl : (unit -> unit) -> unit
  val version : unit -> string

  structure Const : CURL_CONST

  structure Easy :
  sig
    type curl
    val init       : unit -> curl
    val setopt_str : curl * int * string -> int
    val setopt_int : curl * int * int -> int
    val setopt_cb  : curl * int * (string -> int) -> int
    val setopt_list : curl * int * string list -> (unit -> unit)
    val perform    : curl -> int
    val cleanup    : curl -> unit
    val getinfo_str : curl * int -> string
    val strerror   : int -> string
  end

  structure Multi :
  sig
    type multi
    type easy
    val easy2int : easy -> LargeInt.int
    val init      : unit -> multi
    val cleanup   : multi -> int
    val setopt_timer_cb  : multi * (multi * int -> int) -> int
    val setopt_socket_cb : multi * (easy *  int * int -> int) -> int

    val add_handle    : multi * easy -> int
    val remove_handle : multi * easy -> int
    val socket_action : multi * int * int -> int
    val info_read     : multi -> (int * easy * int) option
  end
end
