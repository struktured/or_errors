module type S =
sig
  type ('a, 'b) t
  val (>>=) : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val (>|=) : ('a, 'd) t -> ('a -> 'b) ->  ('b, 'd) t
  val (>>|) : ('a, 'd) t -> ('a -> 'b) -> ('b, 'd) t
  module Monad_infix :
  sig
    val (>>|) : ('a, 'd) t -> ('a -> 'b) -> ('b, 'd) t
    val (>|=) : ('a, 'd) t -> ('a -> 'b) ->  ('b, 'd) t
    val (>>=) : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  end
  val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'd) t -> f:('a -> 'b) -> ('b, 'd) t
  val join : (('a, 'd) t, 'd) t -> ('a, 'd) t
  val ignore : ('a, 'd) t -> (unit, 'd) t
  val all : ('a, 'd) t list -> ('a list, 'd) t
  val both : ('a, 'd) t -> ('b, 'd) t -> (('a * 'b), 'd) t
  val all_ignore : (unit, 'd) t list -> (unit, 'd) t
  val map_error : ('ok, 'error1) t -> f:('error1 -> 'error2) -> ('ok, 'error2) t
end

module Showable =
struct
  module type RESULT = S
  module type S = sig
  include RESULT
  val show :
      (Format.formatter -> 'a -> unit) ->
      (Format.formatter -> 'b -> unit) ->
      ('a, 'b) t -> (string, string) t
  val pp :
      (Format.formatter -> 'a -> unit) ->
      (Format.formatter -> 'b -> unit) ->
      Format.formatter -> ('a, 'b) t ->
      (unit, unit) t
  end

  module Make(Result: RESULT) : S with type ('a, 'b) t = ('a, 'b) Result.t =
  struct
    include Result
    let pp a_ft b_ft ft (t:('a,'b) t) : (unit,unit) t=
      map ~f:(a_ft ft) t |> map_error ~f:(b_ft ft)

    let show a_ft b_ft t : (string, string) t=
      Format.flush_str_formatter() |> fun _ ->
      pp a_ft b_ft Format.str_formatter t |>
      map ~f:Format.flush_str_formatter |>
      map_error ~f:Format.flush_str_formatter
  end
end
