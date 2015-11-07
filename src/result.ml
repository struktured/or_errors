module type S =
sig
  type ('a, 'b) t
  val ( >>= ) : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val (>|=) : ('a, 'd) t -> ('a -> 'b) ->  ('b, 'd) t
  val ( >>| ) : ('a, 'd) t -> ('a -> 'b) -> ('b, 'd) t
  module Monad_infix :
  sig
    val (>>|) : ('a, 'd) t -> ('a -> 'b) -> ('b, 'err) t
    val (>|=) : ('a, 'd) t -> ('a -> 'b) ->  ('b, 'err) t
    val (>>=) : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  end
  val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'd) t -> f:('a -> 'b) -> ('b, 'd) t
  val join : (('a, 'd) t, 'd) t -> ('a, 'd) t
  val ignore : ('a, 'd) t -> (unit, 'd) t
  val all : ('a, 'd) t list -> ('a list, 'd) t
  val both : ('a, 'err) t -> ('b, 'err) t -> (('a * 'b), 'err) t
  val all_ignore : (unit, 'd) t list -> (unit, 'd) t
  val map_error : ('ok, 'error1) t -> f:('error1 -> 'error2) -> ('ok, 'error2) t
end
