module type S =
  sig
    type 'a t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    module Monad_infix :
      sig
        val (>>|) : 'a t -> ('a -> 'b) -> 'b t
        val (>|=) : 'a t -> ('a -> 'b) -> 'b t
        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      end
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val join : 'a t t -> 'a t
    val ignore: 'a t -> unit t
    val all : 'a t list -> 'a list t
    val all_ignore : unit t list -> unit t
    val both : 'a t -> 'b t -> ('a * 'b) t
end
