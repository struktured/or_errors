module Error = Or_errors_error
module Result = Or_errors_result
module type S =
sig
  module Error : Error.S
  module Result : Result.S
  type 'a t
  include Or_errors_monad.S with type 'a t := 'a t
  val fail : Error.t -> 'a t
end

module Showable =
struct
  module type OR_ERROR = S
  module type S =
  sig
    module Result : Result.S
    include OR_ERROR with module Result := Result
  end
  module Make
    (Result : Result.S)
    (Error : Error.S)
    (Fail: sig
       type 'a t = ('a, Error.t) Result.t
       val fail : Error.t -> 'a t end) :
    S with
      module Result = Result and
      module Error = Error and
      type 'a t = ('a, Error.t) Result.t =
  struct
      type 'a t = ('a, Error.t) Result.t
      module Error = Error
      module Result = Result
      let bind = Result.bind
      let return = Result.return
      let fail = Fail.fail
      let map = Result.map
      let all = Result.all
      let all_ignore = Result.all_ignore
      let both = Result.both
      let ignore = Result.ignore
      let join = Result.join
      module Monad_infix = Result.Monad_infix
      include Monad_infix
      let of_result t = t

      let pp (a_formatter:Format.formatter -> 'a -> unit) formatter =
        map ~f:(fun a -> a_formatter formatter a)
      let show a_f t =
        Format.flush_str_formatter() |> fun (_:string) ->
       pp a_f Format.str_formatter t |> map ~f:Format.flush_str_formatter
  end
end
