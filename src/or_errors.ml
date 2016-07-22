module Std =
struct

  module Monad = Monad
  module Error = Error
  module Result = Result
  module Or_error = Or_error
  module Error_converters = Error_converters

  module type MONAD = Monad.S
  module type ERROR = Error.S
  module type RESULT = Result.S
  module type OR_ERROR = Or_error.S
  module type ERROR_CONVERTER = Error_converters.S
end
