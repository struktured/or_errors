module Std =
struct

  module Monad = Or_errors_monad
  module Error = Or_errors_error
  module Result = Or_errors_result
  module Or_error = Or_errors_or_error
  module Error_converters = Or_errors_error_converters

  module type MONAD = Monad.S
  module type ERROR = Error.S
  module type RESULT = Result.S
  module type OR_ERROR = Or_error.S
  module type ERROR_CONVERTER = Error_converters.S
end
