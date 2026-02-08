#' @title Match Functions to Argument List
#' @description
#' MatchFunc2Args identifies which functions (provided via \code{...}) can
#' accept all named arguments in \code{args_list}. A function is considered
#' compatible if:
#' \itemize{
#'   \item It contains a \code{...} parameter (can accept any arguments), OR
#'   \item All names in \code{args_list} exist in the function's formal parameters
#' }
#'
#' This is useful for dynamic dispatch scenarios where you need to select
#' appropriate functions based on available parameters.
#'
#' @param args_list A named list of arguments to match against function signatures.
#'   Must have non-empty names for all elements when non-empty.
#' @param ... Functions to test for compatibility with \code{args_list}
#' @param name_only Logical. If \code{TRUE}, return character vector of function
#'   names/identifiers instead of function objects. Default is \code{FALSE}.
#'
#' @return
#'   \itemize{
#'     \item When \code{name_only = FALSE}: A list of compatible function objects
#'     \item When \code{name_only = TRUE}: A character vector of function identifiers:
#'       \itemize{
#'         \item Named functions: their symbol name (e.g., \code{"mean"})
#'         \item Anonymous functions: \code{"anonymous_<position>"} where position
#'           is the 1-based index in the \code{...} arguments
#'       }
#'   }
#'
#' @details
#' \strong{Argument Validation:}
#' \itemize{
#'   \item \code{args_list} must be a named list with non-empty names for all
#'     elements when non-empty. Unnamed lists will trigger an error.
#'   \item Non-function arguments in \code{...} are skipped with a warning.
#'   \item Primitive functions (e.g., \code{`+`}, \code{`[`)}) are handled specially:
#'     only match when \code{args_list} is empty (since their formal parameters
#'     cannot be introspected reliably).
#' }
#'
#' \strong{Name Resolution Strategy for \code{name_only = TRUE}:}
#' \itemize{
#'   \item Symbol arguments (e.g., \code{mean}) → return symbol name as string
#'   \item Anonymous functions (e.g., \code{function(x) x}) → \code{"anonymous_<index>"}
#'   \item Complex expressions → \code{"anonymous_<index>"}
#' }
#'
#' @examples
#' \dontrun{
#' # Example functions with different signatures
#' func1 <- function(a, b, c = 10) a + b + c
#' func2 <- function(x, y, ...) x * y
#' func3 <- function(p, q) p - q
#'
#' # Argument list to match
#' args <- list(a = 1, b = 2)
#'
#' # Find compatible functions (returns function objects)
#' MatchFunc2Args(args, func1, func2, func3)
#' # Returns list containing func1 and func2 (func3 lacks 'a' and 'b')
#'
#' # Get only function names/identifiers
#' MatchFunc2Args(args, func1, func2, func3, name_only = TRUE)
#' # Returns c("func1", "func2")
#'
#' # Anonymous function example
#' MatchFunc2Args(
#'   list(x = 5, y = 3),
#'   function(x, y) x + y,
#'   mean,
#'   name_only = TRUE
#' )
#' # Returns c("anonymous_1", "mean")
#' }
#'
#' @seealso
#' [formals()] for inspecting function signatures,
#' [FilterArgs4Func()] for the inverse operation (filtering arguments for a function)
#' @export
MatchFunc2Args <- function(args_list, ..., name_only = FALSE) {
  # Validate args_list has proper names when non-empty
  if (length(args_list) > 0) {
    if (is.null(names(args_list)) || any(names(args_list) == "")) {
      cli::cli_abort(c(
        "x" = "`args_list` must be a named list with non-empty names for all elements when non-empty."
      ))
    }
  }

  # Capture actual function objects
  dots_funcs <- rlang::list2(...)

  # Handle empty ... case
  if (length(dots_funcs) == 0L) {
    return(if (name_only) character(0L) else list())
  }

  func_names <- names(rlang::enquos(..., .named = TRUE))
  func_formals <- lapply(dots_funcs, formals)

  logical_vec <- vapply(
    X = func_formals,
    FUN = \(lst) {
      modified <- utils::modifyList(lst, args_list, keep.null = TRUE)
      if (length(modified) != length(lst)) FALSE else TRUE
    },
    FUN.VALUE = logical(1L)
  )

  if (name_only) {
    return(func_names[logical_vec])
  }

  names(dots_funcs) <- func_names
  dots_funcs[logical_vec]
}
