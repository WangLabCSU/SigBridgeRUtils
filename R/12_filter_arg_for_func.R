#' @title Keep Wanted Arguments According to A Function from Dots
#' @description
#' FilterArgs4Func filters a list of arguments to include only those that
#' match the formal arguments of a specified function. This is useful for
#' preparing argument lists for function calls, especially when dealing with
#' functions that have many optional parameters.
#'
#' @param args_list A named list of arguments to filter
#' @param fun The target function whose formal arguments will be used for filtering
#'
#' @return A filtered list containing only the arguments from args_list that
#' match the formal parameter names of fun (excluding the "..." parameter).
#'
#' @details
#' This function is particularly useful in scenarios where you have a large
#' list of parameters and want to pass only the relevant ones to a specific
#' function, avoiding "unused argument" errors.
#'
#' @examples
#' \dontrun{
#' # Example function with specific parameters
#' example_function <- function(a, b, c = 10, d = 20) {
#' return(a + b + c + d)
#' }
#'
#' # Create a list with both relevant and irrelevant arguments
#' all_args <- list(
#' a = 1,
#' b = 2,
#' c = 3,
#' e = 4, # Not in function formals
#' f = 5 # Not in function formals
#' )
#'
#' # Filter to only include arguments that match function parameters
#' filtered_args <- FilterArgs4Func(all_args, example_function)
#' print(filtered_args)
#'
#' # Use do.call to execute the function with filtered arguments
#' result <- do.call(example_function, filtered_args)
#' print(result)
#' }
#'
#' @seealso
#' [formals()] for accessing function formal arguments,
#' [do.call()] for executing functions with argument lists,
#' [names()] for working with list names
#' @export
FilterArgs4Func <- function(args_list, fun) {
    fun_formals <- names(formals(fun))
    fun_formals <- fun_formals[fun_formals != "..."]
    args_list[names(args_list) %in% fun_formals]
}
