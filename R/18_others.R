#' @title Check all the Objects Are Identical
#'
#' @description
#' all_identical checks if all provided objects are pairwise identical and returns
#' a symmetric matrix showing the comparison results between all object pairs.
#'
#' @param ... Objects to compare for identity
#' @param names Optional character vector of names for the objects. If not provided,
#' objects will be named as "Obj1", "Obj2", etc.
#'
#' @return A symmetric logical matrix where entry \[i, j\] indicates whether
#' object i is identical to object j. The matrix has dimensions n x n where
#' n is the number of objects, with row and column names from the names parameter.
#'
#' @details
#' This function provides a comprehensive way to compare multiple objects at once,
#' returning a matrix that shows all pairwise comparisons. This is particularly
#' useful for testing and validation scenarios where you need to verify that
#' multiple objects are identical.
#'
#' @examples
#' # Compare identical objects
#' x <- 1:5
#' y <- 1:5
#' z <- 1:5
#'
#' # All objects are identical
#' result <- all_identical(x, y, z)
#' print(result)
#'
#' # Compare different objects with custom names
#' a <- 1:3
#' b <- 1:5
#' c <- 1:3
#'
#' result2 <- all_identical(a, b, c, names = c("first", "second", "third"))
#' print(result2)
#'
#' # Single object case
#' single_result <- all_identical(x)
#' print(single_result)
#'
#' @seealso
#' [identical()] for pairwise object comparison,
#' [matrix()] for creating matrices
#' @export
all_identical <- function(..., names = NULL) {
    objs <- list(...)
    n <- length(objs)

    if (is.null(names)) {
        names <- paste0("Obj", seq_len(n))
    } else if (length(names) != n) {
        cli::cli_abort(c(
            "x" = "The length of `names` must be equal to the number of objects."
        ))
    }

    result <- matrix(TRUE, n, n, dimnames = list(names, names))

    if (n > 1) {
        for (i in seq_len(n - 1)) {
            for (j in (i + 1):n) {
                is_identical <- identical(objs[[i]], objs[[j]])
                result[i, j] <- is_identical
                result[j, i] <- is_identical # 对称矩阵
            }
        }
    }

    result
}

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
