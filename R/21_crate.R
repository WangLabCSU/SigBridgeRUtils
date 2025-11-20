#' @title Create Self-Contained Functions
#' @description
#' Creates a self-contained function that contains all necessary dependencies.
#' This is useful for parallel processing where functions need to be serialized
#' and sent to different workers.
#'
#' @inheritParams carrier::crate
#' @param ... Objects to include in the crate
#'
#' @return A crated function with all dependencies included
#'
#' @examples
#' \dontrun{
#' # Create a function with dependencies
#' data <- 1:10
#' crated_fn <- crate(function(x) x + mean(data), data = data)
#' crated_fn(5)
#' }
#'
#' @seealso [carrier::crate()]
#' @export
crate <- function(
    .fn,
    ...,
    .parent_env = baseenv(),
    .error_arg = ".fn",
    .error_call = environment()
) {
    rlang::check_installed("carrier")
    getExportedValue("carrier", "crate")(
        .fn = .fn,
        ...,
        .parent_env = .parent_env,
        .error_arg = .error_arg,
        .error_call = .error_call
    )
}
