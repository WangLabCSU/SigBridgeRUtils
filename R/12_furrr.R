#' @title Future plan for furrr
#' @export
#' @inheritParams future::plan
#' @importFrom future multisession multicore cluster
#'
plan <- function(
    strategy = NULL,
    ...,
    substitute = TRUE,
    .skip = FALSE,
    .call = TRUE,
    .cleanup = NA,
    .init = TRUE
) {
    rlang::check_installed("future")
    getExportedValue("future", "plan")(
        strategy = strategy,
        ...,
        substitute = substitute,
        .skip = .skip,
        .call = .call,
        .cleanup = .cleanup,
        .init = .init
    )
}

#' @title Parallel Processing Utilities
#' @name parallel-utils
#' @description
#' Functions for querying available computational resources in parallel processing environments.
#' These functions provide information about available CPU cores and workers for
#' configuring parallel execution in the SigBridgeR package.
NULL

#' @rdname parallel-utils
#' @description
#' availableCores returns the number of available CPU cores for parallel processing.
#' This function detects various system settings and environment variables that
#' may limit the number of cores available for computation.
#'
#'
#' @return An integer specifying the number of available CPU cores.
#'
#' @details
#' The function considers several factors when determining available cores:
#' - System configuration (physical and logical cores)
#' - Environment variables (e.g., R_FUTURE_AVAILABLECORES_FALLBACK)
#' - R options (e.g., future.availableCores.fallback)
#' - Container and cloud environment limitations
#'
#' @examples
#' \dontrun{
#' # Get number of available cores
#' n_cores <- availableCores()
#' print(n_cores)
#'
#' # Use in parallel configuration
#' plan("multisession", workers = availableCores() - 1)
#' }
#'
#' @seealso
#' [future::availableCores()] for the underlying implementation,
#' [availableWorkers()] for getting available worker identifiers
#' @export
availableCores <- function() {
    rlang::check_installed("future")
    getExportedValue("future", "availableCores")()
}

#' @rdname parallel-utils
#' @description
#' availableWorkers returns a character vector of available worker identifiers
#' for parallel processing. This is useful for setting up cluster configurations
#' and distributed computing.
#'
#'
#' @return A character vector of available worker identifiers. The specific
#' format depends on the parallel backend and system configuration.
#'
#' @details
#' The function detects available workers based on:
#' - System hostnames for multi-machine clusters
#' - Local machine core count for single-machine parallelization
#' - Environment-specific worker configurations
#' - Cloud and HPC environment settings
#'
#' @examples
#' \dontrun{
#' # Get available workers
#' workers <- availableWorkers()
#' print(workers)
#'
#' # Use in cluster configuration
#' plan("cluster", workers = availableWorkers())
#' }
#'
#' @seealso
#' [future::availableWorkers()] for the underlying implementation,
#' [availableCores()] for getting the number of available cores
#' @export
availableWorkers <- function() {
    rlang::check_installed("future")
    getExportedValue("future", "availableWorkers")()
}


#' @title Future map for furrr
#' @export
#' @inheritParams furrr::future_map_dbl
future_map_dbl <- function(
    .x,
    .f,
    ...,
    .options = furrr_options(),
    .env_globals = parent.frame(),
    .progress = FALSE
) {
    rlang::check_installed("furrr")
    getExportedValue("furrr", "future_map_dbl")(
        .x = .x,
        .f = .f,
        ...,
        .options = .options,
        .env_globals = .env_globals,
        .progress = .progress
    )
}

#' @title Future map for furrr
#' @export
#' @inheritParams furrr::future_map
future_map <- function(
    .x,
    .f,
    ...,
    .options = furrr_options(),
    .env_globals = parent.frame(),
    .progress = FALSE
) {
    rlang::check_installed("furrr")
    getExportedValue("furrr", "future_map")(
        .x = .x,
        .f = .f,
        ...,
        .options = .options,
        .env_globals = .env_globals,
        .progress = .progress
    )
}


#' @title Future options for furrr
#' @export
#' @inheritParams furrr::furrr_options
furrr_options <- function(
    ...,
    stdout = TRUE,
    conditions = "condition",
    globals = TRUE,
    packages = NULL,
    seed = FALSE,
    scheduling = 1,
    chunk_size = NULL,
    prefix = NULL
) {
    rlang::check_installed("furrr")
    getExportedValue("furrr", "furrr_options")(
        ...,
        stdout = stdout,
        conditions = conditions,
        globals = globals,
        packages = packages,
        seed = seed,
        scheduling = scheduling,
        chunk_size = chunk_size,
        prefix = prefix
    )
}
