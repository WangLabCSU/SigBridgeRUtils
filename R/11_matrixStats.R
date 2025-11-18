#' @title Matrix Statistics Functions (Not Necessarily)
#' @name matrix-stats
#' @description
#' A collection of functions for computing matrix statistics with fallback
#' implementations when specialized packages are not available. These functions
#' provide efficient row-wise and column-wise computations for large matrices.
NULL

#' @rdname matrix-stats
#' @description
#' rowVars computes the variance for each row of a numeric matrix.
#' Uses matrixStats::rowVars if available, otherwise provides a base R implementation.
#'
#' @param x A numeric matrix or array. For row* functions, rows represent
#' observations and columns represent variables.
#' @param na.rm Logical indicating whether to remove missing values.
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric vector of length nrow(x) containing row variances.
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#'
#' # Compute row variances
#' row_vars <- rowVars(mat)
#'
#' # With missing values
#' mat[1, 1] <- NA
#' row_vars_na <- rowVars(mat, na.rm = TRUE)
#'
#' @seealso [matrixStats::rowVars()] for the underlying implementation
#' @export
rowVars <- function(x, na.rm = FALSE, ...) {
    if (rlang::is_installed("matrixStats")) {
        return(getExportedValue("matrixStats", "rowVars")(
            x,
            na.rm = na.rm,
            ...
        ))
    }

    if (na.rm) {
        n <- rowSums(!is.na(x))
        rowSums((x - rowMeans(x, na.rm = TRUE))^2, na.rm = TRUE) / (n - 1)
    } else {
        rowSums((x - rowMeans(x))^2) / (ncol(x) - 1)
    }
}

#' @rdname matrix-stats
#' @description
#' colVars computes the variance for each column of a numeric matrix.
#' Uses matrixStats::colVars if available, otherwise provides a base R implementation.
#'
#'
#' @return A numeric vector of length ncol(x) containing column variances.
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#'
#' # Compute column variances
#' col_vars <- colVars(mat)
#'
#' # With missing values
#' mat[1, 1] <- NA
#' col_vars_na <- colVars(mat, na.rm = TRUE)
#'
#' @seealso [matrixStats::colVars()] for the underlying implementation
#' @export
colVars <- function(x, na.rm = FALSE, ...) {
    if (rlang::is_installed("matrixStats")) {
        return(getExportedValue("matrixStats", "colVars")(
            x,
            na.rm = na.rm,
            ...
        ))
    }
    if (na.rm) {
        n <- colSums(!is.na(x))
        colSums((x - colMeans(x, na.rm = TRUE))^2, na.rm = TRUE) / (n - 1)
    } else {
        colSums((x - colMeans(x))^2) / (nrow(x) - 1)
    }
}

#' @rdname matrix-stats
#' @description
#' rowSds computes the standard deviation for each row of a numeric matrix.
#' Uses matrixStats::rowSds if available, otherwise computes as the square root of row variances.
#'
#'
#' @return A numeric vector of length nrow(x) containing row standard deviations.
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_sds <- rowSds(mat)
#'
#' @seealso [matrixStats::rowSds()] for the underlying implementation
#' @export
rowSds <- function(x, na.rm = FALSE, ...) {
    if (rlang::is_installed("matrixStats")) {
        return(getExportedValue("matrixStats", "rowSds")(x, na.rm = na.rm, ...))
    }
    sqrt(rowVars(x, na.rm = na.rm, ...))
}

#' @rdname matrix-stats
#' @description
#' colSds computes the standard deviation for each column of a numeric matrix.
#' Uses matrixStats::colSds if available, otherwise computes as the square root of column variances.
#'
#' @return A numeric vector of length ncol(x) containing column standard deviations.
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' col_sds <- colSds(mat)
#'
#' @seealso [matrixStats::colSds()] for the underlying implementation
#' @export
colSds <- function(x, na.rm = FALSE, ...) {
    if (rlang::is_installed("matrixStats")) {
        return(getExportedValue("matrixStats", "colSds")(x, na.rm = na.rm))
    }
    sqrt(colVars(x, na.rm = na.rm, ...))
}

#' @rdname matrix-stats
#' @description
#' colQuantiles computes quantiles for each column of a numeric matrix.
#' Uses matrixStats::colQuantiles if available, otherwise uses base R quantile function.
#'
#' @param probs Numeric vector of probabilities with values between 0 and 1.
#' @param ... Additional arguments passed to quantile.
#'
#' @return A matrix of quantiles with length(probs) rows and ncol(x) columns.
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#'
#' # Compute quartiles for each column
#' quartiles <- colQuantiles(mat)
#'
#' # Compute specific quantiles
#' specific_quantiles <- colQuantiles(mat, probs = c(0.1, 0.5, 0.9))
#'
#' @seealso [matrixStats::colQuantiles()] for the underlying implementation
#' @export
colQuantiles <- function(x, probs = seq(0, 1, 0.25), ...) {
    if (rlang::is_installed("matrixStats")) {
        return(getExportedValue("matrixStats", "colQuantiles")(
            x,
            probs = probs,
            ...
        ))
    }
    stats::quantile(x, probs = probs, ...)
}
