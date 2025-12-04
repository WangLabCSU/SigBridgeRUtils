#' @title Matrix Statistics Functions (Not Necessarily)
#' @name matrix-stats
#' @description
#' A collection of functions for computing matrix statistics with fallback
#' implementations when specialized packages are not available. These functions
#' provide efficient row-wise and column-wise computations for large matrices.
NULL


#' @rdname matrix-stats
#' @description
#' rowMeans2 computes the mean value for each row of a numeric matrix.
#' Uses matrixStats::rowVars if available, otherwise provides a base R implementation.
#'
#' @param x A numeric matrix or array. For row* functions, rows represent
#' observations and columns represent variables.
#' @param na.rm Logical indicating whether to remove missing values.
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric vector of length nrow(x) containing row variances.
#'
#'
#' @export
rowMeans2 <- function(x, na.rm = FALSE, ...) {
    if (
        rlang::is_installed('sparseMatrixStats') && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue('sparseMatrixStats', 'rowMeans2'))(
            x = x,
            na.rm = na.rm,
            ...
        )
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "rowMeans2")(
            x = x,
            na.rm = na.rm,
            ...
        ))
    }
    if (rlang::is_installed("Matrix") && inherits(x, "denseMatrix")) {
        return(getExportedValue("Matrix", "rowMeans"))(
            x = x,
            na.rm = na.rm,
            ...
        )
    }
    rowMeans(x, na.rm = na.rm, ...)
}

#' @rdname matrix-stats
#' @description
#' colMeans2 computes the mean value for each column of a numeric matrix.
#' Uses matrixStats::rowVars if available, otherwise provides a base R implementation.
#'
#' @param x A numeric matrix or array. For row* functions, rows represent
#' observations and columns represent variables.
#' @param na.rm Logical indicating whether to remove missing values.
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric vector of length nrow(x) containing row variances.
#'
#'
#' @export
colMeans2 <- function(x, na.rm = FALSE, ...) {
    if (
        rlang::is_installed('sparseMatrixStats') && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue('sparseMatrixStats', 'colMeans2'))(
            x = x,
            na.rm = na.rm,
            ...
        )
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "colMeans2")(
            x = x,
            na.rm = na.rm,
            ...
        ))
    }
    if (rlang::is_installed("Matrix") && inherits(x, "denseMatrix")) {
        return(getExportedValue("Matrix", "colMeans"))(
            x = x,
            na.rm = na.rm,
            ...
        )
    }
    colMeans(x, na.rm = na.rm, ...)
}

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
    if (
        rlang::is_installed('sparseMatrixStats') && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue('sparseMatrixStats', 'rowVars'))(
            x = x,
            na.rm = na.rm,
            ...
        )
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "rowVars")(
            x = x,
            na.rm = na.rm,
            ...
        ))
    }
    if (rlang::is_installed("Matrix") && inherits(x, "denseMatrix")) {
        # mask base R functions
        rowSums <- getExportedValue("Matrix", "rowSums")
        rowMeans <- getExportedValue("Matrix", "rowMeans")
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
#' @export
colVars <- function(x, na.rm = FALSE, ...) {
    if (
        rlang::is_installed('sparseMatrixStats') && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue('sparseMatrixStats', 'colVars'))(
            x = x,
            na.rm = na.rm,
            ...
        )
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "colVars")(
            x = x,
            na.rm = na.rm,
            ...
        ))
    }

    if (rlang::is_installed("Matrix") && inherits(x, "denseMatrix")) {
        # mask base R functions
        colSums <- getExportedValue("Matrix", "colSums")
        colMeans <- getExportedValue("Matrix", "colMeans")
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
#' @export
rowSds <- function(x, na.rm = FALSE, ...) {
    if (
        rlang::is_installed("sparseMatrixStats") && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue("sparseMatrixStats", "rowSds")(
            x = x,
            na.rm = na.rm,
            ...
        ))
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "rowSds")(
            x = x,
            na.rm = na.rm,
            ...
        ))
    }
    sqrt(rowVars(x = x, na.rm = na.rm, ...))
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
    if (
        rlang::is_installed('sparseMatrixStats') && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue('sparseMatrixStats', 'colSds')(
            x = x,
            na.rm = na.rm,
            ...
        ))
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "colSds")(
            x = x,
            na.rm = na.rm,
            ...
        ))
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
#' @export
colQuantiles <- function(x, probs = seq(0, 1, 0.25), ...) {
    if (
        rlang::is_installed('sparseMatrixStats') && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue('sparseMatrixStats', 'colQuantiles')(
            x = x,
            probs = probs,
            ...
        ))
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "colQuantiles")(
            x = x,
            probs = probs,
            ...
        ))
    }
    # denseMatrix is Okay
    stats::quantile(x, probs = probs, ...)
}

#' @rdname matrix-stats
#' @description
#' rowMaxs computes the maximum value for each row of a numeric matrix.
#' Uses matrixStats::rowMaxs or sparseMatrixStats::rowMaxs if available,
#' otherwise provides a base R implementation.
#'
#' @param x A numeric matrix or array
#' @param rows,cols Indices specifying subset of rows/columns to operate over
#' @param na.rm Logical indicating whether to remove missing values
#' @param dim. Dimensions of the input matrix
#' @param useNames Logical indicating whether to preserve row names in output
#' @param ... Additional arguments passed to methods
#'
#' @return A numeric vector of length nrow(x) containing row maximums
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#'
#' # Compute row maximums
#' row_maxs <- rowMaxs(mat)
#'
#' # With missing values
#' mat[1, 1] <- NA
#' row_maxs_na <- rowMaxs(mat, na.rm = TRUE)
#'
#' @export
rowMaxs <- function(
    x,
    rows = NULL,
    cols = NULL,
    na.rm = FALSE,
    dim. = dim(x),
    ...,
    useNames = TRUE
) {
    if (
        rlang::is_installed('sparseMatrixStats') && inherits(x, 'sparseMatrix')
    ) {
        return(getExportedValue("sparseMatrixStats", "rowMaxs")(
            x = x,
            rows = rows,
            cols = cols,
            na.rm = na.rm,
            ...,
            useNames = useNames
        ))
    }
    if (rlang::is_installed("matrixStats") && !isS4(x)) {
        return(getExportedValue("matrixStats", "rowMaxs")(
            x = x,
            rows = rows,
            cols = cols,
            na.rm = na.rm,
            dim. = dim.,
            ...,
            useNames = useNames
        ))
    }
    if (rlang::is_installed("Matrix") && inherits(x, "denseMatrix")) {
        # mask base R functions
        rowSums <- getExportedValue("Matrix", "rowSums")
    }

    if (!is.null(rows) || !is.null(cols)) {
        rows <- rows %||% seq_len(nrow(x))
        cols <- cols %||% seq_len(ncol(x))
        x <- x[rows, cols, drop = FALSE]
    }

    nr <- nrow(x)
    nc <- ncol(x)

    if (nc == 0) {
        result <- rep(NA_real_, nr)
    } else if (nr == 0) {
        result <- numeric(0)
    } else if (nc == 1) {
        result <- as.numeric(x[, 1])
    } else {
        row_has_na <- rowSums(is.na(x)) > 0

        if (!any(row_has_na)) {
            # 无NA：直接用max.col()
            idx <- max.col(x, ties.method = "first")
            result <- x[cbind(seq_len(nr), idx)]
        } else if (all(row_has_na) && !na.rm) {
            # 全是NA且不移除：全返回NA
            result <- rep(NA_real_, nr)
        } else {
            # 混合情况
            result <- numeric(nr)

            # 无NA的行
            clean_rows <- !row_has_na
            if (any(clean_rows)) {
                idx <- max.col(
                    x[clean_rows, , drop = FALSE],
                    ties.method = "first"
                )
                result[clean_rows] <- x[clean_rows, , drop = FALSE][
                    seq_len(cbind(sum(clean_rows))),
                    idx
                ]
            }

            # 有NA的行
            if (any(row_has_na)) {
                if (na.rm) {
                    # 移除NA后计算
                    result[row_has_na] <- apply(
                        x[row_has_na, , drop = FALSE],
                        1,
                        max,
                        na.rm = TRUE
                    )
                } else {
                    # 保留NA
                    result[row_has_na] <- NA_real_
                }
            }
        }

        # 保留行名
        if (useNames && !is.null(rownames(x))) {
            names(result) <- rownames(x)
        }

        result
    }
}
