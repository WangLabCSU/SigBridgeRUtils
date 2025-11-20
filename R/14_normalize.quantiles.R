#' @title Quantile Normalization
#' @description
#' normalize.quantiles performs quantile normalization on a matrix, transforming
#' the distributions of each column to match a common target distribution.
#' Supports both dense matrices and sparse Matrix formats (dgCMatrix, dgRMatrix, etc.)
#' for memory-efficient processing of large datasets.
#'
#' @param x A numeric matrix (dense or sparse Matrix format) where columns
#' represent samples and rows represent features.
#' @param copy Logical indicating whether to work on a copy of the matrix (TRUE)
#' or modify in-place (FALSE). Note: sparse matrices always create a copy.
#' @param keep.names Logical indicating whether to preserve row and column names.
#' @param ... Additional arguments (currently not used).
#'
#' @return A numeric matrix of the same type as input with quantile-normalized data.
#' If input is a sparse Matrix, output will also be sparse.
#'
#' @examples
#' # Dense matrix
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' normalized_mat <- normalize.quantiles(mat)
#'
#' # Sparse matrix for memory efficiency
#' library(Matrix)
#' sparse_mat <- Matrix(mat, sparse = TRUE)
#' normalized_sparse <- normalize.quantiles(sparse_mat)
#'
#' # Large sparse matrix example (single-cell RNA-seq style)
#' large_sparse <- sparseMatrix(
#'     i = sample(1:10000, 50000, replace = TRUE),
#'     j = sample(1:100, 50000, replace = TRUE),
#'     x = rpois(50000, 2),
#'     dims = c(10000, 100)
#' )
#' normalized_large <- normalize.quantiles(large_sparse, keep.names = TRUE)
#'
#' # Preserve original names
#' rownames(mat) <- paste0("Gene", 1:10)
#' colnames(mat) <- paste0("Sample", 1:10)
#' normalized_with_names <- normalize.quantiles(mat, keep.names = TRUE)
#'
#' @seealso
#' \itemize{
#'   \item [preprocessCore::normalize.quantiles()] for the reference implementation
#'   \item [Matrix::Matrix()] for sparse matrix formats
#' }
#' @useDynLib SigBridgeRUtils, .registration = TRUE
#' @export
normalize.quantiles <- function(x, copy = TRUE, keep.names = FALSE, ...) {
    # Validate input
    is_sparse <- inherits(x, "sparseMatrix") || inherits(x, "Matrix")
    is_dense <- is.matrix(x)

    if (!is_dense && !is_sparse) {
        cli::cli_abort(c(
            "x" = "Matrix expected in normalize.quantiles",
            ">" = "Input is a {.field {class(x)}}",
            "i" = "Supported types: matrix, Matrix (sparse)"
        ))
    }

    if (is_sparse) {
        rlang::check_installed("Matrix")
    }
    # Use C++ implementation
    normalize_quantiles_cpp(x, copy = copy, keep_names = keep.names)
}
