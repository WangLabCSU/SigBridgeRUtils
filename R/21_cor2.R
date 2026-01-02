#' A substitute for cor() supporting sparse matrices
#'
#' @param X base matrix
#' @param Y base matrix, defaults to X
#' @param use missing value handling: "everything" (default) or "na.or.complete"
#' @param center logical: center columns (default TRUE)
#'
#' @return p x q matrix of correlations
cor2 <- function(
  X,
  Y = NULL,
  use = "everything",
  center = TRUE
) {
  if (!is.matrix(X)) {
    cli::cli_warn("[{.fun cor2}]: X is not a base matrix, coerced to matrix.")
    X <- as.matrix(X)
  }
  if (is.null(Y)) {
    Y <- X
  } else if (!is.matrix(Y)) {
    cli::cli_warn("[{.fun cor2}]: Y is not a base matrix, coerced to matrix.")
    Y <- as.matrix(Y)
  }
  stopifnot(nrow(X) == nrow(Y))

  n <- nrow(X)

  if (use == "na.or.complete") {
    # Identify complete cases
    x_na <- matrixStats::rowSums2(is.na(X)) > 0
    y_na <- matrixStats::rowSums2(is.na(Y)) > 0

    keep <- !(x_na | y_na)
    X <- X[keep, , drop = FALSE]
    Y <- Y[keep, , drop = FALSE]
    n <- sum(keep)
  } else if (use != "everything") {
    cli::cli_abort("Only 'everything' supported now")
  }

  if (center) {
    mx <- matrixStats::colMeans2(X, na.rm = TRUE)
    my <- matrixStats::colMeans2(Y, na.rm = TRUE)
    X <- X - matrix(rep(mx, each = n), nrow = n)
    Y <- Y - matrix(rep(my, each = n), nrow = n)
  }

  # 协方差
  cov_xy <- if (rlang::is_installed("Rfast") && n > 5000) {
    Rfast::Crossprod(X, Y) / (n - 1L)
  } else {
    crossprod(X, Y) / (n - 1L)
  }

  # 标准差（用 colSds2 支持 NA）
  sx <- matrixStats::colSds(X, na.rm = TRUE)
  sy <- matrixStats::colSds(Y, na.rm = TRUE)

  # Pearson correlation
  cor_mat <- if (rlang::is_installed("Rfast") && n > 5000) {
    cov_xy / Rfast::Tcrossprod(sx, sy)
  } else {
    cov_xy / tcrossprod(sx, sy)
  }

  dimnames(cor_mat) <- list(colnames(X), colnames(Y))

  cor_mat
}
