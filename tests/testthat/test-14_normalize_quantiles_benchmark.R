# test_that("normalize quantiles", {
#   set.seed(1234L)
#   n_row <- 1000
#   n_col <- 1000
#   mat <- Matrix::Matrix(rpois(n_row * n_col, 1000), n_row)

#   b <- microbenchmark::microbenchmark(
#     limma = limma::normalizeQuantiles(mat),
#     # preprocessCore can't accept S4Matrix
#     preprocessCore = preprocessCore::normalize.quantiles(as.matrix(mat)),
#     times = 3
#   )

#   # ！limma is slower
#   plot(b)
# })
