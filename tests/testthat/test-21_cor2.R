test_that("It works correctly", {
  mat1 = matrix(rnbinom(10000, 1000, 0.5), 100)
  mat2 = matrix(rnbinom(10000, 1000, 0.8), 100)

  Mat1 <- Matrix::Matrix(mat1)
  Mat2 <- Matrix::Matrix(mat2)

  bench <- microbenchmark::microbenchmark(
    base = {
      # In real usage, there are some transformations
      temp_Mat1 <- as.matrix(Mat1)
      temp_Mat2 <- as.matrix(Mat2)
      res1 <- cor(temp_Mat1, temp_Mat2)
    },
    my = {
      temp_Mat1 <- as.matrix(Mat1)
      temp_Mat2 <- as.matrix(Mat2)
      res2 <- cor2(Mat1, Mat2)
    }
  )

  print(bench)

  tol <- max(abs(res1 - res2))

  cli::cli_alert_info("Max difference: {.val {tol}}")

  expect_lt(tol, 1e-10)
})

test_that("It works better", {
  data <- readRDS(
    "~/R/Project/R_code/SigBridgeR/vignettes/example_data/survival_example_data.rds"
  )

  seurat <- qs::qread("/home/data/sigbridger/scpas_result.qs")$scRNA_data

  sc_expr <- SeuratObject::LayerData(seurat) |> as.matrix()

  bulk <- data[[2]]
  bulk <- as.matrix(bulk)

  cm_gene <- intersect(rownames(sc_expr), rownames(bulk))
  sc_expr <- sc_expr[cm_gene, ]
  bulk <- bulk[cm_gene, ]

  bench2 <- microbenchmark::microbenchmark(
    base = {
      # In real usage, there are some transformations
      temp_Mat1 <- as.matrix(bulk)
      temp_Mat2 <- as.matrix(sc_expr)
      res1 <- cor(temp_Mat1, temp_Mat2)
    },
    my = {
      temp_Mat1 <- as.matrix(bulk)
      temp_Mat2 <- as.matrix(sc_expr)
      res2 <- cor2(temp_Mat1, temp_Mat2)
    }
  )

  print(bench2)

  tol <- max(abs(res1 - res2))

  cli::cli_alert_info("Max difference in real data: {.val {tol}}")

  expect_lt(tol, 1e-10)
})
