# Project memory

- R package `SigBridgeRUtils`; C++ sources are in `src/`, tests in `tests/testthat/`.
- Vendored Eigen core headers are at `inst/include/eigen/Eigen`; compile with `-I../inst/include/eigen` in both `src/Makevars` and `src/Makevars.win`.
- `src/matrix_stats.cpp` uses `<Eigen/Dense>` and beachmat/tatami for ordinary, sparse, and DelayedArray matrices.
- Its OpenMP loop must create a separate `dense_row()`/`dense_column()` accessor per thread; sharing an accessor causes nondeterministic sparse-matrix statistics.
