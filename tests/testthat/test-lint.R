test_that("Package is lint free", {
  withr::with_envvar(
    c(NOT_CRAN = "true"),
    withr::with_dir(
      testthat::test_path("..", ".."),
      lintr::expect_lint_free()
    )
  )
})
