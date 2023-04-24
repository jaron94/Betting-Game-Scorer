shifter <- function(x, n = 1) {
  stopifnot(is.numeric(n), length(n) == 1L)
  mod_n <- n %% length(x)
  if (mod_n == 0) x else c(utils::tail(x, -mod_n), utils::head(x, mod_n))
}


calc_points <- function(bid, tricks) {
  tricks + (bid == tricks) * 10
}
