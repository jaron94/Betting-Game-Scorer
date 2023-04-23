Rank <- function(d) {
  j <- unique(rev(sort(d)))
  return(sapply(d, function(dd) which(dd == j)))
}

shifter <- function(x, n = 1) {
  mod_n <- n %% length(x)
  if (mod_n == 0) x else c(utils::tail(x, -mod_n), utils::head(x, mod_n))
}

calc_points <- function(bid, tricks) {
  tricks +
    if (bid == tricks) 10 else 0
}
