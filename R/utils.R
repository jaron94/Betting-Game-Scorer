shifter <- function(x, n = 1) {
  stopifnot(is.numeric(n), length(n) == 1L)
  mod_n <- n %% length(x)
  if (mod_n == 0) x else c(utils::tail(x, -mod_n), utils::head(x, mod_n))
}


calc_points <- function(bid, tricks) {
  tricks + (bid == tricks) * 10
}

# Function to determine the number of cards dealt in the round
card_seq <- function(round, max_cards = 7) {
  c(max_cards:1, 2:max_cards)[round]
}

tot_rounds <- function(max_cards = 7) {
  length(c(max_cards:1, 2:max_cards))
}
