gen_bid_set <- function(round, n_players, valid = TRUE, seed = 42) {
  tot_tricks <- card_seq(round)
  poss_bids <- seq(0, tot_tricks)

  if (valid) {
    bids_base <- withr::with_seed(
      # Ensure seed is different on the way down and back up
      seed + round,
      sample(poss_bids, n_players - 1, replace = TRUE)
    )
    # Valid bid set doesn't sum to total tricks
    # 'last' player has to go 1 if the other bids sum to total tricks
    bids <- c(bids_base, as.logical(sum(bids_base) == tot_tricks))
  } else {
    bids_base <- rep(floor(tot_tricks / n_players), n_players - 1)
    bids <- c(bids_base, tot_tricks - sum(bids_base))
  }

  bids <- as.integer(bids)

  if (!all(bids %in% poss_bids)) {
    stop("Impossible bids generated in test code")
  }

  bids
}


gen_trick_set <- function(round, n_players, valid = TRUE, seed = 42) {
  tot_tricks <- card_seq(round)
  poss_tricks <- seq(0, tot_tricks)

  if (valid) {
    tricks_base <- rep(floor(tot_tricks / n_players), n_players - 1)
    tricks <- c(tricks_base, tot_tricks - sum(tricks_base))
  } else {
    tricks_base <- withr::with_seed(
      # Ensure seed is different on the way down and back up
      # (and different to `sim_bids()`)
      (seed + round) * 2,
      sample(poss_tricks, n_players - 1, replace = TRUE)
    )
    # Invalid trick set doesn't sum to total tricks
    tricks <- c(tricks_base, as.logical(sum(tricks_base) == tot_tricks))
  }

  tricks <- as.integer(tricks)

  if (!all(tricks %in% poss_tricks)) {
    stop("Impossible tricks generated in test code")
  }
}


rand_game <- function(
    up_to_round, players = c("P1", "P2", "P3"), bid_stage = TRUE, seed = 42) {
  game <- Game$new("rand_game")
  game$add_players(purrr::map(players, Player$new))
  n_players <- game$num_players()
  if (up_to_round == 1) {
    return(game)
  }
  for (i in seq_len(up_to_round - 1)) {
    game$record_bids(gen_bid_set(game$get_round(), n_players, seed = seed))
    game$record_tricks(gen_trick_set(game$get_round(), n_players, seed = seed))
  }
  if (!bid_stage) {
    game$record_bids(gen_bid_set(game$get_round(), n_players, seed = seed))
  }
  game
}
