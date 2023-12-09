expect_round <- function(game, round) {
  testthat::expect_identical(game$get_round(), round)
}

expect_order <- function(game, order) {
  testthat::expect_identical(game$get_order(), order)
}

play_round <- function(game, exp_order, exp_round, bids, tricks) {
  expect_order(game, exp_order)
  expect_round(game, exp_round)
  testthat::expect_no_error(game$record_bids(bids))
  testthat::expect_no_error(game$output_table())
  testthat::expect_no_error(game$record_tricks(tricks))
  testthat::expect_no_error(game$output_table())
}

test_that("'Game' class works", {
  withr::local_options(bgScorer.use_gcs = FALSE)

  game <- Game$new("new_game")

  players <- c(
    "Player1",
    "Player2",
    "Player3"
  )

  # Round 0
  expect_round(game, 0)
  game$add_players(purrr::map(players, Player$new))

  # Round 1
  expect_order(game, players)
  expect_round(game, 1)
  expect_error(game$record_bids(c(1, 2, 4)), "exactly bid")
  expect_no_error(game$record_bids(c(1, 2, 3)))
  expect_no_error(game$output_table())
  expect_error(game$record_tricks(c(1, 2, 3)), "tricks")
  expect_round(game, 1)
  expect_order(game, players)
  expect_no_error(game$record_tricks(c(1, 2, 4)))
  expect_no_error(game$output_table())

  # Round 2
  expect_round(game, 2)
  expect_order(game, shifter(players, 1))

  expected_scores <- data.frame(
    player = players,
    score = c(1 + 10, 2 + 10, 4)
  )

  expect_identical(
    game$calc_table() |> dplyr::select(player, score),
    expected_scores
  )

  expect_no_error(game$loss_tracker())

  expect_no_error(game$record_bids(c(1, 2, 2)))
  expect_no_error(game$record_tricks(c(1, 2, 3)))

  # Round 3
  play_round(game, shifter(players, 2), 3, c(1, 2, 3), c(1, 2, 2))

  # Round 4
  play_round(game, shifter(players, 3), 4, c(1, 2, 3), c(1, 2, 1))

  id <- game$get_id()

  withr::with_tempfile("sg_dir", {
    game_orig <- game$clone(deep = TRUE)
    game$save(sg_dir)
    game$load(id, sg_dir)
    expect_identical(game_orig, game)
  })
})
