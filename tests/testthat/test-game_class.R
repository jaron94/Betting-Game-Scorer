test_that("'Game' class works", {
  withr::local_options(bgScorer.use_gcs = FALSE)

  game <- Game$new("new_game")

  players <- c(
    "Player1",
    "Player2",
    "Player3"
  )

  game$add_players(purrr::map(players, \(x) Player$new(x)))

  expect_error(game$record_bids(c(1, 2, 4)), "exactly bid")
  expect_no_error(game$record_bids(c(1, 2, 3)))
  expect_no_error(game$output_table())
  expect_error(game$record_tricks(c(1, 2, 3)), "tricks")
  expect_no_error(game$record_tricks(c(1, 2, 4)))
  expect_no_error(game$output_table())

  expected_scores <- data.frame(
    player = players,
    score = c(1 + 10, 2 + 10, 4)
  )

  expect_equal(
    game$calc_table() |> dplyr::select(player, score),
    expected_scores
  )

  expect_no_error(
    game$loss_tracker()
  )

  id <- game$get_id()

  withr::with_tempfile("sg_dir", {
    game_orig <- game$clone(deep = TRUE)
    game$save(sg_dir)
    game$load(id, sg_dir)
    expect_equal(game_orig, game)
  })
})
