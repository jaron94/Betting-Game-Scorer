library(shinytest2)

setup_game <- function(app, players) {
  app$set_inputs(num_players = length(players))
  app$set_inputs(!!!players, allow_no_input_binding_ = TRUE)
  app$click("set_up")
}

sim_bids <- function(app, players, valid = TRUE) {
  round <- app$get_value(export = "round")

  tot_tricks <- 7 - round

  n_players <- length(players)

  bid_ids <- paste0(players, "BR")

  if (valid) {
    bids <- sample(seq(0, tot_tricks), n_players)
    if (sum(bids) == tot_tricks) {
      bids[1] <- bids[1] + 1
    }
  } else {
    bids <- rep(floor(tot_tricks / n_players), n_players - 1)
    bids <- c(bids, tot_tricks - sum(bids))
  }

  names(bids) <- bid_ids

  app$set_inputs(!!!bids, allow_no_input_binding_ = TRUE)

  app$click("bet")
}

sim_tricks <- function(app, players, valid = TRUE) {
  round <- app$get_value(export = "round")

  tot_tricks <- 7 - round

  n_players <- length(players)

  trick_ids <- paste0(players, "PR")

  if (!valid) {
    tricks <- sample(seq(0, tot_tricks), n_players)
    if (sum(tricks) == tot_tricks) {
      tricks[1] <- tricks[1] + 1
    }
  } else {
    tricks <- rep(floor(tot_tricks / n_players), n_players - 1)
    tricks <- c(tricks, tot_tricks - sum(tricks))
  }

  names(tricks) <- trick_ids

  app$set_inputs(!!!tricks, allow_no_input_binding_ = TRUE)

  app$click("score")
}


test_that("{shinytest2} recording: Betting-Game-Scorer", {
  on.exit(
    {
      unlink(
        test_path("test_app", 
                  c("table.csv", "round.csv", "game_id.csv", "creds.json"))
      )
    },
    add = TRUE,
    after = FALSE
  )
  
  creds_path <- test_path("test_app", "creds.json")

  google_auth_config(base = test_path("test_app", Sys.getenv("GOOGLE_CREDS_BASE")),
                     complete = creds_path)
  
  withr::with_envvar(
    new = c(GOOGLE_APPLICATION_CREDENTIALS = creds_path),
    expect_s3_class(bg_read_sheet("Completed"), "data.frame")
  )

  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )

  app <- AppDriver$new(
    app_dir = test_path("test_app"),
    variant = platform_variant(),
    name = "Betting-Game-Scorer",
    height = 569, width = 979,
    seed = 42
  )

  on.exit(app$stop(), add = TRUE, after = FALSE)
  
  app$wait_for_value(input = "num_players")

  expect_equal(app$get_value(input = "num_players"), "2")

  setup_game(app, players)

  app$wait_for_value(
    input = paste0(players[1], "BR"),
    ignore = list(NULL)
  )

  expect_equal(
    app$get_values(input = paste0(players, "BR")) |>
      purrr::flatten() |>
      purrr::flatten_chr(),
    rep("", length(players))
  )

  sim_bids(app, players, valid = FALSE)

  sweet_alert_button <- ".swal2-confirm"

  expect_length(app$get_html(sweet_alert_button), 1)

  app$click(selector = sweet_alert_button)

  sim_bids(app, players, valid = TRUE)

  app$wait_for_value(
    input = paste0(players[1], "PR"),
    ignore = list(NULL)
  )

  expect_null(app$get_html(sweet_alert_button))

  expect_equal(
    app$get_values(input = paste0(players, "PR")) |>
      purrr::flatten() |>
      purrr::flatten_chr(),
    rep("", length(players))
  )

  sim_tricks(app, players, valid = FALSE)

  expect_length(app$get_html(sweet_alert_button), 1)

  app$click(selector = sweet_alert_button)

  sim_tricks(app, players, valid = TRUE)

  expect_null(app$get_html(sweet_alert_button))
})
