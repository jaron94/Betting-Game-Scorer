start_app <- function(
    app_dir = testthat::test_path("test_app"),
    variant = shinytest2::platform_variant(),
    name = "Betting-Game-Scorer",
    height = 569,
    width = 979,
    # NB: this doesn't set the seed for the tests, only for the app
    seed = 42,
    timeout = 6 * 1000,
    options = list(shiny.devmode = TRUE),
    ...) {
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    variant = variant,
    name = name,
    height = height,
    width = width,
    seed = seed,
    timeout = timeout,
    options = options, # nolint undesirable_function_linter
    ...
  )

  app$wait_for_value(input = "num_players")

  testthat::expect_identical(app$get_value(input = "num_players"), "2")

  app
}

setup_game <- function(app, players) {
  app$log_message("Setting up game")
  app$set_inputs(num_players = length(players))
  app$set_inputs(!!!players, allow_no_input_binding_ = TRUE)
  app$click(selector = "#set_up")
  app$wait_for_value(
    input = paste0(players[1], "BR"),
    ignore = list(NULL)
  )
  testthat::expect_identical(
    app$get_values(input = paste0(players, "BR")) |>
      purrr::flatten() |>
      as.integer(),
    rep(0L, length(players))
  )
  app$log_message("Game set up")
}

sim_bids <- function(app, players, valid = TRUE, seed = 42) {
  round <- app$get_value(export = "round")
  app$log_message(paste("Round:", round))
  app$log_message(
    paste("Simulating", if (valid) "valid" else "invalid", "bids")
  )

  sweet_alert_button <- ".swal2-confirm"
  sweet_alert_title <- ".swal2-title"

  n_players <- length(players)

  bids <- gen_bid_set(round, n_players, valid, seed)

  names(bids) <- paste0(players, "BR")

  if (sum(bids) > 0) {
    app$set_inputs(!!!bids, allow_no_input_binding_ = TRUE)
  } else {
    app$set_inputs(!!!bids, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  }

  app$click(selector = "#bet")
  app$wait_for_idle()

  if (valid) {
    app$wait_for_value(
      input = paste0(players[1], "PR"),
      ignore = list(NULL)
    )

    testthat::expect_null(app$get_html(sweet_alert_button))

    pr_input_ids <- paste0(players, "PR")

    app_pr_inputs <- app$get_values(input = pr_input_ids) |>
      purrr::flatten() |>
      unlist(use.names = TRUE)

    app_pr_inputs <- app_pr_inputs[pr_input_ids]

    exp_pr_inputs <- bids |>
      purrr::set_names(\(x) sub("BR", "PR", x, fixed = TRUE))

    testthat::expect_identical(
      app_pr_inputs,
      exp_pr_inputs
    )
  } else {
    testthat::expect_identical(app$get_text(sweet_alert_title), "Error")
    testthat::expect_identical(app$get_text(sweet_alert_button), "Ok")

    app$click(selector = sweet_alert_button)
    app$wait_for_idle()
  }
}


sim_tricks <- function(app, players, valid = TRUE, seed = 42) {
  round <- app$get_value(export = "round")
  app$log_message(paste("Round:", round))
  app$log_message(
    paste("Simulating", if (valid) "valid" else "invalid", "tricks")
  )

  sweet_alert_button <- ".swal2-confirm"
  sweet_alert_title <- ".swal2-title"

  n_players <- length(players)

  tricks <- gen_trick_set(round, n_players, valid, seed)

  names(tricks) <- paste0(players, "PR")

  app$set_inputs(!!!tricks, allow_no_input_binding_ = TRUE)

  app$click(selector = "#score")
  app$wait_for_idle()

  if (valid) {
    loss_indicator <- app$get_html(
      paste(
        "body > div.swal2-container.swal2-center.swal2-backdrop-show",
        "> div > div.swal2-icon.swal2-info.swal2-icon-show"
      )
    )

    if (!is.null(loss_indicator)) {
      app$click(selector = sweet_alert_button)
    }

    app$wait_for_idle()

    testthat::expect_null(app$get_html(sweet_alert_button))
  } else {
    testthat::expect_identical(app$get_text(sweet_alert_title), "Error")
    testthat::expect_identical(app$get_text(sweet_alert_button), "Ok")

    app$click(selector = sweet_alert_button)
    app$wait_for_idle()
  }
}

test_reload <- function(name, players) {
  app <- start_app(name = name)
  setup_game(app, players)
  sim_bids(app, players)
  game <- app$get_value(export = "game")
  app$click(selector = "#save_game")

  app2 <- start_app()
  app2$click(selector = "#reload")
  loaded_game <- app2$get_value(export = "game")
  testthat::expect_identical(game, loaded_game)
  sim_tricks(app2, players)
  game2 <- app2$get_value(export = "game")
  app2$click(selector = "#save_game")

  app3 <- start_app()
  app3$click(selector = "#reload")
  loaded_game2 <- app3$get_value(export = "game")
  testthat::expect_identical(game2, loaded_game2)
}


with_gcs_dir <- function(dir, code) {
  on.exit(googleCloudStorageR::gcs_delete_all(dir))
  force(code)
  invisible(dir)
}
