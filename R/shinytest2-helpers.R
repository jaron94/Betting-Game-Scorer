start_app <- function(
    app_dir = testthat::test_path("test_app"),
    variant = shinytest2::platform_variant(),
    name = "Betting-Game-Scorer",
    height = 569,
    width = 979,
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
  app$set_inputs(num_players = length(players))
  app$set_inputs(!!!players, allow_no_input_binding_ = TRUE)
  app$click("set_up")
  app$wait_for_value(
    input = paste0(players[1], "BR"),
    ignore = list(NULL)
  )
  testthat::expect_identical(
    app$get_values(input = paste0(players, "BR")) |>
      purrr::flatten() |>
      purrr::flatten_chr(),
    rep("", length(players))
  )
}

sim_bids <- function(app, players, valid = TRUE) {
  round <- app$get_value(export = "round")

  sweet_alert_button <- ".swal2-confirm"
  sweet_alert_title <- ".swal2-title"

  tot_tricks <- card_seq(round)

  n_players <- length(players)

  bid_ids <- paste0(players, "BR")

  if (valid) {
    bids <- sample(seq(0, tot_tricks), n_players, replace = TRUE)
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

  if (valid) {
    app$wait_for_value(
      input = paste0(players[1], "PR"),
      ignore = list(NULL)
    )

    testthat::expect_null(app$get_html(sweet_alert_button))

    testthat::expect_identical(
      app$get_values(input = paste0(players, "PR")) |>
        purrr::flatten() |>
        purrr::flatten_chr(),
      rep("", length(players))
    )
  } else {
    testthat::expect_identical(app$get_text(sweet_alert_title), "Error")
    testthat::expect_identical(app$get_text(sweet_alert_button), "Ok")

    app$click(selector = sweet_alert_button)
  }
}

sim_tricks <- function(app, players, valid = TRUE) {
  round <- app$get_value(export = "round")

  sweet_alert_button <- ".swal2-confirm"
  sweet_alert_title <- ".swal2-title"

  tot_tricks <- card_seq(round)

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

  if (valid) {
    app$wait_for_idle()

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
  }
}


with_gcs_dir <- function(dir, code) {
  on.exit(googleCloudStorageR::gcs_delete_all(dir))
  force(code)
  invisible(dir)
}
