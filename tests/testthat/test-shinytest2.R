library(shinytest2)

start_app <- function(
    app_dir = test_path("test_app"),
    variant = platform_variant(),
    name = "Betting-Game-Scorer",
    height = 569,
    width = 979,
    seed = 42,
    ...
) {
  app <- AppDriver$new(
    app_dir = app_dir,
    variant = variant,
    name = name,
    height = height,
    width = width,
    seed = seed,
    ...
  )
  
  app$wait_for_value(input = "num_players")
  
  expect_equal(app$get_value(input = "num_players"), "2")
  
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
  expect_equal(
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
    
    expect_null(app$get_html(sweet_alert_button))
    
    expect_equal(
      app$get_values(input = paste0(players, "PR")) |>
        purrr::flatten() |>
        purrr::flatten_chr(),
      rep("", length(players))
    )
  } else {
    expect_equal(app$get_text(sweet_alert_title), "Error")
    expect_equal(app$get_text(sweet_alert_button), "Ok")
    
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
    
    loss_indicator <- app$get_html("body > div.swal2-container.swal2-center.swal2-backdrop-show > div > div.swal2-icon.swal2-info.swal2-icon-show")
    
    if (!is.null(loss_indicator)) {
      app$click(selector = sweet_alert_button)
    }
    
    app$wait_for_idle()
    
    expect_null(app$get_html(sweet_alert_button))
  } else {
    expect_equal(app$get_text(sweet_alert_title), "Error")
    expect_equal(app$get_text(sweet_alert_button), "Ok")
    
    app$click(selector = sweet_alert_button)
  }
}

google_auth_config_test <- function(app_dir = test_path("test_app"),
                                    envir = parent.frame()) {
  base_path <- file.path(app_dir, Sys.getenv("GOOGLE_CREDS_BASE"))
  creds_path <- withr::local_file(file.path(app_dir, "creds.json"),
                                  .local_envir = envir)
  google_auth_config(base = base_path, complete = creds_path)
  
  withr::local_envvar(GOOGLE_APPLICATION_CREDENTIALS = creds_path,
                      .local_envir = envir)
  
  expect_s3_class(bg_read_sheet("Completed"), "data.frame")
}

test_that("Google auth works", {
  google_auth_config_test()
})

test_that("{shinytest2} recording: Betting-Game-Scorer", {
  
  sg_dir <- withr::local_tempdir()
  withr::local_envvar(BG_GAMES_DIR = sg_dir)

  app <- start_app()
  withr::defer(app$stop())
  
  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )

  setup_game(app, players)

  sim_bids(app, players, valid = FALSE)
  sim_bids(app, players, valid = TRUE)

  sim_tricks(app, players, valid = FALSE)
  sim_tricks(app, players, valid = TRUE)
  
  for (i in seq(2, 12)) {
    sim_bids(app, players)
    sim_tricks(app, players)
  }
  
  rlang::check_installed("rvest", "to test final scores")
  
  final_scores_ui <- app$get_value(output = "final_scores") |>
    rvest::read_html() |>
    rvest::html_element("table") |>
    rvest::html_table() |>
    tibble::as_tibble(.name_repair = "unique") |>
    tibble::column_to_rownames("...1")
  
  game <- app$get_value(export = "game")
  
  final_scores_server <- game$calc_final_score()
  
  expect_equal(final_scores_ui, final_scores_server)
})

test_that("Reloading game works", {
  
  sg_dir <- withr::local_tempdir()
  withr::local_envvar(BG_GAMES_DIR = sg_dir)
  
  app <- start_app()
  
  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )
  
  setup_game(app, players)
  
  sim_bids(app, players)
  
  game <- app$get_value(export = "game")
  
  app$click("save_game")
  app$stop()
  
  app <- start_app()
  app$click("reload")
  
  loaded_game <- app$get_value(export = "game")
  
  expect_equal(game, loaded_game)
  
  sim_tricks(app, players)
  
  game <- app$get_value(export = "game")
  
  app$click("save_game")
  app$stop()
  
  app <- start_app()
  app$click("reload")
  
  loaded_game <- app$get_value(export = "game")
  
  expect_equal(game, loaded_game)
})
