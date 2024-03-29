withr::local_envvar(GOLEM_CONFIG_ACTIVE = "test")

test_that("{shinytest2} recording: Betting-Game-Scorer", {
  sg_dir <- withr::local_tempdir()
  withr::local_options(bgScorer.use_gcs = FALSE)
  withr::local_envvar(BG_GAMES_DIR = sg_dir)
  seed <- 42

  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )

  app <- start_app(name = "full_game_test")
  setup_game(app, players)

  sim_bids(app, players, valid = FALSE, seed = seed)
  sim_bids(app, players, valid = TRUE, seed = seed)

  sim_tricks(app, players, valid = FALSE, seed = seed)
  sim_tricks(app, players, valid = TRUE, seed = seed)

  for (i in seq(2, tot_rounds())) {
    sim_bids(app, players, seed = seed)
    sim_tricks(app, players, seed = seed)
  }

  # Should be end modal
  app$expect_text(".popup")
  app$expect_screenshot(name = "final_scores")

  rlang::check_installed("rvest", "to test final scores")

  final_scores_ui <- app$get_html(".popup") |>
    rvest::read_html() |>
    rvest::html_element("table") |>
    rvest::html_table() |>
    tibble::as_tibble(.name_repair = "unique") |>
    tibble::column_to_rownames("...1")

  game <- app$get_value(export = "game")

  final_scores_server <- game$curr_score() |>
    dplyr::mutate("Final Score" = as.integer(.data$Score)) |>
    dplyr::select(-"Score")

  testthat::expect_identical(final_scores_ui, final_scores_server)
})

test_that("Reloading game works", {
  sg_dir <- withr::local_tempdir()
  withr::local_options(bgScorer.use_gcs = FALSE)
  withr::local_envvar(BG_GAMES_DIR = sg_dir)

  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )

  test_reload("local_reload", players)
})

test_that("Reloading game works with GCS", {
  withr::local_options(bgScorer.use_gcs = TRUE)
  bg_gcs_auth()

  sg_dir_path <- get_golem_config("bg_games_dir")
  sg_dir <- withr::local_file(test_path("test_app", sg_dir_path))
  withr::defer({
    objs <- googleCloudStorageR::gcs_list_objects(prefix = sg_dir_path)$name
    purrr::walk(objs, googleCloudStorageR::gcs_delete_object)
  })
  withr::local_envvar(BG_GAMES_DIR = sg_dir_path)

  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )

  test_reload("gcs_reload", players)
})
