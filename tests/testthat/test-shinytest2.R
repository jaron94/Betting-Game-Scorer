withr::local_envvar(GOLEM_CONFIG_ACTIVE="test")

test_that("{shinytest2} recording: Betting-Game-Scorer", {
  
  sg_dir <- withr::local_tempdir()
  withr::local_envvar(BG_GAMES_DIR = sg_dir, BG_USE_GCS=FALSE)
  
  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )

  app <- start_app()
  app$log_message("Running full game test")
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

test_reload <- function(players) {
  app <- start_app()
  setup_game(app, players)
  sim_bids(app, players)
  game <- app$get_value(export = "game")
  app$click("save_game")
  
  app2 <- start_app()
  app2$click("reload")
  loaded_game <- app2$get_value(export = "game")
  expect_equal(game, loaded_game)
  sim_tricks(app2, players)
  game2 <- app2$get_value(export = "game")
  app2$click("save_game")
  
  app3 <- start_app()
  app3$click("reload")
  loaded_game2 <- app3$get_value(export = "game")
  expect_equal(game2, loaded_game2)
}

test_that("Reloading game works", {
  
  sg_dir <- withr::local_tempdir()
  withr::local_envvar(BG_GAMES_DIR = sg_dir, BG_USE_GCS=FALSE)
  
  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )
  
  test_reload(players)
})

test_that("Reloading game works with GCS", {
  
  base_path <- test_path("test_app", Sys.getenv("GOOGLE_CREDS_BASE"))
  creds_path <- withr::local_file(
    test_path("test_app", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
  )
  google_auth_config(base = base_path, complete = creds_path)
  
  bg_gcs_auth(creds_path)
  
  sg_dir_path <- get_golem_config("bg_games_dir")
  sg_dir <- withr::local_file(test_path("test_app", sg_dir_path))
  withr::defer({
    objs <- googleCloudStorageR::gcs_list_objects(prefix = sg_dir_path)$name
    purrr::walk(objs, googleCloudStorageR::gcs_delete_object)
  })
  withr::local_envvar(BG_GAMES_DIR = sg_dir_path, BG_USE_GCS = TRUE)
  
  players <- c(
    P1 = "Jon",
    P2 = "Tash",
    P3 = "Mum"
  )
  
  test_reload(players)
})
