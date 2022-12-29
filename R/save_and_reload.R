#' @title Use googlesheets4
#'
#' @name googlesheets4_partials
#'
#' @description Mapping from bg_* to googlesheets4 functions
#' @details
#' * [bg_write_sheet] -- [googlesheets4::sheet_write]
#' * [bg_read_sheet] -- [googlesheets4::read_sheet]
#' * [bg_sheet_append] -- [googlesheets4::sheet_append]
#' * [bg_sheet_names] -- [googlesheets4::sheet_delete]
#' * [bg_sheet_delete] -- [googlesheets4::sheet_delete]
#'
#' @usage NULL
bg_write_sheet <- function(...) {
  googlesheets4::sheet_write(..., ss = Sys.getenv("SS_ID"))
}

#' @rdname googlesheets4_partials
#' @usage NULL
bg_read_sheet <- purrr::partial(googlesheets4::read_sheet,
  ss = !!Sys.getenv("SS_ID")
)

#' @rdname googlesheets4_partials
#' @usage NULL
bg_sheet_append <- purrr::partial(googlesheets4::sheet_append,
  ss = !!Sys.getenv("SS_ID")
)

#' @rdname googlesheets4_partials
#' @usage NULL
bg_sheet_names <- purrr::partial(googlesheets4::sheet_names,
  ss = !!Sys.getenv("SS_ID")
)

#' @rdname googlesheets4_partials
#' @usage NULL
bg_sheet_delete <- purrr::partial(googlesheets4::sheet_delete,
  ss = !!Sys.getenv("SS_ID")
)


table_schema <- function() {
  data.frame(
    game_id = character(),
    Player = character(),
    Round = integer(),
    Stage = character(),
    value = integer()
  )
}


reshape_game_data <- function(data) {
  readr::read_csv("table.csv") |>
    tidyr::gather("Player", "value", -Round, -Stage) |>
    dplyr::relocate(Player)
}


#' Save game data to a Google sheet
#'
#' @param game_id String containing ID of the game
#' @param data Data frame containing game data
#' @param in_progress TRUE if game is in progress, FALSE if game is completed
#'
#' @return Invisibly, the name of the sheet where the data was saved
#' @export
save_game <- function(game_id, data, in_progress) {
  correct_names <- names(table_schema())

  if (in_progress) {
    # Create or overwrite the in progress save data for this game
    bg_write_sheet(
      data = data,
      sheet = game_id
    )

    return(invisible(game_id))
  } else {
    if (!identical(names(data), correct_names[-1])) {
      stop("Game data is not in the correct format")
    }

    res_sheet <- get_golem_config("results_sheet")

    res_sheet_names <- function() {
      names(bg_read_sheet(sheet = res_sheet, n_max = 1))
    }

    if (!res_sheet %in% bg_sheet_names() ||
      !identical(correct_names, res_sheet_names())) {
      stop("Results sheet has not been correctly initialised")
    }

    # Append the completed game data to the results sheet
    data |>
      dplyr::filter(Round >= 0) |>
      reshape_game_data() |>
      tibble::add_column(game_id = game_id) |>
      bg_sheet_append(sheet = res_sheet)

    # If it exists, delete the in progress save data for this game
    if (game_id %in% bg_sheet_names()) {
      bg_sheet_delete(sheet = game_id)
    }

    return(invisible(res_sheet))
  }
}


#' Load an in-progress game
#'
#' @inheritParams save_game
#' @export
load_game <- function(game_id) {
  bg_read_sheet(sheet = game_id) |>
    readr::write_csv(get_golem_config("table_path"))

  readr::write_csv(
    tibble::tibble(game_id = game_id),
    get_golem_config("game_id_path")
  )
}


init_results_sheet <- function() {
  if (!interactive()) {
    warning("Attempt to overwrite sheet data non-interactively failed")
    return(invisible())
  }

  answer <- readline(
    paste0(
      "Are you sure you want to overwrite all previous data on sheet ",
      get_golem_config("results_sheet"), "? y/n "
    )
  )

  template <- table_schema()

  if (answer %in% c("y", "Y")) {
    bg_write_sheet(
      data = template,
      sheet = get_golem_config("results_sheet")
    )
  }
}


read_game_id <- function(game_id_path) {
  readr::read_csv(game_id_path, col_types = "c")$game_id
}


google_auth_config <- function(
    base = Sys.getenv("GOOGLE_CREDS_BASE"),
    complete = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"),
    pk = Sys.getenv("GOOGLE_PK")
  ) {
  
  c(
    jsonlite::read_json(base), 
    private_key = jsonlite::fromJSON(shQuote(pk))
  ) |>
    jsonlite::write_json(complete,
                         auto_unbox = TRUE, 
                         pretty = TRUE)
}
