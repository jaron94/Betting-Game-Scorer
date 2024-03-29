#' Authenticate bgScorer to GCS
#'
#' @param file_name Name of file containing Service Account key
#' @param use_gcs Whether to use GCS. Default is the "use_gcs" config value
#'
#' @return Function to be called when starting the app
#' @export
bg_gcs_auth <- function(
  file_name = "bgScorer-testing.json",
  use_gcs = get_golem_config("use_gcs")
) {
  if (use_gcs) {
    json <- gargle::secret_decrypt_json(
      system.file("secret", "bgScorer-testing.json", package = "bgScorer"),
      key = "BGSCORER_PASSWORD"
    )
    googleCloudStorageR::gcs_auth(json)
    default_bucket <- Sys.getenv("GCS_DEFAULT_BUCKET")
    googleCloudStorageR::gcs_global_bucket(default_bucket)
  }
}

get_saved_games <- function(use_gcs = get_golem_config("use_gcs")) {
  saved_games <- if (use_gcs) {
    gcs_objs <- googleCloudStorageR::gcs_list_objects(
      prefix = get_saved_game_dir()
    )

    if (purrr::is_empty(gcs_objs)) {
      return()
    }

    gcs_objs |>
      dplyr::arrange(dplyr::desc(.data$updated)) |>
      dplyr::pull("name") |>
      basename() |>
      tools::file_path_sans_ext()
  } else {
    saved_game_files <- list.files(get_saved_game_dir(), full.names = TRUE)

    if (purrr::is_empty(saved_game_files)) {
      return()
    }

    file.info(saved_game_files) |>
      dplyr::arrange(dplyr::desc(.data$mtime)) |>
      rownames() |>
      basename() |>
      tools::file_path_sans_ext()
  }

  saved_game_dates <- format_game_id(saved_games) |>
    dplyr::coalesce(saved_games)

  names(saved_games) <- saved_game_dates

  saved_games
}

get_saved_game_dir <- function(dir = get_golem_config("bg_games_dir")) {
  dir
}

format_game_id <- function(id) {
  suppressWarnings(as.numeric(id)) |>
    as.POSIXct(origin = "1970-01-01") |>
    as.character()
}
