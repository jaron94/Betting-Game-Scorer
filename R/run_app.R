#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = bg_gcs_auth, # nolint
                    options = list(),
                    enableBookmarking = NULL, # nolint
                    uiPattern = "/", # nolint
                    ...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options, # nolint undesirable_function_linter
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}


#' Run the Shiny Application on Mobile
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app_mobile <- function(onStart = bg_gcs_auth, # nolint
                           options = list(),
                           enableBookmarking = NULL, # nolint
                           uiPattern = "/", # nolint
                           ...) {
  with_golem_options(
    app = shinyApp(
      ui = mobile_ui,
      server = mobile_server,
      onStart = onStart,
      options = options, # nolint undesirable_function_linter
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
