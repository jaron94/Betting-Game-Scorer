#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = shinythemes::shinytheme("readable"),
      titlePanel("Betting Game Scorer"),
      sidebarLayout(
        sidebarPanel(
          width = 2,
          h4(uiOutput("round_info")),
          uiOutput("betting"),
          shinyjs::hidden(uiOutput("playing")),
          actionButton("save_game", "Save game")
        ),
        mainPanel(
          fluidRow(column(10,
            align = "center",
            tableOutput("play_table")
          ))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  jscode <- paste0(
    "window.onbeforeunload = function() ",
    '{ return "Please use the button on the webpage"; };'
  )

  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bgScorer"
    ),
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .form-group, {
           margin-left: 0px;
           margin-right: 0px
      }")),
    tags$script(jscode)
  )
}

# Function to create the modal dialog on startup to set up the game
startup_modal <- function() {
  saved_games <- get_saved_games()

  max_players <- 7

  modalDialog(
    fluidRow(column(12, align = "center", h3("Game Set-up"))),
    fluidRow(
      column(12,
        align = "center",
        shinyWidgets::pickerInput("num_players",
          "How many players?",
          choices = seq(2, max_players)
        )
      )
    ),
    fluidRow(
      column(6, textInput("P1", NULL, placeholder = "Who deals first?")),
      purrr::map(
        seq(2, max_players),
        \(x) column(6, textInput(paste0("P", x), NULL))
      )
    ),
    fluidRow(hr()),
    fluidRow(
      column(3, actionButton("reload", "Load Game:", width = "100%")),
      column(3, shinyWidgets::pickerInput(
        "saved_game_id",
        NULL,
        choices = saved_games,
        width = "100%"
      )),
      column(6, actionButton("set_up", "New Game", width = "100%"))
    ),
    footer = NULL
  )
}

# Function to create a modal dialog to display the scores at the end
end_modal <- function() {
  modalDialog(
    easyClose = TRUE,
    fluidRow(
      column(12,
        align = "center",
        h4(textOutput("end_message"))
      )
    ),
    fluidRow(
      column(12,
        align = "center",
        tableOutput("final_scores")
      )
    )
  )
}
