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
          shinyjs::hidden(uiOutput("playing"))
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
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bgScorer"
    ),
    shinyjs::useShinyjs()
  )
}

# Function to determine the number of cards dealt in the round
card_seq <- function(round) {
  c(7:1, 2:7)[round]
}

# Wrapper around read_csv to suppress messages
read_csv_q <- function(file) {
  suppressMessages(readr::read_csv(file))
}

# Function to create the modal dialog on startup to set up the game
startup_modal <- function() {
  modalDialog(
    column(12, align = "center", h3("Game Set-up")),
    column(12,
      align = "center",
      shinyWidgets::pickerInput("num_players",
        "How many players?",
        choices = 2:7
      )
    ),
    column(6, textInput("P1", "Who deals first?")),
    uiOutput("player_inputs"),
    footer = tagList(
      actionButton("reload", "Reload Previous Game"),
      actionButton("set_up", "New Game")
    )
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
