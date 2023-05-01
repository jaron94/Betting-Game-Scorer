#' The application Mobile User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
mobile_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    f7Page(
      title = "Betting Game Scorer",
      f7TabLayout(
        navbar = f7Navbar(
          title = "popup",
          hairline = FALSE,
          shadow = TRUE,
          leftPanel = TRUE,
          rightPanel = TRUE
        ),
        f7Tabs(
          f7Tab(
            title = "Popup button",
            tabName = "pop",
            f7Button("togglePopup", "Toggle Popup"),
            active = TRUE
          )
        ),
        panels = tagList(
          f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover"),
          f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
        )
      ),
      options = list(dark = FALSE)
    )
  )
}

# Function to create the modal dialog on startup to set up the game
mob_startup_modal <- function() {
  saved_games <- get_saved_games()
  
  max_players <- 7
  
  player_inputs <- purrr::map(
    seq(2, max_players), \(x) f7Text(paste0("P", x), NULL)
  )
  
  f7Popup(
    id = "setup_popup",
    title = "Game Set-up",
    f7Row(
      f7Col(
        f7Select("num_players", "How many players?", choices = seq(2, max_players))
      )
    ),
    f7Flex(
      f7Text("P1", NULL, placeholder = "Who deals first?"),
      player_inputs
    ),
    f7Block(
      f7Row(gap = FALSE,
        f7Col(f7Button("reload", "Load Game:")),
        f7Col(f7Select("saved_game_id", NULL, choices = saved_games))
      ),
      f7Row(
        f7Button("set_up", "New Game")
      ),
      hairlines = FALSE
    )
    
  )
}

mobile_server <- function(input, output, session) {
  observeEvent(input$togglePopup, {
    # f7Popup(
    #   id = "popup1",
    #   title = "POPUP",
    #   f7Select("num_players", "How many players?", choices = seq(2, 7)),
    #   f7Row(
    #     f7Col(
    #       f7Select("num_players", "How many players?", choices = seq(2, 7))
    #     )
    #   )
    # )
    mob_startup_modal()
  })
  
  # send the theme to javascript
  observe({
    session$sendCustomMessage(
      type = "ui-tweak",
      message = list(os = input$theme, skin = input$color)
    )
  })
}
