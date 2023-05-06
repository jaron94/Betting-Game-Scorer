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
          title = "Navigate",
          hairline = FALSE,
          shadow = TRUE,
          leftPanel = TRUE,
          rightPanel = TRUE
        ),
        f7Tabs(
          f7Tab(
            title = "Play",
            tabName = "play",
            active = TRUE,
            f7Card(
                uiOutput("betting"),
                shinyjs::hidden(uiOutput("playing"))
              )
          ),
          f7Tab(
            title = "Scores",
            tabName = "scores",
            f7Card(
              gt::gt_output("play_table")
            )
          ),
          animated = FALSE,
          swipeable = TRUE
        ),
        panels = tagList(
          f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover"),
          f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
        )
      ),
      options = list(
        theme = c("auto"),
        dark = FALSE,
        filled = FALSE,
        color = "#007aff",
        touch = list(
          tapHold = TRUE,
          tapHoldDelay = 750,
          iosTouchRipple = FALSE
        ),
        iosTranslucentBars = FALSE,
        navbar = list(
          iosCenterTitle = TRUE,
          hideNavOnPageScroll = TRUE
        ),
        toolbar = list(
          hideNavOnPageScroll = FALSE
        ),
        pullToRefresh = FALSE
      )
    )
  )
}

# Function to create the modal dialog on startup to set up the game
mob_startup_modal <- function() {
  saved_games <- get_saved_games()
  
  max_players <- 7
  
  num_players_opts <- seq(2, max_players)
  
  player_inputs <- purrr::map(
    num_players_opts, \(x) f7Text(paste0("P", x), NULL)
  )
  
  f7Popup(
    id = "setup_popup",
    title = "Game Set-up",
    f7Block(
      f7BlockHeader("Start new game"),
      f7Select("num_players", "How many players?", choices = num_players_opts),
      f7Flex(
        f7Text("P1", NULL, placeholder = "Who deals first?"),
        player_inputs
      ),
      f7Row(
        f7Button("set_up", "New Game")
      ),
      hairlines = FALSE
    ),
    f7Block(
      f7BlockHeader("Load saved game"),
      f7Select("saved_game_id", NULL, choices = saved_games),
      f7Button("reload", "Load Game:"),
      hairlines = FALSE
    )
  )
}
