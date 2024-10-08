#' The application Mobile User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  mobile <- getOption("bgScorer.mobile", TRUE)

  inputs_block <- div(
    class = "main_div",
    f7Block(
      id = "play_div",
      f7BlockHeader(uiOutput("round_info", inline = TRUE)),
      f7Button(
        "rollback",
        f7Icon("arrow_counterclockwise"),
        fill = FALSE
      ),
      f7Button(
        "save_game",
        f7Icon("floppy_disk"),
        fill = FALSE
      ),
      uiOutput("betting", class = "ginput_div"),
      shinyjs::hidden(uiOutput("playing", class = "ginput_div")),
      strong = TRUE
    )
  )

  play_table_block <- div(
    class = "main_div",
    f7Block(
      gt::gt_output("play_table")
    )
  )

  inputs_tab <- f7Tab(
    title = "Play",
    tabName = "play",
    active = TRUE,
    inputs_block
  )

  play_table_tab <- f7Tab(
    tabName = "scores",
    title = "Scores",
    play_table_block
  )

  navbar <- f7Navbar(
    title = "The Betting Game",
    hairline = FALSE,
    rightPanel = TRUE
  )

  panels <- tagList(
    f7Panel(id = "left_panel", side = "left"),
    f7Panel(
      id = "right_panel",
      title = "Settings",
      side = "right",
      settings_div()
    )
  )

  main_layout <- if (mobile) {
    f7TabLayout(
      navbar = navbar,
      panels = panels,
      f7Tabs(
        inputs_tab,
        play_table_tab,
        animated = FALSE,
        swipeable = FALSE
      )
    )
  } else {
    f7SingleLayout(
      navbar = navbar,
      panels = panels,
      div(
        class = "main_div",
        inputs_block,
        play_table_block
      )
    )
  }

  tagList(
    golem_add_external_resources(),
    f7Page(
      allowPWA = TRUE,
      title = "Betting Game Scorer",
      main_layout,
      options = list(
        theme = "auto",
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

#' Generate the avatar inputs
#'
#' @param num_players_opts The number of players
#'
#' @details Avatars can be generated at https://www.dicebear.com/playground
#' @return A list of avatar inputs
#' @noRd
gen_avatar_inputs <- function(num_players_opts) {
  avatar_opts <- app_sys("app", "www", "icons") |>
    list.files(pattern = "\\.svg$")

  avatar_imgs <- file.path("www", "icons", avatar_opts)

  avatar_inputs <- purrr::map(
    seq(num_players_opts),
    \(x) {
      input <- f7SmartSelect(
        paste0("A", x),
        "",
        choices = stats::setNames(avatar_imgs, rep(" ", length(avatar_imgs))),
        openIn = "popover",
        closeOnSelect = TRUE,
        selected = avatar_imgs[x],
        searchbar = FALSE
      ) |>
        htmltools::tagQuery()

      input$find("option")$each(
        \(x, i)  {
          tagAppendAttributes(
            x,
            `data-option-image` = avatar_imgs[i],
            `data-option-class` = "avatar-option"
          )
        }
      )
      input$allTags()
    }
  )

  avatar_inputs
}

settings_div <- function() {
  div(
    f7Toggle(
      "autosave",
      "Enable autosave"
    ),
    f7Button(
      "save_settings",
      "Save settings"
    ) |>
      tagSetStyle("margin-top: 20px"),
    class = "block no-hairlines"
  )
}

# Function to create the modal dialog on startup to set up the game
mob_startup_modal <- function() {
  saved_games <- get_saved_games()

  max_players <- 7

  num_players_opts <- seq(2, max_players)

  player_inputs <- c(
    list(f7Text("P1", NULL, placeholder = "Who deals first?")),
    purrr::map(
      num_players_opts, \(x) f7Text(paste0("P", x), NULL)
    )
  )

  avatar_inputs <- gen_avatar_inputs(seq_len(max_players))

  f7Popup(
    id = "setup_popup",
    title = "Game Set-up",
    div(
      f7Select("num_players", "How many players?", choices = num_players_opts),
      div(
        id = "apinputs_div",
        div(player_inputs, id = "pinputs", class = "apinputs"),
        div(avatar_inputs, id = "ainputs", class = "apinputs")
      ),
      f7Button("set_up", "New Game"),
      div(
        f7Select("saved_game_id", NULL, choices = saved_games),
        f7Button("reload", "Load Game"),
        id = "load_game_div",
        class = "list"
      ),
      id = "setup_div",
      class = "block no-hairlines"
    )
  )
}

# Function to create a modal dialog to display the scores at the end
mob_end_modal <- function() {
  f7Popup(
    id = "end_modal",
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
