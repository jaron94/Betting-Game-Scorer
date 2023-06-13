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
          title = "The Betting Game",
          hairline = FALSE,
          shadow = TRUE
        ),
        f7Tabs(
          f7Tab(
            title = "Play",
            tabName = "play",
            active = TRUE,
            f7Block(
              f7BlockHeader(uiOutput("round_info", inline = TRUE)) |>
                tagSetStyle("font-size: 20px; height: calc(10% - var(--f7-block-header-margin));"),
              hairlines = TRUE,
              strong = TRUE,
              inset = FALSE,
              uiOutput("betting", class = "ginput_div"),
              shinyjs::hidden(uiOutput("playing", class = "ginput_div"))
            ) |>
              tagSetHeight("calc(100% - 2 * var(--f7-block-margin-vertical))")
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
        )
      ),
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

# Container for bid/tricks inputs that ensures they fit in one screen
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
        choices = setNames(avatar_imgs, rep(" ", length(avatar_imgs))),
        openIn = "popover",
        closeOnSelect = TRUE,
        selected = avatar_imgs[x],
        searchbar = FALSE
      ) |>
        htmltools::tagQuery()

      input$find("option")$each(
        \(x, i) tagAppendAttributes(
          x,
          `data-option-image` = avatar_imgs[i],
          `data-option-class` = "avatar-option"
        )
      )
      input$allTags()
    }
  )

  avatar_inputs
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
      f7BlockHeader("Start new game"),
      f7Select("num_players", "How many players?", choices = num_players_opts),
      div(id = "apinputs_div",
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

f7TimelineItemDiv <- function(..., id) {
  htmltools::tagAppendAttributes(
    f7TimelineItem(...),
    id = id,
    .cssSelector = ".timeline-item-divider"
  )
}

gen_timeline_items <- function(max_rounds = 13) {
  purrr::map(
    seq_len(max_rounds),
    \(round) {
      f7TimelineItemDiv(
        paste("Round", round),
        id = paste0("tl_item", round)
      )
    }
  )
}
