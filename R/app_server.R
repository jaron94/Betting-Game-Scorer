# Set the Kable options to display missing values as empty strings
options(knitr.kable.NA = "")

utils::globalVariables(
  c(
    "Round", "Table", "Final Score", "player", "temp", "TEMP", "Stage",
    "Player"
  )
)

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  saved_game_dir <- get_saved_game_dir()
  
  stages <- c("B", "T", "S")
  trump_opts <- c("&spades;", "&hearts;", "&diams;", "&clubs;", "")
  
  game <- Game$new(Sys.time() |> as.numeric() |> as.character())
  
  init("update_game")
  
  gargoyle::on("update_game", {
    exportTestValues(game = game, round = game$get_round())
  })
  
  showModal(startup_modal())
  
  observeEvent(input$num_players, {
    max_players <- 7
    
    n_players <- as.integer(input$num_players)
    
    # Always show the first two player name inputs
    for (player_id in seq(3, max_players)) {
      shinyjs::toggle(paste0("P", player_id),
                      condition = player_id <= n_players
      )
    }
  })
  
  observeEvent(input$set_up, {
    
    player_names <- purrr::map_chr(seq_len(input$num_players),
                                   \(x) input[[paste0("P", x)]])
    
    if (any(player_names == "")) {
      send_error_alert(text = "Names not recorded for all players")
    }
    
    purrr::walk(player_names, req)
    
    players <- purrr::map(player_names, \(x) Player$new(x))
    
    game$add_players(players)
    
    if (isTruthy(input$game_id)) {
      game$change_id(input$game_id)
    }
    
    trigger("update_game")
    
    game$save(saved_game_dir)
    
    removeModal()
  })
  
  observeEvent(input$reload, {
    game_id <- input$saved_game_id
    req(game_id)
    game$load(game_id, saved_game_dir)
    trigger("update_game")
    removeModal()
  })
  
  observeEvent(input$bet, {
    bids <- purrr::map_int(game$get_player_names(),
                           \(name) as.integer(input[[paste0(name, "BR")]]))
    
    req(tryCatch(
      game$record_bids(bids),
      error = function(e) {
        send_error_alert(e$message)
        return(FALSE)
      }
    ))
    
    trigger("update_game")
  })
  
  observeEvent(input$score, {
    tricks <- purrr::map_int(game$get_player_names(),
                             \(name) as.integer(input[[paste0(name, "PR")]]))
    
    req(tryCatch(
      game$record_tricks(tricks),
      error = function(e) {
        send_error_alert(e$message)
        return(FALSE)
      }
    ))
    
    trigger("update_game")
    
    if (game$get_round() > 12) {
      
      game$save(saved_game_dir)
      
      shinyjs::hideElement("betting")
      shinyjs::hideElement("playing")
      
      final_scores <- game$calc_final_score()
      
      output$final_scores <- renderTable(
        final_scores,
        rownames = TRUE,
        digits = 0,
        spacing = "l",
        bordered = TRUE
      )
      
      output$end_message <- renderText({
        paste0(
          "Congratulations ",
          final_scores |>
            dplyr::filter(Rank == 1) |>
            rownames() |>
            paste(collapse = " and "),
          "!"
        )
      })
      
      showModal(end_modal())
      
      return()
    }
    
    tracked_losses <- game$loss_tracker()
    
    if (!is.null(tracked_losses)) {
      shinyWidgets::sendSweetAlert(
        session,
        type = "info",
        title = "",
        text = tracked_losses
      )
    }
  })
  
  observeEvent(input$save_game, {
    watch("update_game")
    game$save(saved_game_dir)
    shinyWidgets::sendSweetAlert(
      session,
      title = "Game Saved",
      text = paste("Game has been successfully saved as",
                   format_game_id(game$get_id())),
      type = "success"
    )
  })
  
  gargoyle::on("update_game", {
    shinyjs::toggle(id = "betting", condition = game$get_bid_stage())
    shinyjs::toggle(id = "playing", condition = !game$get_bid_stage())
  })
  
  output$betting <- renderUI({
    watch("update_game")
    create_game_inputs(game, TRUE)
  })
  
  output$playing <- renderUI({
    watch("update_game")
    create_game_inputs(game, FALSE)
  })
  
  output$round_info <- renderUI({
    watch("update_game")
    round <- game$get_round()
    
    if (round > 0) {
      trumps <- rep(trump_opts, 3)[round]
      HTML(paste0(
        "Round ", round, ": ", card_seq(round), " cards. ",
        if (trumps == "") "No Trumps" else paste("Trumps are", trumps)
      ))
    }
  })
  
  output$play_table <- function() {
    watch("update_game")
    
    curr_round <- game$get_round()
    
    if (curr_round < 1) {
      return()
    }
    
    num_players <- game$num_players()
    
    groups <- c(1, rep(3, num_players), 1, 1) |>
      purrr::set_names(c(" ", game$get_player_names(), " ", " "))
    col_names <- c("Round", rep(stages, num_players), "Cards", "Suit")
    
    output_table(game) |>
      kableExtra::kable(
        format = "html",
        digits = 0,
        col.names = col_names,
        escape = FALSE
      ) |>
      kableExtra::kable_styling(bootstrap_options = c("bordered", "striped")) |>
      kableExtra::row_spec(0,
                           font_size = if (num_players > 6) 9.7 else NULL
      ) |>
      kableExtra::add_header_above(
        groups,
        font_size = if (num_players > 6) 14 else NULL
      ) |>
      kableExtra::column_spec(
        c(1, which(col_names == "Cards"), which(col_names == "Suit")),
        width = "2cm"
      ) |>
      kableExtra::column_spec(
        seq(3, as.numeric(num_players) * 3, 3) + 1,
        bold = TRUE
      )
  }
  
}

# Function to create a vector of column names in the correct order for the table
get_col_order <- function(name, stages) {
  paste(name, stages, sep = "_")
}

Rank <- function(d) {
  j <- unique(rev(sort(d)))
  return(sapply(d, function(dd) which(dd == j)))
}

shifter <- function(x, n = 1) {
  mod_n <- n %% length(x)
  if (mod_n == 0) x else c(utils::tail(x, -mod_n), utils::head(x, mod_n))
}

calc_points <- function(bid, tricks) {
  tricks +
    if (bid == tricks) 10 else 0
}

loss_tracker <- function(tab) {
  temp <- tab |>
    dplyr::filter(Round != -1) |>
    dplyr::group_by(Round) |>
    dplyr::summarise_if(is.numeric, ~ .[2] == .[1]) |>
    dplyr::ungroup() |>
    dplyr::select(-Round) |>
    dplyr::summarise_all(~ list(tibble::tibble(
      lengths = rle(.)$lengths,
      values = rle(.)$values
    ))) |>
    tidyr::gather(key = "player", value = "rle") |>
    dplyr::filter(purrr::map_lgl(rle, ~ !utils::tail(.$values, 1))) |>
    dplyr::filter(purrr::map_lgl(rle, ~ utils::tail(.$lengths, 1) >= 3)) |>
    dplyr::mutate(rle = purrr::map(
      rle,
      ~ dplyr::filter(., lengths >= 3, !values)
    )) |>
    dplyr::group_split(player)

  if (length(temp) > 0) {
    purrr::map_chr(
      temp,
      ~ paste(.$player, "has lost", .$rle[[1]]$lengths, "times in a row")
    ) |>
      paste(collapse = "\n")
  } else {
    NA_character_
  }
}

create_game_inputs <- function(game, bid_stage) {
  tagList(
    purrr::map(
      game$get_order(),
      \(name) shinyWidgets::pickerInput(
        paste0(name, if (bid_stage) "BR" else "PR"),
        paste0(name, if (bid_stage) " bids?" else ": how many tricks?"),
        choices = c("", 0, seq_len(game$num_cards()))
      )
    ),
    actionButton(if (bid_stage) "bet" else "score",
                 if (bid_stage) "Enter Bids" else "Enter Results")
  )
}

get_saved_game_dir <- function(dir = Sys.getenv("BG_GAMES_DIR", "saved_games")) {
  dir
}

output_table <- function(game) {
  stages <- c("B", "T", "S")
  trump_opts <- c("&spades;", "&hearts;", "&diams;", "&clubs;", "")
  num_players <- game$num_players()
  
  game$calc_table() |>
    tidyr::pivot_wider(
      names_from = player,
      values_from = c(bid, tricks, score),
      names_glue = "{player}_{.value}",
      names_vary = "slowest"
    ) |>
    dplyr::mutate(
      Cards = card_seq(Round),
      Suit = rep(trump_opts, 3)[Round]
    )
}

send_error_alert <- function(text, session = getDefaultReactiveDomain()) {
  shinyWidgets::sendSweetAlert(
    session,
    title = "Error",
    text = text,
    type = "error"
  )
}

format_game_id <- function(id) {
  suppressWarnings(as.numeric(id)) |>
    as.POSIXct(origin = "1970-01-01") |>
    as.character()
}
