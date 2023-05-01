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
    player_names <- purrr::map_chr(
      seq_len(input$num_players),
      \(x) input[[paste0("P", x)]]
    )

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
    bids <- purrr::map_int(
      game$get_player_names(),
      \(name) as.integer(input[[paste0(name, "BR")]])
    )

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
    tricks <- purrr::map_int(
      game$get_player_names(),
      \(name) as.integer(input[[paste0(name, "PR")]])
    )

    req(tryCatch(
      game$record_tricks(tricks),
      error = function(e) {
        send_error_alert(e$message)
        return(FALSE)
      }
    ))

    trigger("update_game")

    if (game$get_round() > tot_rounds()) {
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
            dplyr::filter(.data$Rank == 1) |>
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
      text = paste(
        "Game has been successfully saved as",
        format_game_id(game$get_id())
      ),
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

    game$play_table()
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
    actionButton(
      if (bid_stage) "bet" else "score",
      if (bid_stage) "Enter Bids" else "Enter Results"
    )
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
