utils::globalVariables(
  c(
    "Round", "Table", "Final Score", "player", "Stage", "Player", "Suit"
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

  act_button <- f7Button

  # send the theme to javascript
  observe({
    session$sendCustomMessage(
      type = "ui-tweak",
      message = list(os = input$theme, skin = input$color)
    )
  })

  game <- Game$new(Sys.time() |> as.numeric() |> as.character())

  init("update_game")

  gargoyle::on("update_game", {
    exportTestValues(game = game, round = game$get_round())
  })

  mob_startup_modal()

  observe({
    purrr::map(
      seq_len(7),
      \(x) {
        golem::invoke_js(
          "genSmartSelectImg",
          list(
            src = req(input[[paste0("A", x)]]),
            i = x
          )
        )
      }
    )
  })

  observeEvent(input$num_players, {
    golem::invoke_js(
      "toggle_inputs",
      list(as.integer(input$num_players))
    )
  })

  observeEvent(input$set_up, {
    player_names <- purrr::map_chr(
      seq_len(input$num_players),
      \(x) input[[paste0("P", x)]]
    )

    if (any(player_names == "")) {
      send_error_alert(text = "Names not recorded for all players")
    }

    avatars <- purrr::map_chr(
      seq_len(input$num_players),
      \(x) input[[paste0("A", x)]]
    )

    if (any(avatars == "")) {
      send_error_alert(text = "Avatars not recorded for all players")
    }

    purrr::walk(player_names, req)
    purrr::walk(avatars, req)

    players <- purrr::map2(
      player_names,
      avatars,
      \(name, avatar) Player$new(name, avatar)
    )

    game$add_players(players)

    if (isTruthy(input$game_id)) {
      game$change_id(input$game_id)
    }

    trigger("update_game")

    game$save(saved_game_dir)

    remove_modal()
  })

  observeEvent(input$reload, {
    game_id <- input$saved_game_id
    req(game_id)
    assign(
      "game",
      game$load(game_id, saved_game_dir)$clone(deep = TRUE),
      inherits = TRUE
    )
    trigger("update_game")
    remove_modal()
  })

  # Set cookie for autosave input
  observeEvent(
    input$save_settings,
    {
      cookies::set_cookie(
        cookie_name = "autosave_setting",
        cookie_value = input$autosave
      )
    }
  )

  observeEvent(
    cookies::get_cookie("autosave_setting"),
    {
      updateF7Toggle(
        inputId = "autosave",
        checked = if (cookies::get_cookie("autosave_setting") == "true") TRUE
      )
    }
  )

  observeEvent(input$bet, {

    Sys.sleep(0.5)

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

    Sys.sleep(0.5)

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

      final_scores <- game$curr_score() |>
        dplyr::rename("Final Score" = "Score")

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

      mob_end_modal()

      return()
    }

    # autosave
    if (input$autosave) {
      game$save(saved_game_dir)
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

  observeEvent(input$rollback, {
    watch("update_game")
    shinyWidgets::confirmSweetAlert(
      session,
      inputId = "confirm_rollback",
      title = "Confirm rollback",
      text = paste0(
        "Push 'Confirm' to rollback the game to the previous stage.",
        "\n",
        "This action is not reversible."
      )
    )
  })

  observeEvent(input$confirm_rollback, {
    watch("update_game")
    if (isTRUE(input$confirm_rollback)) {
      game$rollback()
      trigger("update_game")
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
    bid_stage <- game$get_bid_stage()
    shinyjs::toggle(id = "betting", condition = bid_stage)
    shinyjs::toggle(id = "playing", condition = !bid_stage)
    shinyjs::toggle(id = "rollback", condition = game$get_round() > 1 || !bid_stage)
  })

  output$betting <- renderUI({
    watch("update_game")
    req(game$get_round() > 0)
    create_game_inputs(game, TRUE)
  })

  output$playing <- renderUI({
    watch("update_game")
    req(game$get_round() > 0)
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

  output$play_table <- gt::render_gt({
    req(input$orientation)

    watch("update_game")

    curr_round <- game$get_round()

    if (curr_round < 1) {
      return()
    }

    tables <- game$play_table()

    if (input$orientation == "landscape-primary") {
      tables$landscape
    } else {
      tables$portrait
    }
  })
}


gen_picker <- function(game, bid_stage, name) {
  player <- game$get_player_by_id(name)

  div(
    class = "row no-gap ginputs",
    div(
      class = "picker_container",
      img(src = player$get_avatar(), class = "avatar"),
      f7Stepper(
        inputId = paste0(name, if (bid_stage) "BR" else "PR"),
        label = name,
        min = 0,
        max = game$num_cards(),
        value = if (bid_stage) 0 else utils::tail(player$get_bids(), 1),
        manual = TRUE
      )
    )
  )
}


create_game_inputs <- function(game, bid_stage) {
  act_button <- f7Button

  num_players <- game$num_players()

  tagList(
    div(
      id = "picker_div",
      purrr::map(
        game$get_order(),
        \(name) gen_picker(game = game, bid_stage = bid_stage, name = name)
      )
    ),
    f7Row(
      id = "play_button",
      act_button(
        if (bid_stage) "bet" else "score",
        if (bid_stage) "Enter Bids" else "Enter Results"
      )
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

remove_modal <- function() {
  shinyjs::runjs("app.popup.get('.popup').close(true);")
}
