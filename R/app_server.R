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

  is_mob <- getOption("bgScorer.mobile", FALSE)

  act_button <- if (is_mob) f7Button else actionButton

  if (is_mob) {
    # send the theme to javascript
    observe({
      session$sendCustomMessage(
        type = "ui-tweak",
        message = list(os = input$theme, skin = input$color)
      )
    })
  }

  game <- Game$new(Sys.time() |> as.numeric() |> as.character())

  init("update_game")

  gargoyle::on("update_game", {
    exportTestValues(game = game, round = game$get_round())
  })

  if (is_mob) {
    mob_startup_modal()
  } else {
    showModal(startup_modal())
  }

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

    remove_modal(is_mob)
  })

  observeEvent(input$reload, {
    game_id <- input$saved_game_id
    req(game_id)
    game$load(game_id, saved_game_dir)
    trigger("update_game")
    remove_modal(is_mob)
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
    req(game$get_round() > 0)
    create_game_inputs(game, TRUE, is_mob)
  })

  output$playing <- renderUI({
    watch("update_game")
    req(game$get_round() > 0)
    create_game_inputs(game, FALSE, is_mob)
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
    watch("update_game")

    curr_round <- game$get_round()

    if (curr_round < 1) {
      return()
    }

    game$play_table()
  })
}


create_game_inputs <- function(game, bid_stage, mob) {
  picker <- if (mob) f7Picker else shinyWidgets::pickerInput
  act_button <- if (mob) f7Button else actionButton

  num_players <- game$num_players()

  input_height <- "var(--f7-input-height)"
  list_margin <- "var(--f7-list-margin-vertical)"

  row_min_height <- glue::glue("calc({input_height} + 2 * {list_margin})")
  row_style <- glue::glue(
    "margin: 0; width: 100%; align-items: center; ",
    "min-height: {row_min_height}; ",
    "height: calc(80% / {num_players})"
  )
  list_style <- "margin: 0; width: 60%"
  bt_style <- glue::glue(
    "margin-top: var(--f7-block-title-margin-bottom);",
    "width: calc(40% - 2 * var(--f7-block-padding-horizontal))"
  )

  gen_picker <- function(name) {
    f7Row(
      picker(
        inputId = paste0(name, if (bid_stage) "BR" else "PR"),
        label = name,
        choices = c("", 0, seq_len(game$num_cards())),
        placeholder = if (bid_stage) "bids?" else ": how many tricks?"
      ),
      gap = FALSE
    )
  }

  pickers <- htmltools::tagQuery(purrr::map(game$get_order(), gen_picker))$
    addAttrs(style = row_style)$
    find(".block-title")$addAttrs(style = bt_style)$
    resetSelected()$
    find(".list")$addAttrs(style = list_style)$
    allTags()

  tagList(
    pickers,
    f7Row(
      act_button(
        if (bid_stage) "bet" else "score",
        if (bid_stage) "Enter Bids" else "Enter Results"
      )
    ) |>
      tagSetHeight("20%")
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

remove_modal <- function(mob) {
  if (mob) {
    shinyjs::runjs("app.popup.get('.popup').close(true);")
  } else {
    removeModal()
  }
}
