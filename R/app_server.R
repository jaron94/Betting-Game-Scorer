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
  # Create identifiers for the three columns on the scorecard:
  # bidding, tricks and score
  stages <- c("B", "T", "S")
  trump_opts <- c("&spades;", "&hearts;", "&diams;", "&clubs;", "")

  table_path <- get_golem_config("table_path")
  round_path <- get_golem_config("round_path")
  game_id_path <- get_golem_config("game_id_path")

  # shinyjs::runjs("$('#game_id').attr('maxlength',15)")

  values <- reactiveValues(a = NULL, b = NULL, c = NULL)

  showModal(startup_modal())

  observeEvent(input$set_up, {
    if (any(purrr::map_lgl(
      seq_len(input$num_players),
      ~ input[[paste0("P", .)]] == ""
    ))) {
      shinyWidgets::sendSweetAlert(session,
        type = "error", title = "Error",
        text = "Names not recorded for all players"
      )
    } else {
      values$a <- 1
      values$b <- 1
      values$c <- 1
    }
  })

  observeEvent(input$reload, {
    values$a <- 1
    values$b <- 1
    values$c <- 1
  })

  observeEvent(input$score, {
    values$b <- 3
    values$b <- 4
    values$c <- 3
    values$c <- 4
  })

  observeEvent(input$bet, {
    values$c <- 3
    values$c <- 4
  })

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

  observeEvent(input$set_up, priority = 1, {
    if (any(purrr::map_lgl(
      seq_len(input$num_players),
      ~ !isTruthy(input[[paste0("P", .)]])
    ))) {
      shinyWidgets::sendSweetAlert(session,
        type = "error",
        text = "Names not recorded for all players"
      )
    } else {
      removeModal()

      players <- purrr::map_chr(
        seq_len(input$num_players),
        ~ input[[paste0("P", .)]]
      )

      matrix(0,
        ncol = as.integer(input$num_players), nrow = length(stages),
        dimnames = list(NULL, players)
      ) |>
        tibble::as_tibble() |>
        tibble::add_column(Round = -1, Stage = stages) |>
        readr::write_csv(table_path)

      readr::write_csv(tibble::tibble(Round = as.integer(0)), round_path)
      exportTestValues(round = 0)

      if (!isTruthy(input$game_id)) {
        game_id <- Sys.time() |>
          as.numeric() |>
          as.character()
      }

      readr::write_csv(tibble::tibble(game_id = game_id), game_id_path)
      # as.POSIXct(as.numeric(game_id), origin = "1970-01-01")
    }
  })

  observeEvent(input$save_game, {
    data <- readr::read_csv(table_path)
    game_id <- read_game_id(game_id_path)

    save_game(game_id, data, in_progress = TRUE)
  })

  observeEvent(input$reload, {
    removeModal()

    game_id <- input$saved_game_id

    if (isTruthy(game_id) && !game_id %in% "auto") {
      load_game(game_id)
    }
  })

  num_players <- eventReactive(values$a, {
    if (input$reload == 0) {
      input$num_players
    } else {
      readr::read_csv(table_path) |>
        dplyr::select(-Round, -Stage) |>
        ncol()
    }
  })

  order <- eventReactive(values$a, {
    if (input$reload == 0) {
      purrr::map_chr(seq_len(num_players()), ~ input[[paste0("P", .)]])
    } else {
      tab <- readr::read_csv(table_path)
      tab |>
        dplyr::select(-Round, -Stage) |>
        colnames() |>
        shifter(n = utils::tail(tab$Round, 1) - 1)
    }
  })

  shifted_order <- eventReactive(values$b, {
    order() |> shifter(n = read_csv_q(round_path)$Round)
  })

  observeEvent(values$b, {
    output$round_info <- renderUI({
      round <- read_csv_q(round_path)$Round + 1
      trumps <- rep(trump_opts, 3)[round]
      HTML(paste0(
        "Round ", round, ": ", card_seq(round), " cards. ",
        if (trumps == "") "No Trumps" else paste("Trumps are", trumps)
      ))
    })
  })

  observeEvent(values$a, {
    output$betting <- renderUI({
      tagList(
        purrr::map(
          shifted_order(),
          ~ shinyWidgets::pickerInput(paste0(., "BR"),
            paste0(., " bids?"),
            choices = c("", seq(0, card_seq(read_csv_q(round_path)$Round + 1)))
          )
        ),
        actionButton("bet", "Enter Bids")
      )
    })
  })

  observeEvent(values$a, {
    output$playing <- renderUI({
      tagList(
        purrr::map(
          shifted_order(),
          ~ shinyWidgets::pickerInput(paste0(., "PR"),
            paste0(., ": how many tricks?"),
            choices = c("", seq(0, card_seq(read_csv_q(round_path)$Round + 1)))
          )
        ),
        actionButton("score", "Enter Results")
      )
    })
  })

  observeEvent(input$bet, {
    round <- read_csv_q(round_path)$Round
    bids <- purrr::map(order(), ~ as.integer(input[[paste0(., "BR")]])) |>
      purrr::set_names(order())
    if (any(is.na(unlist(bids)))) {
      shinyWidgets::sendSweetAlert(session,
        title = "Error", text = "Not all players have bid",
        type = "error"
      )
    } else if (sum(as.integer(bids, na.rm = TRUE)) == card_seq(round + 1)) {
      shinyWidgets::sendSweetAlert(session,
        title = "Error", text = "You are currently exactly bid",
        type = "error"
      )
    } else {
      readr::read_csv(table_path) |>
        dplyr::bind_rows(bids |>
          list(Round = round, Stage = stages[1]) |>
          purrr::flatten()) |>
        readr::write_csv(table_path)

      shinyjs::hideElement("betting")
      shinyjs::showElement("playing")
    }
  })

  observeEvent(input$score, {
    round <- read_csv_q(round_path)$Round
    tricks <- purrr::map(order(), ~ as.integer(input[[paste0(., "PR")]])) |>
      purrr::set_names(order())
    if (any(is.na(unlist(tricks)))) {
      shinyWidgets::sendSweetAlert(session,
        title = "Error",
        text = "Tricks have not recorded for all players",
        type = "error"
      )
    } else if (sum(unlist(tricks), na.rm = TRUE) == card_seq(round + 1)) {
      prev_scores <- readr::read_csv(table_path) |>
        dplyr::filter(Stage == stages[3]) |>
        utils::tail(1) |>
        dplyr::select(-Round, -Stage)
      round_scores <- purrr::map2(
        purrr::map_dbl(order(), ~ as.integer(input[[paste0(., "BR")]])),
        tricks,
        ~ calc_points(.x, .y)
      ) |>
        purrr::set_names(order())
      curr_scores <- (if (nrow(prev_scores) > 0) {
        prev_scores + round_scores
      } else {
        round_scores
      }) |>
        list(
          Round = read_csv_q(round_path)$Round,
          Stage = stages[3]
        ) |>
        purrr::flatten()
      readr::read_csv(table_path) |>
        dplyr::bind_rows(tricks |>
          list(
            Round = read_csv_q(round_path)$Round,
            Stage = stages[2]
          ) |>
          purrr::flatten()) |>
        dplyr::bind_rows(curr_scores) |>
        readr::write_csv(table_path)

      if (read_csv_q(round_path)$Round < 12) {
        read_csv_q(round_path) |>
          dplyr::mutate(Round = Round + 1) |>
          readr::write_csv(round_path)

        exportTestValues(round = read_csv_q(round_path)$Round)

        shinyjs::showElement("betting")
        shinyjs::hideElement("playing")

        tracked_losses <- loss_tracker(readr::read_csv(table_path))
        if (!is.na(tracked_losses)) {
          shinyWidgets::sendSweetAlert(session,
            type = "info", title = "",
            text = tracked_losses
          )
        }
      } else {
        shinyjs::hideElement("playing")

        result <- readr::read_csv(table_path)
        game_id <- read_game_id(game_id_path)

        save_game(result, game_id, in_progress = FALSE)

        final_scores <- result |>
          utils::tail(1) |>
          dplyr::select(-Round, -Stage) |>
          tidyr::gather("Player", "Final Score") |>
          tibble::column_to_rownames("Player") |>
          dplyr::arrange(dplyr::desc(`Final Score`)) |>
          dplyr::mutate(Rank = Rank(`Final Score`), .before = 1)

        output$final_scores <- renderTable(
          rownames = TRUE,
          digits = 0,
          spacing = "l",
          bordered = TRUE,
          {
            final_scores
          }
        )

        output$end_message <- renderText({
          paste0(
            "Congratulations ",
            final_scores |>
              dplyr::filter(Rank == 1) |>
              dplyr::pull(Player) |>
              paste(collapse = " and "),
            "!"
          )
        })

        showModal(end_modal())
      }
    } else {
      shinyWidgets::sendSweetAlert(session,
        title = "Error",
        text = "# of tricks declared doesn't equal the total for this round",
        type = "error"
      )
    }
  })

  table_import <- eventReactive(values$c, {
    readr::read_csv(table_path)
  })

  output$play_table <- function() {
    col_order <- c(
      "Round",
      purrr::map(order(), ~ get_col_order(., stages)) |>
        unlist()
    )
    groups <- c(1, rep(3, num_players()), 1, 1) |>
      purrr::set_names(c(" ", order(), " ", " "))
    col_names <- c(
      "Round", rep(stages, num_players()),
      "Cards", "Suit"
    )
    out_tab <- table_import() |>
      tidyr::gather(key = "Player", value = "temp", -Round, -Stage) |>
      tidyr::unite(TEMP, Player, Stage) |>
      tidyr::spread(TEMP, temp) |>
      dplyr::select(dplyr::all_of(col_order)) |>
      dplyr::mutate(Round = Round + 1) |>
      dplyr::slice(-1) |>
      dplyr::mutate(
        Cards = card_seq(Round),
        Suit = rep(trump_opts, 3)[Round]
      ) |>
      kableExtra::kable(
        format = "html",
        digits = 0,
        col.names = col_names,
        escape = FALSE
      ) |>
      kableExtra::kable_styling(bootstrap_options = c("bordered", "striped")) |>
      kableExtra::row_spec(0,
        font_size = if (num_players() > 6) 9.7 else NULL
      ) |>
      kableExtra::add_header_above(
        groups,
        font_size = if (num_players() > 6) 14 else NULL
      )

    if (table_import() |> dplyr::pull(Round) |> max() == -1) {
      out_tab
    } else {
      out_tab |>
        kableExtra::column_spec(
          c(1, which(col_names == "Cards"), which(col_names == "Suit")),
          width = "2cm"
        ) |>
        kableExtra::column_spec(
          seq(3, as.numeric(num_players()) * 3, 3) + 1,
          bold = TRUE
        )
    }
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

get_saved_game_dir <- function() {
  "saved_games"
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
