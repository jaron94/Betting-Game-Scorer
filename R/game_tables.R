.calc_table <- function(self, private) {
  purrr::map(
    private$players,
    \(x) {
      bids <- x$get_bids()
      ntricks <- x$get_ntricks()
      if (purrr::is_empty(bids)) bids <- NA_integer_
      if (length(bids) > length(ntricks)) ntricks <- c(ntricks, NA_integer_)

      data.frame(
        Round = seq_along(bids),
        bid = bids,
        tricks = ntricks
      ) |>
        dplyr::mutate(
          score = cumsum(.data$tricks + (.data$bid == .data$tricks) * 10)
        )
    }
  ) |>
    purrr::set_names(self$get_player_names()) |>
    purrr::list_rbind(names_to = "player")
}

.output_table <- function(self, private) {
  self$calc_table() |>
    tidyr::pivot_wider(
      names_from = "player",
      values_from = dplyr::all_of(c("bid", "tricks", "score")),
      names_glue = "{player}_{.value}",
      names_vary = "slowest"
    ) |>
    dplyr::mutate(
      Cards = card_seq(Round),
      Suit = rep(private$trump_order, 3)[Round]
    )
}


.play_table <- function(self, private) {
  # Set the Kable options to display missing values as empty strings
  withr::local_options(knitr.kable.NA = "")
  num_players <- self$num_players()

  groups <- c(1, rep(3, num_players), 1, 1) |>
    purrr::set_names(c(" ", self$get_player_names(), " ", " "))
  col_names <- c("Round", rep(private$stages, num_players), "Cards", "Suit")

  self$output_table() |>
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


.loss_tracker <- function(self, private) {
  tracked_losses <- self$calc_table() |>
    dplyr::mutate(win = .data$bid == .data$tricks) |>
    dplyr::nest_by(.data$player) |>
    dplyr::mutate(rle = list(
      rle(.data$data$win) |> c() |> purrr::map(\(x) utils::tail(x, 1))
    )) |>
    dplyr::select(-"data") |>
    tidyr::unnest_wider("rle") |>
    dplyr::filter(lengths >= 3, !.data$values)

  if (nrow(tracked_losses) == 0) {
    return()
  }

  tracked_losses |>
    dplyr::mutate(
      msg = paste(.data$player, "has lost", .data$lengths, "times in a row")
    ) |>
    dplyr::pull("msg") |>
    paste(collapse = "\n")
}


.calc_final_score <- function(self, private) {
  self$calc_table() |>
    dplyr::filter(.data$Round == max(.data$Round)) |>
    dplyr::select(-"Round") |>
    tibble::column_to_rownames("player") |>
    dplyr::arrange(dplyr::desc(.data$score)) |>
    dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(.data$score)),
                  .before = 1) |>
    dplyr::rename("Final Score" = "score")
}
