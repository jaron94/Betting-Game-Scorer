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
          score = cumsum(calc_points(.data$bid, .data$tricks)),
          win = .data$bid == .data$tricks
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
      values_from = dplyr::all_of(c("bid", "tricks", "score", "win")),
      names_glue = "{player}_{.value}",
      names_vary = "slowest"
    ) |>
    dplyr::mutate(
      Cards = card_seq(Round),
      Suit = rep(private$trump_order, 3)[Round]
    )
}

.play_table <- function(self, private) {
  player_names <- self$get_player_names()

  cols_abbrev <- c(bid = "B", tricks = "T", score = "S")

  base <- self$output_table() |>
    dplyr::rename_with(
      \(name) stringr::str_replace_all(name, cols_abbrev),
      .cols = dplyr::starts_with(player_names)
    ) |>
    gt::gt(id = "play_table") |>
    gt::data_color(
      columns = gt::ends_with("_win"),
      target_columns = gt::ends_with("_S"),
      fn = scales::col_factor(
        palette = c("red", "green"),
        domain = c(FALSE, TRUE),
        na.color = "transparent"
      )
    ) |>
    gt::fmt_passthrough(Suit, escape = FALSE) |>
    gt::cols_hide(columns = gt::ends_with("_win")) |>
    gt::cols_align(align = "center") |>
    gt::sub_missing(missing_text = "") |>
    gt::tab_style(
      style = gt::cell_text(whitespace = "pre-line"),
      locations = gt::cells_body(gt::starts_with(player_names))
    )
  
  landscape <- base |>
    gt::tab_spanner_delim("_", gt::starts_with(player_names)) |>
    gt::cols_width(-c(Round, Cards, Suit) ~ px(30))
  
  portrait <- base |>
    gt::cols_label_with(columns = gt::ends_with("_S"), fn = \(x) sub("_S", "", x)) |>
    gt::cols_hide(columns = gt::matches("(_B|_T)$"))
  
  dplyr::lst(portrait, landscape)
}


.loss_tracker <- function(self, private) {
  tracked_losses <- self$calc_table() |>
    dplyr::nest_by(.data$player) |>
    dplyr::mutate(rle = list(
      rle(.data$data$win) |> c() |> purrr::map(\(x) utils::tail(x, 1)) # nolint unnecessary_lambda_linter
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

.curr_score <- function(self, private) {
  self$calc_table() |>
    dplyr::filter(.data$Round == max(.data$Round)) |>
    dplyr::select(-"Round") |>
    tibble::column_to_rownames("player") |>
    dplyr::arrange(dplyr::desc(.data$score)) |>
    dplyr::mutate(
      Rank = dplyr::min_rank(dplyr::desc(.data$score)),
      .before = 1
    ) |>
    dplyr::select("Rank", Score = "score")
}
