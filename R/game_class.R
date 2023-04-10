Base <- R6::R6Class(
  "Base",
  private = list(
    id = ""
  ),
  public = list(
    initialize = function(id) {
      private$id <- id
    },
    get_id = function() {
      private$id
    },
    change_id = function(new_id) {
      private$id <- new_id
    }
  )
)

Player <- R6::R6Class(
  "Player",
  inherit = Base,
  private = list(
    bids = integer(),
    ntricks = integer(),
    scores = integer()
  ),
  public = list(
    print = function() {
      cat("Player:", private$id)
    },
    record_bid = function(bid) {
      private$bids <- append(private$bids, bid)
      invisible(self)
    },
    record_ntricks = function(ntricks) {
      private$ntricks <- append(private$ntricks, ntricks)
      invisible(self)
    },
    get_bids = function() {
      private$bids
    },
    get_ntricks = function() {
      private$ntricks
    }
  )
)

Game <- R6::R6Class(
  "Game",
  inherit = Base,
  private = list(
    players = list(),
    round = 0,
    bid_stage = TRUE,
    order = character()
  ),
  public = list(
    get_player_names = function() {
      if (!is.null(private$players)) purrr::map_chr(private$players, \(x) x$get_id())
    },
    get_player = function(pos) {
      private$players[[pos]]
    },
    print = function() {
      cat("Game:", private$id, "\n")
      if (self$num_players() > 0) {
        cat("Players:", toString(self$get_player_names()), "\n")
      }
      if (private$round >= 0) {
        cat("Round:", private$round, "\n")
      }
      if (private$round >= 1 || !private$bid_stage) {
        cat("Bids:", purrr::map(private$players, \(x) x$get_bids()) |> unlist() |> toString(), "\n")
      }
      if (private$round >= 1) {
        cat("Scores:", purrr::map(private$players, \(x) x$get_ntricks()) |> unlist() |> toString(), "\n")
      }
    },
    add_player = function(player) {
      private$order <- c(private$order, player$get_id())
      private$players <- append(private$players, player)
      invisible(self)
    },
    add_players = function(players) {
      if (self$num_players() > 0) {
        stop("This game already has players")
      }
      purrr::walk(players, \(x) self$add_player(x))
      self$next_round()
    },
    num_players = function() {
      length(private$players)
    },
    get_round = function() {
      private$round
    },
    get_bid_stage = function() {
      private$bid_stage
    },
    next_round = function() {
      private$order <- shifter(private$order, private$round)
      private$round <- private$round + 1
      invisible(self)
    },
    advance = function() {
      if (!private$bid_stage) {
        self$next_round()
      }
      private$bid_stage <- !private$bid_stage
      invisible(self)
    },
    record_bids = function(bids) {
      if (any(is.na(bids))) stop("Not all players have bid")
      if (sum(bids) == self$num_cards()) stop("You are currently exactly bid")
      
      purrr::walk2(
        private$players, bids,
        \(player, bid) player$record_bid(bid)
      )
      self$advance()
      invisible(self)
    },
    record_tricks = function(tricks) {
      if (any(is.na(tricks))) {
        stop("Tricks have not been recorded for all players")
      }
      
      if (sum(tricks) != self$num_cards()) {
        stop("# of tricks declared doesn't equal the total for this round")
      }
      
      purrr::walk2(
        private$players, tricks,
        \(player, ntricks) player$record_ntricks(ntricks)
      )
      self$advance()
      invisible(self)
    },
    get_order = function() {
      private$order
    },
    num_cards = function() {
      card_seq(private$round)
    },
    calc_table = function() {
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
            dplyr::mutate(score = tricks + (bid == tricks) * 10)
        }
      ) |>
        purrr::set_names(self$get_player_names()) |>
        purrr::list_rbind(names_to = "player")
    },
    loss_tracker = function() {
      tracked_losses <- self$calc_table() |>
        dplyr::mutate(win = bid == tricks) |>
        dplyr::nest_by(player) |>
        dplyr::mutate(rle = list(rle(data$win) |> c() |> purrr::map(\(x) tail(x, 1)))) |>
        dplyr::select(-data) |>
        tidyr::unnest_wider(rle) |>
        dplyr::filter(lengths >= 3, !values)
      
      if (nrow(tracked_losses) == 0) {
        return()
      }
      
      tracked_losses |>
        dplyr::mutate(
          msg = paste(player, "has lost", lengths, "times in a row")
        ) |>
        dplyr::pull(msg) |>
        paste(collapse = "\n")
      
    },
    save = function(...) {
      saveRDS(self, file.path(..., paste0(private$id, ".rds")))
    },
    load = function(id, ...) {
      loaded <- readRDS(file.path(..., paste0(id, ".rds")))$clone(deep = TRUE)
      private$round <- loaded$get_round()
      self$add_players(purrr::map(seq_len(loaded$num_players()),
                                  \(pos) loaded$get_player(pos)$clone(deep = TRUE)))
      private$bid_stage <- loaded$get_bid_stage()
    }
  )
)
