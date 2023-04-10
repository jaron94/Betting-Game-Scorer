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
    record_score = function(score) {
      private$scores <- append(private$scores, score)
      invisible(self)
    },
    get_bids = function() {
      private$bids
    },
    get_scores = function() {
      private$scores
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
        cat("Scores:", purrr::map(private$players, \(x) x$get_scores()) |> unlist() |> toString(), "\n")
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
      private$round <- private$round + 1
      private$order <- shifter(private$order)
      invisible(self)
    },
    advance = function() {
      if (!private$bid_stage) {
        self$next_round()
      }
      private$bid_stage <- !private$bid_stage
      invisible(self)
    },
    record_bids = function(input) {
      for (i in seq_len(self$num_players())) {
        self$get_player(i)$record_bid(input[[paste0("bid", i)]])
      }
      invisible(self)
    },
    record_scores = function(input) {
      for (i in seq_len(self$num_players())) {
        self$get_player(i)$record_score(input[[paste0("score", i)]])
      }
      invisible(self)
    },
    get_order = function() {
      private$order
    },
    num_cards = function() {
      card_seq(private$round)
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
