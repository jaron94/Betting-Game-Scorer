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
    scores = integer(),
    avatar = character()
  ),
  public = list(
    initialize = function(id, avatar = NULL) {
      super$initialize(id)
      private$avatar <- avatar
    },
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
    },
    get_avatar = function() {
      private$avatar
    }
  )
)


Game <- R6::R6Class( # nolint cyclocomp_linter
  "Game",
  inherit = Base,
  private = list(
    created_at = as.POSIXct(character()),
    last_saved_at = as.POSIXct(character()),
    players = list(),
    round = 0,
    bid_stage = TRUE,
    order = character(),
    stages = c("B", "T", "S"),
    trump_order = c("&spades;", "&hearts;", "&diams;", "&clubs;", "")
  ),
  public = list(
    initialize = function(id) {
      super$initialize(id)
      private$created_at <- private$last_saved_at <- Sys.time()
    },
    get_player_names = function() {
      if (!is.null(private$players)) {
        purrr::map_chr(private$players, \(x) x$get_id())
      }
    },
    get_player = function(pos) {
      private$players[[pos]]
    },
    get_player_by_id = function(name) {
      self$get_player(which(self$get_player_names() == name))
    },
    print = function() {
      cat("Game:", private$id, "\n")
      cat("Created at:", private$created_at, "\n")
      cat("Last saved at:", private$last_saved_at, "\n")
      if (self$num_players() > 0) {
        cat("Players:", toString(self$get_player_names()), "\n")
      }
      if (private$round >= 0) {
        cat("Round:", private$round, "\n")
      }
      if (private$round >= 1 || !private$bid_stage) {
        cat("Bids:", purrr::map(
          private$players,
          \(x) x$get_bids()
        ) |>
          unlist() |>
          toString(), "\n")
      }
      if (private$round >= 1) {
        cat("Scores:", purrr::map(
          private$players,
          \(x) x$get_ntricks()
        ) |>
          unlist() |>
          toString(), "\n")
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
      purrr::walk(players, self$add_player)
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
      private$order <- shifter(self$get_player_names(), private$round)
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
      if (anyNA(bids)) stop("Not all players have bid")
      if (sum(bids) == self$num_cards()) stop("You are currently exactly bid")
      if (!private$bid_stage) stop("Cannot record bids outside of bid stage")

      purrr::walk2(
        private$players, bids,
        \(player, bid) player$record_bid(bid)
      )
      self$advance()
      invisible(self)
    },
    record_tricks = function(tricks) {
      if (anyNA(tricks)) {
        stop("Tricks have not been recorded for all players")
      }

      if (sum(tricks) != self$num_cards()) {
        stop("# of tricks declared doesn't equal the total for this round")
      }

      if (private$bid_stage) {
        stop("Tricks cannot be recorded in the bid stage")
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
    trump_suit = function() {
      shifter(private$trump_order, private$round)
    },
    calc_table = function() {
      .calc_table(self, private)
    },
    output_table = function() {
      .output_table(self, private)
    },
    play_table = function() {
      .play_table(self, private)
    },
    loss_tracker = function() {
      .loss_tracker(self, private)
    },
    calc_final_score = function() {
      .calc_final_score(self, private)
    },
    save = function(..., use_gcs = get_golem_config("use_gcs")) {
      file <- file.path(..., paste0(private$id, ".rds"))

      if (!dir.exists(dirname(file))) {
        dir.create(dirname(file), recursive = TRUE)
      }

      private$last_saved_at <- Sys.time()

      saveRDS(self, file)

      if (use_gcs) {
        googleCloudStorageR::gcs_upload(file = file, name = file)
      }
    },
    load = function(id, ..., use_gcs = get_golem_config("use_gcs")) {
      file <- file.path(..., paste0(id, ".rds"))
      loaded <- if (use_gcs) {
        temp_file <- tempfile()
        googleCloudStorageR::gcs_get_object(file, saveToDisk = temp_file)
        on.exit(unlink(temp_file))
        readRDS(temp_file)
      } else {
        readRDS(file)
      }
      self <- loaded
      lockEnvironment(self)
      invisible(self)
    }
  )
)
