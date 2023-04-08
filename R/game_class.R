library(shiny)

reactive_trigger <- function() {
  rv <- shiny::reactiveVal(0)
  list(
    depend = function() {
      invisible(rv())
    },
    trigger = function() {
      rv(isolate(rv() + 1))
    }
  )
}

React <- R6::R6Class(
  "React",
  private = list(
    id = "",
    rx_trigger = NULL,
    depend = function() {
      if (!is.null(private$rx_trigger)) private$rx_trigger$depend()
    },
    trigger = function() {
      if (!is.null(private$rx_trigger)) private$rx_trigger$trigger()
    }
  ),
  public = list(
    initialize = function(id, reactive = TRUE) {
      private$id <- id
      if (reactive) {
        private$rx_trigger <- reactive_trigger()
      }
    },
    change_id = function(new_id) {
      private$id <- new_id
      private$trigger()
    },
    get_id = function() {
      private$depend()
      private$id
    }
  )
)

Player <- R6::R6Class(
  "Player",
  inherit = React,
  private = list(
    bids = integer(),
    scores = integer()
  ),
  public = list(
    print = function() {
      private$depend()
      cat("Player:", private$id)
    },
    record_bid = function(bid) {
      private$bids <- append(private$bids, bid)
      private$trigger()
      invisible(self)
    },
    record_score = function(score) {
      private$scores <- append(private$scores, score)
      private$trigger()
      invisible(self)
    },
    get_bids = function() {
      private$depend()
      private$bids
    },
    get_scores = function() {
      private$depend()
      private$scores
    }
  )
)

Game <- R6::R6Class(
  "Game",
  inherit = React,
  private = list(
    players = list(),
    round = 0,
    bid_stage = TRUE
  ),
  public = list(
    get_player_names = function() {
      private$depend()
      if (!is.null(private$players)) purrr::map_chr(private$players, \(x) x$get_id())
    },
    get_player = function(pos) {
      private$depend()
      private$players[[pos]]
    },
    print = function() {
      private$depend()
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
      to_add <- list(player)
      names(to_add) <- player$get_id()
      private$players <- append(private$players, to_add)
      private$trigger()
      invisible(self)
    },
    add_players = function(players) {
      purrr::walk(players, \(x) self$add_player(x))
    },
    num_players = function() {
      length(private$players)
    },
    next_round = function() {
      private$round <- private$round + 1
      private$trigger()
      invisible(self)
    },
    advance = function() {
      if (!private$bid_stage) {
        self$next_round()
      }
      private$bid_stage <- !private$bid_stage
      private$trigger()
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
    }
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("num_players", "No. of players", 3, min = 2, max = 10),
      textInput("P1", "Player1"),
      textInput("P2", "Player2"),
      textInput("P3", "Player3"),
      actionButton("setup", "Set up"),
      numericInput("bid1", "Bid 1", 0, min = 0, max = 7),
      numericInput("bid2", "Bid 2", 0, min = 0, max = 7),
      numericInput("bid3", "Bid 3", 0, min = 0, max = 7),
      actionButton("set_bids", "Set Bids"),
      numericInput("score1", "Score 1", 0, min = 0, max = 7),
      numericInput("score2", "Score 2", 0, min = 0, max = 7),
      numericInput("score3", "Score 3", 0, min = 0, max = 7),
      actionButton("set_scores", "Set Scores")
    ),
    mainPanel(
      verbatimTextOutput("sitch")
    )
  )
)

server <- function(input, output, session) {
  game <- Game$new("new_game")

  observeEvent(input$setup, once = TRUE, {
    players <- purrr::map(
      seq_len(input$num_players),
      \(x) Player$new(input[[paste0("P", x)]])
    )
    game$add_players(players)
  })

  observeEvent(input$set_bids, {
    game$record_bids(input)
    game$advance()
  })

  observeEvent(input$set_scores, {
    game$record_scores(input)
    game$advance()
  })

  output$sitch <- renderPrint({
    game
  })
}

shinyApp(ui = ui, server = server)
