devtools::load_all()

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
  
  observeEvent(input$setup, {
    player_names <- purrr::map_chr(seq_len(input$num_players),
                                   \(x) input[[paste0("P", x)]])
    
    if (any(player_names == "")) {
      shinyWidgets::sendSweetAlert(session,
                                   type = "error", title = "Error",
                                   text = "Names not recorded for all players")
    } else {
      players <- purrr::map(player_names, \(x) Player$new(x))
    }
    
    game$add_players(players)
  })
  
  observeEvent(input$reload, {
    game_id <- input$saved_game_id
    if (isTruthy(game_id))
    game <<- readRDS(game_id)
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
