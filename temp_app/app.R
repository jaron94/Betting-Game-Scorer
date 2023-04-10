devtools::load_all()
library(gargoyle)

saved_game_dir <- "saved_games"

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num_players", "No. of players", 3, min = 2, max = 10),
      textInput("P1", "Player1"),
      textInput("P2", "Player2"),
      textInput("P3", "Player3"),
      actionButton("setup", "Set up"),
      actionButton("reload", "Reload"),
      selectInput("saved_game_id", "Select saved game",
                  choices = tools::file_path_sans_ext(list.files(saved_game_dir))),
      actionButton("next_round", "Next Round"),
      numericInput("bid1", "Bid 1", 0, min = 0, max = 7),
      numericInput("bid2", "Bid 2", 0, min = 0, max = 7),
      numericInput("bid3", "Bid 3", 0, min = 0, max = 7),
      actionButton("set_bids", "Set Bids"),
      numericInput("score1", "Score 1", 0, min = 0, max = 7),
      numericInput("score2", "Score 2", 0, min = 0, max = 7),
      numericInput("score3", "Score 3", 0, min = 0, max = 7),
      actionButton("set_scores", "Set Scores"),
      actionButton("save_game", "Save Game")
    ),
    
    mainPanel(
      verbatimTextOutput("sitch")
    )
  )
)

server <- function(input, output, session) {
  
  game <- Game$new(Sys.time() |> as.numeric() |> as.character())
  
  init("update_game")
  
  observeEvent(input$setup, {
    
    player_names <- purrr::map_chr(seq_len(input$num_players),
                                   \(x) input[[paste0("P", x)]])
    
    if (any(player_names == "")) {
      shinyWidgets::sendSweetAlert(session,
                                   type = "error", title = "Error",
                                   text = "Names not recorded for all players")
    }
    
    purrr::walk(player_names, req)
    
    players <- purrr::map(player_names, \(x) Player$new(x))
    
    game$add_players(players)
    
    if (isTruthy(input$game_id)) {
      game$change_id(input$game_id)
    }
    
    trigger("update_game")
    
    game$save(saved_game_dir)
  })
  
  observeEvent(input$reload, {
    game_id <- input$saved_game_id
    req(game_id)
    game$load(game_id, saved_game_dir)
    trigger("update_game")
  })
  
  observeEvent(input$set_bids, {
    game$record_bids(input)
    game$advance()
    trigger("update_game")
  })
  
  observeEvent(input$set_scores, {
    game$record_scores(input)
    game$advance()
    trigger("update_game")
  })
  
  observeEvent(input$next_round, {
    game$next_round()
    trigger("update_game")
  })
  
  output$sitch <- renderPrint({
    watch("update_game")
    game
  })
  
  observeEvent(input$save_game, {
    watch("update_game")
    game$save(saved_game_dir)
  })
  
}

shinyApp(ui = ui, server = server)
