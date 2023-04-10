devtools::load_all()
library(gargoyle)

saved_game_dir <- get_saved_game_dir()

ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      numericInput("num_players", "No. of players", 3, min = 2, max = 10),
      textInput("P1", "Player1"),
      textInput("P2", "Player2"),
      textInput("P3", "Player3"),
      actionButton("set_up", "Set up"),
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
  
  showModal(startup_modal())
  
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
  
  observeEvent(input$set_up, {
    
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
    
    game$next_round()
    
    trigger("update_game")
    
    game$save(saved_game_dir)
    
    removeModal()
  })
  
  observeEvent(input$reload, {
    game_id <- input$saved_game_id
    req(game_id)
    game$load(game_id, saved_game_dir)
    trigger("update_game")
    removeModal()
  })
  
  send_error_alert <- function(text, session = getDefaultReactiveDomain()) {
    shinyWidgets::sendSweetAlert(
      session,
      title = "Error",
      text = text,
      type = "error"
    )
  }
  
  observeEvent(input$bet, {
    bids <- purrr::map_int(game$get_player_names(),
                           \(name) as.integer(input[[paste0(name, "BR")]]))
    
    if (any(is.na(bids))) {
      send_error_alert("Not all players have bid")
      return()
    }
    
    if (sum(bids) == game$num_cards()) {
      send_error_alert("You are currently exactly bid")
      return()
    }
    
    game$record_bids(bids)
    game$advance()
    trigger("update_game")
    shinyjs::hideElement("betting")
    shinyjs::showElement("playing")
  })
  
  observeEvent(input$score, {
    tricks <- purrr::map_int(game$get_player_names(),
                             \(name) as.integer(input[[paste0(name, "PR")]]))
    
    if (any(is.na(tricks))) {
      send_error_alert("Tricks have not been recorded for all players")
      return()
    }
    
    if (sum(tricks) != game$num_cards()) {
      send_error_alert("# of tricks declared doesn't equal the total for this round")
      return()
    }
    
    game$record_tricks(tricks)
    game$advance()
    trigger("update_game")
    shinyjs::hideElement("playing")
    shinyjs::showElement("betting")
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
  
  output$betting <- renderUI({
    watch("update_game")
    create_game_inputs(game, TRUE)
  })
  
  output$playing <- renderUI({
    watch("update_game")
    create_game_inputs(game, FALSE)
  })
  
}

shinyApp(ui = app_ui, server = server)
