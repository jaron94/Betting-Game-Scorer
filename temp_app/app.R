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
      send_error_alert(text = "Names not recorded for all players")
    }
    
    purrr::walk(player_names, req)
    
    players <- purrr::map(player_names, \(x) Player$new(x))
    
    game$add_players(players)
    
    if (isTruthy(input$game_id)) {
      game$change_id(input$game_id)
    }
    
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
  
  observeEvent(input$bet, {
    bids <- purrr::map_int(game$get_player_names(),
                           \(name) as.integer(input[[paste0(name, "BR")]]))
    
    req(tryCatch(
      game$record_bids(bids),
      error = function(e) {
        send_error_alert(e$message)
        return(FALSE)
      }
    ))
    
    trigger("update_game")
    shinyjs::hideElement("betting")
    shinyjs::showElement("playing")
  })
  
  observeEvent(input$score, {
    tricks <- purrr::map_int(game$get_player_names(),
                             \(name) as.integer(input[[paste0(name, "PR")]]))
    
    req(tryCatch(
      game$record_tricks(tricks),
      error = function(e) {
        send_error_alert(e$message)
        return(FALSE)
      }
    ))
    
    trigger("update_game")
    shinyjs::hideElement("playing")
    shinyjs::showElement("betting")
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
  
  stages <- c("B", "T", "S")
  trump_opts <- c("&spades;", "&hearts;", "&diams;", "&clubs;", "")
  
  output$play_table <- function() {
    watch("update_game")
    
    curr_round <- game$get_round()
    
    if (curr_round < 1) {
      return()
    }
    
    num_players <- game$num_players()
    
    groups <- c(1, rep(3, num_players), 1, 1) |>
      purrr::set_names(c(" ", game$get_player_names(), " ", " "))
    col_names <- c("Round", rep(stages, num_players), "Cards", "Suit")
    
    output_table(game) |>
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
  
}

shinyApp(ui = app_ui, server = server)
