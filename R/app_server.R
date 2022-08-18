utils::globalVariables(
  c("Round", "Table", "Final Score", "player", "temp", "TEMP", "Stage",
    "Player")
)

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Create identifiers for the three columns on the scorecard:
  # bidding, tricks and score
  stages <- c("B", "T", "S")
  
  table_path <- system.file("table.csv", package = "bgScorer", mustWork = TRUE)
  round_path <- system.file("round.csv", package = "bgScorer", mustWork = TRUE)
  
  values <- reactiveValues(a = NULL, b = NULL, c = NULL)
  
  showModal(startup_modal())
  
  observeEvent(input$set_up, {
    if(any(purrr::map_lgl(1:input$num_players,
                   ~input[[paste0("P", .)]] == ""))) {
      shinyWidgets::sendSweetAlert(session, type = "error", title = "Error", 
                     text = "Names not recorded for all players")
    } else {
      values$a <- 1
      values$b <- 1
      values$c <- 1
    }
  })
  
  observeEvent(input$reload, {
    values$a <- 1
    values$b <- 1
    values$c <- 1
  })
  
  observeEvent(input$score, {
    values$b <- 3
    values$b <- 4
    values$c <- 3
    values$c <- 4
  })
  
  observeEvent(input$bet, {
    values$c <- 3
    values$c <- 4
  })
  
  output$player_inputs <- renderUI({
    purrr::map(2:input$num_players, 
        ~column(6, textInput(paste0("P", .), "Enter Player Name")))
  })
  
  observeEvent(input$set_up, priority = 1, {
    if(any(purrr::map_lgl(1:input$num_players,
                   ~input[[paste0("P", .)]] == ""))) {
      shinyWidgets::sendSweetAlert(session, type = "error", 
                     text = "Names not recorded for all players")
    } else {
      
      removeModal()
      
      matrix(0, ncol = as.integer(input$num_players), nrow = 3) %>%
        magrittr::set_colnames(
          purrr::map_chr(1:input$num_players, ~input[[paste0("P", .)]])
        ) %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(tibble::tibble(Round = -1, Stage = stages)) %>%
        readr::write_csv(table_path)
      
      readr::write_csv(tibble::tibble(Round = as.integer(0)), round_path)
    }
  })
  
  observeEvent(input$reload, {
    removeModal()
  })
  
  num_players <- eventReactive(values$a, {
    if(input$reload == 0) {
      input$num_players
    } else {
      readr::read_csv(table_path) %>%
        dplyr::select(-Round, -Stage) %>%
        ncol()
    }
  })
  
  order <- eventReactive(values$a, {
    if(input$reload == 0) {
      purrr::map_chr(1:num_players(), ~input[[paste0("P", .)]])
    } else {
      tab <- readr::read_csv(table_path)
      tab %>% dplyr::select(-Round, -Stage) %>% colnames() %>%
        shifter(n = tab$Round %>% utils::tail(1) %>% magrittr::subtract(1))
      
      
    }
  })
  
  shifted_order <- eventReactive(values$b, {
    order() %>% shifter(n = read_csv_q(round_path)$Round)
  })
  
  observeEvent(values$b, {
    
    output$round_info <- renderUI({
      round <- read_csv_q(round_path)$Round + 1
      trumps <- rep(c("&spades;", "&hearts;", "&diams;", "&clubs;", ""), 3)[round]
      HTML(paste0("Round ", round, ": ", card_seq(round), " cards. ", 
                  if(trumps == "") "No Trumps" else paste("Trumps are", trumps)))
    })
  })
  
  observeEvent(values$a, {
    
    output$betting <- renderUI({
      tagList(
        purrr::map(shifted_order(), 
            ~shinyWidgets::pickerInput(paste0(., "BR"), 
                         paste0(., " bids?"),
                         choices = c("", 0:card_seq(read_csv_q(round_path)$Round+1)))),
        actionButton("bet", "Enter Bids")
      )
    })
    
    
    
    
  })
  
  observeEvent(values$a, {
    
    output$playing <- renderUI({
      tagList(
        purrr::map(shifted_order(), 
            ~shinyWidgets::pickerInput(paste0(., "PR"), 
                         paste0(., ": how many tricks?"),
                         choices = c("", 0:card_seq(read_csv_q(round_path)$Round+1)))),
        actionButton("score", "Enter Results")
      )
    })
  })
  
  observeEvent(input$bet, {
    round <- read_csv_q(round_path)$Round
    bids <- purrr::map(order(), ~as.integer(input[[paste0(., "BR")]])) %>%
      purrr::set_names(order())
    if(any(is.na(unlist(bids)))) {
      shinyWidgets::sendSweetAlert(session, title = "Error", text = "Not all players have bid",
                     type = "error")
    } else if(as.integer(bids) %>% sum(na.rm = T) %>% 
              magrittr::equals(card_seq(round+1))) {
      shinyWidgets::sendSweetAlert(session, title = "Error", text = "You are currently exactly bid",
                     type = "error")
    } else {
      readr::read_csv(table_path) %>%
        dplyr::bind_rows(bids %>%
                    list(Round = round, Stage = stages[1]) %>%
                    purrr::flatten()) %>%
        readr::write_csv(table_path)
      
      shinyjs::hideElement("betting")
      shinyjs::showElement("playing")
    }
    
  })
  
  observeEvent(input$score, {
    round <- read_csv_q(round_path)$Round
    tricks <- purrr::map(order(), ~as.integer(input[[paste0(., "PR")]])) %>%
      purrr::set_names(order())
    if(any(is.na(unlist(tricks)))) {
      shinyWidgets::sendSweetAlert(session, title = "Error", 
                     text = "Tricks have not recorded for all players",
                     type = "error")
    } else if(unlist(tricks) %>% sum(na.rm = T) %>% 
              magrittr::equals(card_seq(round+1))) {
      prev_scores <- readr::read_csv(table_path) %>%
        dplyr::filter(Stage == stages[3]) %>%
        utils::tail(1) %>%
        dplyr::select(-Round, -Stage)
      round_scores <- purrr::map2(purrr::map_dbl(order(), ~as.integer(input[[paste0(., "BR")]])),
                           tricks,
                           ~calc_points(.x, .y)) %>%
        purrr::set_names(order())
      curr_scores <- (if(nrow(prev_scores) > 0) prev_scores + round_scores else round_scores) %>%
        list(Round = read_csv_q(round_path)$Round, 
             Stage = stages[3]) %>%
        purrr::flatten()
      readr::read_csv(table_path) %>%
        dplyr::bind_rows(tricks %>% 
                    list(Round = read_csv_q(round_path)$Round, 
                         Stage = stages[2]) %>%
                    purrr::flatten()) %>%
        dplyr::bind_rows(curr_scores) %>%
        readr::write_csv(table_path)
      
      if(read_csv_q(round_path)$Round < 12) {
        read_csv_q(round_path) %>%
          dplyr::mutate(Round = Round + 1) %>%
          readr::write_csv(round_path)
        
        shinyjs::showElement("betting")
        shinyjs::hideElement("playing")
        
        tracked_losses <- loss_tracker(readr::read_csv(table_path))
        if(!is.na(tracked_losses)) {
          shinyWidgets::sendSweetAlert(session, type = "info", title = "", 
                         text = tracked_losses)
        }
        
      } else {
        shinyjs::hideElement("playing")
        final_scores <- readr::read_csv(table_path) %>%
          utils::tail(1) %>% dplyr::select(-Round, -Stage) %>%
          t() %>%
          magrittr::set_colnames("Final Score") %>%
          tibble::as_tibble(rownames = "Player") %>%
          dplyr::arrange(dplyr::desc(`Final Score`)) %>%
          dplyr::mutate(Rank = Rank(`Final Score`))
        
        output$final_scores <- 
          renderTable(rownames = TRUE, digits = 0, spacing = "l", bordered = TRUE, {
            final_scores %>%
              dplyr::select(Player, Rank, `Final Score`) %>%
              tibble::column_to_rownames("Player")
          })
        
        output$end_message <- renderText({
          paste0("Congratulations ", 
                 final_scores %>% dplyr::filter(Rank == 1) %>% dplyr::pull(Player) %>% 
                   paste(collapse = " and "), "!")
        })
        
        showModal(end_modal())
        
      }
      
    } else {
      shinyWidgets::sendSweetAlert(session, title = "Error", 
                     text = "The number of tricks declared doesn't equal the total for this round",
                     type = "error")
    }
  })
  
  table_import <- eventReactive(values$c, {
    readr::read_csv(table_path)
  })
  
  output$play_table <- function() {
    col_order <- c("Round", purrr::map(order(), ~get_col_order(., stages)) %>% unlist())
    groups <- c(1, rep(3, num_players()), 1, 1) %>%
      purrr::set_names(c(" ", order(), " ", " "))
    col_names <- c("Round", rep(stages, num_players()), 
                   "Cards", "Suit")
    out_tab <- table_import() %>%
      tidyr::gather(key = "Player", value = "temp", -Round, -Stage) %>%
      tidyr::unite(TEMP, Player, Stage) %>%
      tidyr::spread(TEMP, temp) %>%
      dplyr::select(dplyr::all_of(col_order)) %>%
      dplyr::mutate(Round = Round + 1) %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Cards = card_seq(Round)) %>%
      dplyr::mutate(Suit = rep(c("&spades;", "&hearts;", "&diams;", "&clubs;", ""), 3)[Round]) %>%
      kableExtra::kable(format = "html", digits = 0, col.names = col_names, escape=F) %>%
      kableExtra::kable_styling(bootstrap_options = c("bordered", "striped")) %>%
      kableExtra::row_spec(0, font_size = if(num_players() > 6) 9.7 else NULL) %>%
      kableExtra::add_header_above(groups, font_size = if(num_players() > 6) 14 else NULL)
    
    if(table_import() %>% dplyr::pull(Round) %>% max() == -1) {
      out_tab
    } else {
      out_tab %>%
        kableExtra::column_spec(c(1, which(col_names == "Cards"), which(col_names == "Suit")), 
                    width = "2cm") %>%
        kableExtra::column_spec(seq(3, as.numeric(num_players()) * 3, 3) + 1, bold = T)
    }
  }
}

# Function to create a vector of column names in the correct order for the table
get_col_order <- function(name, stages) {
  paste(name, stages, sep = "_")
}

Rank <- function(d) {
  j <- unique(rev(sort(d)));
  return(sapply(d,function(dd) which(dd==j)));
}

shifter <- function(x, n = 1) {
  mod_n <- n %% length(x)
  if(mod_n == 0) x else c(utils::tail(x, -mod_n), utils::head(x, mod_n))
}

calc_points <- function(bid, tricks) {
  tricks +
    if(bid == tricks) 10 else 0
}

loss_tracker <- function(tab) {
  temp <- tab %>%
    dplyr::filter(Round != -1) %>%
    dplyr::group_by(Round) %>%
    dplyr::summarise_if(is.numeric, ~.[2] == .[1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Round) %>%
    dplyr::summarise_all(~list(tibble::tibble(lengths = rle(.)$lengths, values = rle(.)$values))) %>%
    tidyr::gather(key = "player", value = "rle") %>%
    dplyr::filter(purrr::map_lgl(rle, ~!utils::tail(.$values, 1))) %>%
    dplyr::filter(purrr::map_lgl(rle, ~utils::tail(.$lengths, 1) >= 3)) %>%
    dplyr::mutate(rle = purrr::map(rle, ~dplyr::filter(., lengths >= 3, !values))) %>%
    dplyr::group_split(player)
  
  if(length(temp) > 0) {
    purrr::map_chr(temp, ~paste(.$player, "has lost", .$rle[[1]]$lengths, "times in a row")) %>%
      paste(collapse = "\n")
  } else {
    NA_character_
  }
  
}
