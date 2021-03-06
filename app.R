library(shiny)
library(tidyverse)
library(magrittr)
library(shinyjs)
library(kableExtra)
library(shinyWidgets)
library(shinythemes)

# Set the Kable options to display missing values as empty strings
options(knitr.kable.NA = '')

# Create identifiers for the three columns on the scorecard:
# bidding, tricks and score
stages <- c("B", "T", "S")

# Function to determine the number of cards dealt in the round
card_seq <- function(round) {
    c(7:1, 2:7)[round]
}

# Wrapper around read_csv to suppress messages
read_csv_q <- function(file) {suppressMessages(read_csv(file))}

# Function to create the modal dialog on startup to set up the game
startup_modal <- function() {
    modalDialog(
        column(12, align = "center", h3("Game Set-up")),
        column(12, align = "center", 
               pickerInput("num_players", "How many players?", choices = 2:7)),
        column(6, textInput("P1", "Who deals first?")),
        uiOutput("player_inputs"),
        
        footer = tagList(
            actionButton("reload", "Reload Previous Game"),
            actionButton("set_up", "New Game")
        )
    )
}

# Function to create a modal dialog to display the scores at the end
end_modal <- function() {
    modalDialog(easyClose = TRUE,
                fluidRow(
                    column(12, align = "center", 
                           h4(textOutput("end_message")))
                ),
                fluidRow(
                    column(12, align = "center",
                           tableOutput("final_scores")
                    )
                )
    )
}

# Function to create a vector of column names in the correct order for the table
get_col_order <- function(name) {
    paste(name, stages, sep = "_")
}

Rank <- function(d) {
    j <- unique(rev(sort(d)));
    return(sapply(d,function(dd) which(dd==j)));
}

shifter <- function(x, n = 1) {
    mod_n <- n %% length(x)
    if(mod_n == 0) x else c(tail(x, -mod_n), head(x, mod_n))
}

calc_points <- function(bid, tricks) {
    tricks +
    if(bid == tricks) 10 else 0
}

loss_tracker <- function(tab) {
    temp <- tab %>%
        filter(Round != -1) %>%
        group_by(Round) %>%
        summarise_if(is.numeric, ~.[2] == .[1]) %>%
        ungroup() %>%
        select(-Round) %>%
        summarise_all(~list(tibble(lengths = rle(.)$lengths, values = rle(.)$values))) %>%
        gather(key = "player", value = "rle") %>%
        filter(map_lgl(rle, ~!tail(.$values, 1))) %>%
        filter(map_lgl(rle, ~tail(.$lengths, 1) >= 3)) %>%
        mutate(rle = map(rle, ~filter(., lengths >= 3, !values))) %>%
        group_split(player)
    
    if(length(temp) > 0) {
        map_chr(temp, ~paste(.$player, "has lost", .$rle[[1]]$lengths, "times in a row")) %>%
            paste(collapse = "\n")
    } else {
        NA_character_
    }
    
}


ui <- fluidPage(
    theme = shinytheme("readable"),
    useShinyjs(),
    titlePanel("Betting Game Scorer"),
    
    sidebarLayout(
        sidebarPanel(width = 2,
                     h4(uiOutput("round_info")),
                     uiOutput("betting"),
                     hidden(uiOutput("playing"))
        ),
        mainPanel(
            fluidRow(column(10, align = "center",
            tableOutput("play_table")
            ))
        )
    )
    
    
)

server <- function(input, output, session) {
    
    values <- reactiveValues(a = NULL, b = NULL, c = NULL)
    
    showModal(startup_modal())
    
    observeEvent(input$set_up, {
        if(any(map_lgl(1:input$num_players,
                       ~input[[paste0("P", .)]] == ""))) {
            sendSweetAlert(session, type = "error", title = "Error", 
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
        map(2:input$num_players, 
            ~column(6, textInput(paste0("P", .), "Enter Player Name")))
    })
    
    observeEvent(input$set_up, priority = 1, {
        if(any(map_lgl(1:input$num_players,
                       ~input[[paste0("P", .)]] == ""))) {
            sendSweetAlert(session, type = "error", 
                           text = "Names not recorded for all players")
        } else {
        
        removeModal()

        matrix(0, ncol = as.integer(input$num_players), nrow = 3) %>%
            set_colnames(
                map_chr(1:input$num_players, ~input[[paste0("P", .)]])
            ) %>%
            as_tibble() %>%
            bind_cols(tibble(Round = -1, Stage = stages)) %>%
            write_csv("table.csv")
        
        write_csv(tibble(Round = as.integer(0)), "round.csv")
        }
    })
    
    observeEvent(input$reload, {
        removeModal()
    })
    
    num_players <- eventReactive(values$a, {
        if(input$reload == 0) {
            input$num_players
        } else {
            read_csv("table.csv") %>%
                select(-Round, -Stage) %>%
                ncol()
        }
    })
    
    order <- eventReactive(values$a, {
        if(input$reload == 0) {
            map_chr(1:num_players(), ~input[[paste0("P", .)]])
        } else {
            tab <- read_csv("table.csv")
            tab %>% select(-Round, -Stage) %>% colnames() %>%
                shifter(n = tab$Round %>% tail(1) %>% subtract(1))
                
            
        }
    })
    
    shifted_order <- eventReactive(values$b, {
        order() %>% shifter(n = read_csv_q("round.csv")$Round)
    })
    
    observeEvent(values$b, {
        
        output$round_info <- renderUI({
            round <- read_csv_q("round.csv")$Round + 1
            trumps <- rep(c("&spades;", "&hearts;", "&diams;", "&clubs;", ""), 3)[round]
            HTML(paste0("Round ", round, ": ", card_seq(round), " cards. ", 
                        if(trumps == "") "No Trumps" else paste("Trumps are", trumps)))
        })
    })
    
    observeEvent(values$a, {
        
        output$betting <- renderUI({
            tagList(
                map(shifted_order(), 
                    ~pickerInput(paste0(., "BR"), 
                                 paste0(., " bids?"),
                                 choices = c("", 0:card_seq(read_csv_q("round.csv")$Round+1)))),
                actionButton("bet", "Enter Bids")
            )
        })
        
        
        
        
    })
    
    observeEvent(values$a, {
        
        output$playing <- renderUI({
            tagList(
                map(shifted_order(), 
                    ~pickerInput(paste0(., "PR"), 
                                 paste0(., ": how many tricks?"),
                                 choices = c("", 0:card_seq(read_csv_q("round.csv")$Round+1)))),
                actionButton("score", "Enter Results")
            )
        })
    })
    
    observeEvent(input$bet, {
        round <- read_csv_q("round.csv")$Round
        bids <- map(order(), ~as.integer(input[[paste0(., "BR")]])) %>%
            set_names(order())
        if(any(is.na(unlist(bids)))) {
            sendSweetAlert(session, title = "Error", text = "Not all players have bid",
                           type = "error")
        } else if(as.integer(bids) %>% sum(na.rm = T) %>% 
                  equals(card_seq(round+1))) {
            sendSweetAlert(session, title = "Error", text = "You are currently exactly bid",
                           type = "error")
        } else {
            read_csv("table.csv") %>%
                bind_rows(bids %>%
                              list(Round = round, Stage = stages[1]) %>%
                              flatten()) %>%
                write_csv("table.csv")
            
            hideElement("betting")
            showElement("playing")
        }
        
    })
    
    observeEvent(input$score, {
        round <- read_csv_q("round.csv")$Round
        tricks <- map(order(), ~as.integer(input[[paste0(., "PR")]])) %>%
            set_names(order())
        if(any(is.na(unlist(tricks)))) {
            sendSweetAlert(session, title = "Error", 
                           text = "Tricks have not recorded for all players",
                           type = "error")
        } else if(unlist(tricks) %>% sum(na.rm = T) %>% 
                  equals(card_seq(round+1))) {
            prev_scores <- read_csv("table.csv") %>%
                filter(Stage == stages[3]) %>%
                tail(1) %>%
                select(-Round, -Stage)
            round_scores <- map2(map_dbl(order(), ~as.integer(input[[paste0(., "BR")]])),
                                 tricks,
                                 ~calc_points(.x, .y)) %>%
                set_names(order())
            curr_scores <- (if(nrow(prev_scores) > 0) prev_scores + round_scores else round_scores) %>%
                list(Round = read_csv_q("round.csv")$Round, 
                     Stage = stages[3]) %>%
                flatten()
            read_csv("table.csv") %>%
                bind_rows(tricks %>% 
                              list(Round = read_csv_q("round.csv")$Round, 
                                   Stage = stages[2]) %>%
                              flatten()) %>%
                bind_rows(curr_scores) %>%
                write_csv("table.csv")
            
            if(read_csv_q("round.csv")$Round < 12) {
                read_csv_q("round.csv") %>%
                mutate(Round = Round + 1) %>%
                write_csv("round.csv")
                
                showElement("betting")
                hideElement("playing")
                
                tracked_losses <- loss_tracker(read_csv("table.csv"))
                if(!is.na(tracked_losses)) {
                    sendSweetAlert(session, type = "info", title = "", 
                                   text = tracked_losses)
                }
                
            } else {
                hideElement("playing")
                final_scores <- read_csv("table.csv") %>%
                    tail(1) %>% select(-Round, -Stage) %>%
                    t() %>%
                    set_colnames("Final Score") %>%
                    as_tibble(rownames = "Player") %>%
                    arrange(desc(`Final Score`)) %>%
                    mutate(Rank = Rank(`Final Score`))
                    
                output$final_scores <- 
                    renderTable(rownames = TRUE, digits = 0, spacing = "l", bordered = TRUE, {
                    final_scores %>%
                            select(Player, Rank, `Final Score`) %>%
                            column_to_rownames("Player")
                })
                    
                output$end_message <- renderText({
                    paste0("Congratulations ", 
                          final_scores %>% filter(Rank == 1) %>% pull(Player) %>% 
                              paste(collapse = " and "), "!")
                })
                
                showModal(end_modal())
                
            }
            
        } else {
            sendSweetAlert(session, title = "Error", 
              text = "The number of tricks declared doesn't equal the total for this round",
              type = "error")
        }
    })
    
    table_import <- eventReactive(values$c, {
        read_csv("table.csv")
    })
    
    output$play_table <- function() {
        col_order <- c("Round", map(order(), get_col_order) %>% unlist())
        groups <- c(1, rep(3, num_players()), 1, 1) %>%
            set_names(c(" ", order(), " ", " "))
        col_names <- c("Round", rep(stages, num_players()), 
                       "Cards", "Suit")
        out_tab <- table_import() %>%
            gather(key = "Player", value = "temp", -Round, -Stage) %>%
            unite(TEMP, Player, Stage) %>%
            spread(TEMP, temp) %>%
            select(all_of(col_order)) %>%
            mutate(Round = Round + 1) %>%
            slice(-1) %>%
            mutate(Cards = card_seq(Round)) %>%
            mutate(Suit = rep(c("&spades;", "&hearts;", "&diams;", "&clubs;", ""), 3)[Round]) %>%
            kable(format = "html", digits = 0, col.names = col_names, escape=F) %>%
            kable_styling(bootstrap_options = c("bordered", "striped")) %>%
            row_spec(0, font_size = if(num_players() > 6) 9.7 else NULL) %>%
            add_header_above(groups, font_size = if(num_players() > 6) 14 else NULL)
        
        if(table_import() %>% pull(Round) %>% max() == -1) {
            out_tab
        } else {
            out_tab %>%
                column_spec(c(1, which(col_names == "Cards"), which(col_names == "Suit")), 
                              width = "2cm") %>%
                column_spec(seq(3, as.numeric(num_players()) * 3, 3) + 1, bold = T)
        }
    }
    
}

# Run the application 
shinyApp(ui = ui, server = server)
