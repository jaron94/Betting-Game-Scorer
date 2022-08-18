create_empty_db <- function() {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), "games.sqlite")
  
  players_tab <- data.frame(player_id = integer(),
                            first_name = character(),
                            last_name = character())
  
  DBI::dbCreateTable(
    conn = con,
    name = "players",
    fields = players_tab
  )
  
  gametimes_tab <- data.frame(
    game_id = integer(),
    game_datetime = character()
  )
  
  DBI::dbCreateTable(
    conn = con,
    name = "gametimes",
    fields = gametimes_tab
  )
  
  gameplayers_tab <- data.frame(
    game_id = integer(),
    player_id = character()
  )
  
  DBI::dbCreateTable(
    conn = con,
    name = "gameplayers",
    fields = gameplayers_tab
  )
  
  rounds_tab <- data.frame(
    round = integer(),
    player_id = integer(),
    bid = integer(),
    tricks = integer(),
    score = integer()
  )
  
  DBI::dbCreateTable(
    conn = con,
    name = "rounds",
    fields = rounds_tab
  )
  
}
