library(RSQLite)

con <- dbConnect(SQLite(), "games.sqlite")

players_tab <- data.frame(player_id = integer(),
                          first_name = character(),
                          last_name = character())

dbCreateTable(
  conn = con,
  name = "players",
  fields = players_tab
)

gametimes_tab <- data.frame(
  game_id = integer(),
  game_datetime = character()
)

dbCreateTable(
  conn = con,
  name = "gametimes",
  fields = gametimes_tab
)

gameplayers_tab <- data.frame(
  game_id = integer(),
  player_id = character()
)

dbCreateTable(
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

dbCreateTable(
  conn = con,
  name = "rounds",
  fields = rounds_tab
)


