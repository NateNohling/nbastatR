get_nba_players_ids <- function(active_only = F, resolve_to_fanduel = F) {
  packages <- c("dplyr", "magrittr", "jsonlite", "tidyr", 
                "purrr", "stringr", "lubridate", "tidyr")
  options(warn = -1)
  lapply(packages, library, character.only = T)
  players.url <- "http://stats.nba.com/stats/commonallplayers/?LeagueID=00&Season=2015-16&IsOnlyCurrentSeason=0"
  json_data <- fromJSON(players.url, simplifyDataFrame = T)
  data <- tbl_df(data.frame(json_data$resultSets$rowSet))
  headers <- unlist(json_data$resultSets$headers) %>% 
    str_to_lower()
  headers_df <- get_headers()
  actual_names <- 1:length(headers) %>%
    map(function(x) data_frame(name.actual = headers_df %>% 
                                 mutate(name.nba = name.nba %>% str_to_lower) %>%
                                 filter(name.nba == headers[x]) %>% .$name.actual)) %>% 
    bind_rows()
  names(data) <- actual_names$name.actual
  names_df <- tbl_df(data.frame(str_split_fixed(data$name.last.display, pattern = "\\,", 2)))
  names(names_df) <- c("name.last", "name.first")
  names_df <- mutate(names_df, player = name.first %>% str_trim %>% paste(name.last %>% str_trim)) %>% 
    select(player)
  data$name.player <- names_df$player
  data <- mutate(data, id.player = id.player %>% as.character %>% as.numeric, is.active_player = ifelse(id.team == 0, FALSE, TRUE),
                 id.team = id.team %>% as.numeric) %>% 
    select(-c(status.roster,name.last.display)) %>% 
    mutate_each(funs(extract_numeric),starts_with("year.")) %>% 
    mutate(id.team = ifelse(id.team == 0, NA, id.team), name.player = name.player %>% str_trim,city.team = ifelse(city.team == "", NA, city.team),
           code.team = ifelse(code.team == "", NA, code.team), slug.team = ifelse(slug.team == "", NA, slug.team),
           team = ifelse(city.team %>% is.na, NA, paste(city.team,team)), seasons.played = year.to - year.from, 
           url.player = id.player %>% paste0("http://stats.nba.com/player/#!/",.), 
           image.player = id.player %>% paste0("http://stats.nba.com/media/players/132x132/",., ".png")) %>% 
    select(name.player, id.player,team, id.team, is.active_player, seasons.played,year.from, year.to, everything())
  if (active_only == T) {
    data <- filter(data, is.active_player == T)
  }
  if (resolve_to_fanduel == T) {
    fd_names <- get_fd_name_df()
    data %<>% left_join(fd_names %>% rename(name.player = name.nba))
    data %<>% mutate(is.different_name = ifelse(is.different_name %>% is.na, F, T), 
                     name.player = ifelse(is.different_name == T, name.fanduel, name.player)) %>% 
      select(-c(is.different_name, name.fanduel)) %>% 
      arrange(name.player)
  }
  return(data)
}
