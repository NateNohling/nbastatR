get_lines <- function(year, type = 'both') {
  packages <- c("dplyr", "XML")
  options(warn = -1)
  lapply(packages, library, character.only = T)
  get_nba_franchise_data <- function (return_franchises = c("all", "active", "current"), 
            return_message = T) {
    team_history_url <- "http://stats.nba.com/stats/franchisehistory?LeagueID=00"
    team_data <- team_history_url %>% fromJSON(simplifyDataFrame = T, 
                                               flatten = )
    names_active <- team_data$resultSets$headers[1] %>% unlist %>% 
      str_to_lower
    names_defunct <- team_data$resultSets$headers[2] %>% unlist %>% 
      str_to_lower
    active_data <- team_data$resultSets$rowSet[1] %>% data.frame %>% 
      tbl_df()
    names(active_data) <- names_active
    active_data %<>% mutate(is.active = T)
    defunct_data <- team_data$resultSets$rowSet[2] %>% data.frame %>% 
      tbl_df()
    names(defunct_data) <- names_defunct
    defunct_data %<>% mutate(is.active = F)
    data <- active_data %>% bind_rows(defunct_data)
    num_cols <- data %>% dplyr::select(-c(contains("team")), 
                                       -is.active) %>% names
    data %<>% mutate_each_(funs(as.numeric), vars = num_cols)
    if (return_franchises == "current") {
      data %<>% mutate(id.row = 1:nrow(.)) %>% group_by(team_id) %>% 
        dplyr::filter(id.row == min(id.row), is.active == 
                        T) %>% dplyr::select(-id.row)
    }
    if (return_franchises == "active") {
      data %<>% dplyr::filter(is.active == T)
    }
    if (return_message == T) {
      "You got NBA franchise data" %>% message
    }
    return(data)
  }
  
  url <- "http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/teams/teams.html"
  html <- readLines(url)
  links <- html[grep('/data/nba/teams/team[0-9]', html)]
  links <- unique(gsub('.*/data/nba/teams/([^\"]*)\"[ ]*>([^<]*).*', '\\1,\\2', links))
  links <- strsplit(links, ',')
  url.base <- paste('http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/teams/pastresults/', year - 1, '-', year, '/', sep = '')
  full.lines <- data.frame()
  
  # Put all team records in one data frame
  for (i in 1:length(links)) {
    
    url <- paste(url.base, links[[i]][1], sep = "")
    tables <- readHTMLTable(url, header = TRUE)
    
    # Get appropriate data: regular season, playoffs, or both
    if (type == 'regular season') {
      
      if (length(tables) == 0) {  # If the team didn't exist at the time, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        temp <- tables[[length(tables)]]    # Regular season is always the last table
      }
      
    } else if (type == 'playoffs') {
      
      if (length(tables) == 1) {  # If the team didn't make the playoffs, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        temp <- tables[[1]]
      }
      
    } else {
      
      if (length(tables) == 0) {  # If the team didn't exist at the time, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else if (length(tables) == 1) {
        temp <- tables[[1]]
      } else {
        temp <- rbind(tables[[1]], tables[[2]])
      }
      
    }
    
    colnames(temp) <- c("date", "away.team", "score", "type", "home.line", "over.under")
    
    if (nrow(temp) > 0) {   # Add home team if the df has any info (we didn't ask for playoffs from a team that didn't make it)
      temp$home.team <- links[[i]][2]
    }
    
    try(full.lines <- rbind(full.lines, temp), silent = FALSE)
  }
  
  teams <- data.frame(team = c("Atlanta", "Boston",  "Brooklyn", "Charlotte", "Chicago", "Cleveland", "Dallas",  "Denver",  "Detroit",
                      "Golden State", "Houston", "Indiana", "L.A. Clippers", "L.A. Lakers", "Memphis", "Miami", "Milwaukee", "Minnesota", 
                      "New Orleans", "New York", "Oklahoma City", "Orlando", "Philadelphia", "Phoenix", "Portland", "Sacramento", "San Antonio", 
                      "Toronto", "Utah","Washington"), slug.team = get_teams_ids()$slug.team)
  
  full.lines <- mutate(full.lines, date = as.Date(date, format = '%m/%d/%y'), team = teams$slug.team[match(home.team, teams$team)], 
                       opp = gsub(pattern = "@ ", x = away.team, replacement = ""), opp = teams$slug.team[match(opp, teams$team)],
                       pts.scored = as.numeric(gsub('[^0-9]*([0-9]+)-([0-9]+)[^0-9]*', '\\1', score)),
                       pts.allowed = as.numeric(gsub('[^0-9]*([0-9]+)-([0-9]+)[^0-9]*', '\\2', score)),
                       line = as.numeric(gsub('[^0-9\\.-]', '', home.line)), 
                       over.under = as.numeric(gsub('[^0-9\\.-]', '', over.under)),
                       teamtotal = ((over.under+line)/2) - line,  opptotal = (over.under+line)/2) %>%
    select(date, team, opp, pts.scored, pts.allowed, teamtotal, opptotal, line, over.under) %>%
    arrange(desc(date))
  
  return(full.lines)
}
