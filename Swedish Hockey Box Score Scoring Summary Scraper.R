library(tidyverse)
library(rvest)
library(progress)

## This function gets URLs for every game for the league & season supplied
get_schedule <- function(league, season, ..., progress = TRUE) {
  
  mydata <- tidyr::crossing(league, season)
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_schedule() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    pb$tick(0)
    
  }
  
  .get_schedule <- function(league, season, ...) {
    
    if (league == "SHL") {
      
      if (season == "2018-19") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9171"}
      
      else if (season == "2017-18") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/8121"}
      else if (season == "2016-17") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/7132"}
      else if (season == "2015-16") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/6052"}
      else if (season == "2014-15") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/5056"}
      else if (season == "2013-14") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/3905"}
      else if (season == "2012-13") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/2892"}
      else if (season == "2011-12") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/965"}
      else if (season == "2010-11") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/258"}
      else if (season == "2009-10") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9024"}
      else if (season == "2008-09") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9052"}
      else if (season == "2007-08") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9077"}
      else if (season == "2006-07") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9143"}
      else if (season == "2005-06") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9146"}
      else if (season == "2004-05") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9149"}
      else if (season == "2003-04") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9152"}
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else if (league == "Allsvenskan") {
      
      if (season == "2018-19") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9168"}
      
      else if (season == "2017-18") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/8122"}
      else if (season == "2016-17") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/7157"}
      else if (season == "2015-16") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/6053"}
      else if (season == "2014-15") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/5057"}
      else if (season == "2013-14") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/3906"}
      else if (season == "2012-13") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/3005"}
      else if (season == "2011-12") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/966"}
      else if (season == "2010-11") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/259"}
      else if (season == "2009-10") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9301"}
      else if (season == "2008-09") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9305"}
      
      else {stop("Season not available. Sorry!")}
      
    }  
    
    else if (league == "SuperElit") {
      
      if (season == "2018-19") {
        
        url <- c(
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/9169",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/9170")
        
      }
      
      else if (season == "2017-18") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8702",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8701",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8124",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/8125")
        
      }
      
      else if (season == "2016-17") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7155",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7154",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7152",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/7153")
        
      }
      
      else if (season == "2015-16") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6695",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6694",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6054",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/6055")
        
      }
      
      else if (season == "2014-15") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5784",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5783",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5024",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/5025")
        
      }
      
      else if (season == "2013-14") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/4656",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/4655",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3940",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3941")
        
      }
      
      else if (season == "2012-13") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3630",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3629",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3002",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/3003")
        
      }
      
      else if (season == "2011-12") {
        
        url <- c(
          url_forts = "http://stats.swehockey.se/ScheduleAndResults/Schedule/1651",
          url_top_10 = "http://stats.swehockey.se/ScheduleAndResults/Schedule/1650",
          url_norra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/960",
          url_sodra = "http://stats.swehockey.se/ScheduleAndResults/Schedule/961")
        
      }
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else {stop("League not available. Sorry!")}
    
    schedule_clean <- function(url, ...) {
      
      clean_schedule <- url %>%
        read_html() %>%
        html_nodes("#groupStandingResultContent a") %>%
        html_attr("href") %>%
        str_replace_all("[\r\n]", "") %>%
        str_squish() %>%
        str_extract("/Game/Events/[[:digit:]]{2,}") %>%
        str_c("http://stats.swehockey.se", .) %>%
        as_tibble() %>%
        set_names("url") %>%
        mutate(league = league) %>%
        mutate(season = season) %>%
        drop_na()
      
      return(clean_schedule)
      
    }
    
    schedule_data <- map_dfr(url, schedule_clean)
    
    if (progress) {pb$tick()}
    
    return(schedule_data)
    
  }
  
  all_schedule_data <- map2_dfr(mydata[["league"]], mydata[["season"]], .get_schedule)
  
  return(all_schedule_data)
  
}

## Get URLs for SHL, Allsvenskan, and SuperElit for the past 2 years
## Pick any season/league combos you want
## Leagues must be spelled exactly: SHL or Allsvenskan or SuperElit
games <- get_schedule(c("SHL", "Allsvenskan", "SuperElit"), c("2018-19", "2017-18"))

## This function gets the box score data using the URLs acquired from get_schedule() 
get_box_score <- function(..., progress = TRUE) {
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_box_score() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    
    pb$tick(0)}
  
  .get_box_score <- function(url, league, season, ...) {
    
    seq(0, 0.5, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    messy_data <- url %>%
      read_html() %>%
      html_nodes("tr+ tr .tblContent .tdOdd") %>%
      html_text() %>%
      as_tibble() %>%
      filter(lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 0) | 
               lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 1) |
               lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 2))
    
    team <- messy_data %>%
      filter(row_number() %% 3 == 2) %>%
      set_names("team")
    
    game_strength <- messy_data %>%
      filter(row_number() %% 3 == 1) %>%
      set_names("game_strength")
    
    players <- messy_data %>%
      filter(row_number() %% 3 == 0) %>%
      set_names("players")
    
    if (nrow(players) != 0) {
      
      box_score_data <- bind_cols(team, game_strength, players) %>%
        mutate(goal_state = str_split(game_strength, "\\(", simplify = TRUE, n = 2)[,1]) %>%
        mutate(game_strength = str_split(game_strength, "\\(", simplify = TRUE, n = 2)[,2]) %>%
        mutate(game_strength = str_replace_all(game_strength, "\\)", "")) %>%
        mutate(players = str_replace_all(players, "[\r\n]", "")) %>%
        mutate(goal = str_split(players, "[0-9]+\\.", simplify = TRUE, n = 4)[,2]) %>%
        mutate(goal = str_split(goal, "\\(", simplify = TRUE)[,1]) %>%
        mutate(goal = str_c(str_squish(str_split(goal, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(goal, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
        mutate(goal = str_replace_all(goal, "[0-9]", "")) %>%
        mutate(primary_assist = str_split(players, "[0-9]+\\.", simplify = TRUE, n = 4)[,3]) %>%
        mutate(primary_assist = str_c(str_squish(str_split(primary_assist, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(primary_assist, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
        mutate(secondary_assist = str_split(players, "[0-9]+\\.", simplify = TRUE, n = 4)[,4]) %>%
        mutate(secondary_assist = str_c(str_squish(str_split(secondary_assist, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(secondary_assist, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
        mutate(league = league) %>%
        mutate(season = season) %>%
        mutate(game_url = url) %>%
        mutate_all(str_squish) %>%
        select(-players)
      
    }
    
    if (progress) {pb$tick()}
    
    return(box_score_data)
    
  }
  
  all_box_score_data <- pmap_dfr(..., .get_box_score)
  
  return(all_box_score_data)
  
}

## Gets the scoring summary data from the games/URLs selected
scoring_data <- games %>% get_box_score()
