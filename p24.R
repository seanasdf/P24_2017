library(rvest)
library(tidyverse)
library(stringr)
library(lubridate)

# load cached p24 data
if (file.exists("teamdata.rda")) {
  load("teamdata.rda")
} else{
  
  #read in leaderboard
  ldrboard <- read_html("http://leaderboard.powderhorn24.com/")
  
  #get list of team numbers
  teams <- html_nodes(ldrboard, "td:nth-child(6)") %>% 
    as.character() 
  
  #convert list of team numbers to URLS, pull down data
  teampages <- map(teams, str_extract, "\\/[0-9]{1,4}") %>% 
    str_extract("[0-9]{1,4}") %>% 
    paste0("http://leaderboard.powderhorn24.com/teams/", .) %>% 
    map(read_html) 
  
  #get team name from team page
  tm_name <- map(teampages, html_nodes, "h3") %>% 
    map(html_text) 
  
  #get team data from team page
  tm_data <- map(teampages, html_nodes, "#laps-list-table") %>% 
    map(html_table)
  
  #save objects to cache them
  save(tm_name, tm_data, file="teamdata.rda")
}

# cumulative_laps <- function(df) {
#   hours <- mdy_hm("08-18-2017, 19:00") + hours(1)*c(1:23)
#   df <- map(hours, function(x) {
#     mutate(df, ifelse(datetime<x), )
#     })
# }

laps <- map(tm_data, 1) %>% 
  map(mutate, rider_num = str_extract(Rider, "\\([0-9]{1,3}"),
      rider_num = str_extract(rider_num, "[0-9]{1,3}"), 
      rider_name = str_extract(Rider, "^(.*?)\\("),
      rider_name = substr(rider_name, 1, nchar(rider_name)-2),
      time = parse_date_time(Time, "hmsp"),
      day = ifelse(hour(time)>=19 & hour(time) <=23, "Friday", "Saturday"),
      datetime = ifelse(day=="Friday", paste0("8/18/2017, ", Time), paste0("8/19/2017, ", Time)),
      datetime = mdy_hms(datetime)
  ) 

hourbreaks <- mdy_hm("08-18-2017, 19:00") + hours(1)*c(1:23) 
vars <- map_chr(hourbreaks, function(x) paste0("hour", 
                                               ifelse(hour(x)>=20, hour(x)-19, hour(x)+5)))

test <- laps %>% 
  map(function(df) map2_df(.x=vars, 
                           .y=hourbreaks,
                           .f = function(.x,.y) {
                             varname <- quo_name(.x)
                             time <- .y
                             #print(df$datetime < time)
                             mutate(df, !!varname := sum(datetime<time))
                           })
  )

