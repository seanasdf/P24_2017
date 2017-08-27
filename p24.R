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
  
  #get team category from leaderboard
  tm_cat <- map(teampages, html_nodes, "dd:nth-child(2)") %>% 
    map_chr(html_text)
  
  #get team data from team page
  tm_data <- map(teampages, html_nodes, "#laps-list-table") %>% 
    map(html_table)
  
  #get vacation stop times
  vacationstops <- read_html("http://leaderboard.powderhorn24.com/vacation_stops") %>% 
    html_node("#vacation-stops-list-table") %>% 
    html_table()

  #save objects to cache them
  save(ldrboard, tm_cat, tm_name, tm_data, vacationstops, file="teamdata.rda")
}

#unlist and clean up team names
tm_name <- unlist(tm_name) %>% 
  map_chr(trimws, which="both")


#Pull all laps into one big Df, calculate datetime of each lap.
laps <- map(tm_data, 1) %>%
  map2(tm_name,
          ~mutate(.x, team=.y)) %>% 
  map2(tm_cat,
       ~mutate(.x, category=.y)) %>% 
  bind_rows() %>% 
  tbl_df() %>% 
  mutate(rider_num = str_extract(Rider, "\\([0-9]{1,3}"),
      rider_num = str_extract(rider_num, "[0-9]{1,3}"), 
      rider_name = str_extract(Rider, "^(.*?)\\("),
      rider_name = substr(rider_name, 1, nchar(rider_name)-2),
      time = parse_date_time(Time, "hmsp"),
      day = ifelse(hour(time)>=19 & hour(time) <=23, "Friday", "Saturday"),
      datetime = ifelse(day=="Friday", paste0("8/18/2017, ", Time), paste0("8/19/2017, ", Time)),
      datetime = mdy_hms(datetime)
  ) %>% 
  select(-Time, -Rider, -time)


# get breakpoints for hours and variable
hourbreaks <- mdy_hm("08-18-2017, 19:00") + hours(1)*c(1:23) 
vars <- map_chr(hourbreaks, function(x) paste0("hour", 
                                               ifelse(hour(x)>=20, hour(x)-19, hour(x)+5)))

# number of laps at each hour interval, by team
laps <- map2_df(vars, hourbreaks, function(x,y) {
  varname <- quo_name(x)
  laps <- laps %>% 
    group_by(team) %>% 
    mutate(!!varname := sum(datetime<y))
})


#read in vacation stops data
vacay <- map(tm_data, 2)

#replace teams with no vacation stops with blank tibbles
blankdf <- tibble(X1=as.character(NA),
                  X2=as.character(NA),
                  X3=as.character(NA))

vacay <- map_if(vacay, 
       unlist(map(vacay, is.null)),
       function(x) blankdf) %>% 
  map2(tm_name,
       function(x,y) mutate(x,tm_name=y)) %>% 
  map(.x=., function(x) left_join(x, vacationstops, by=c("X1"="Name"))) %>% 
  map(setNames, c("stopname", "recorded", "rider", "tm_name", "location", "starts", "ends"))