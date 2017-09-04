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

###############################################################
############ LAPS LAPS LAPS LAPS LAPS LAPS ####################
###############################################################

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
lapvarnames <- map_chr(hourbreaks, function(x) paste0("laps_hour_", 
                                               ifelse(hour(x)>=20, hour(x)-19, hour(x)+5)))

# number of laps at each hour interval, by team
for (i in 1:length(hourbreaks)) {
  varname <- quo_name(lapvarnames[i])
  time <- hourbreaks[i]
  laps <- laps %>% 
    group_by(team) %>%  
    mutate(!!varname := sum(time>datetime)) 
}

#condense to just list of teams
team_laps <- laps %>% 
  select(team, category, laps_hour_1:laps_hour_23) %>% 
  distinct()

###############################################################
############ VACAY VACAY VACAY VACAY VACAY ####################
###############################################################

#read in vacation stops data
vacay <- map(tm_data, 2)

#replace teams with no vacation stops with blank tibbles
blankdf <- tibble(X1=as.character(NA),
                  X2=as.character(NA),
                  X3=as.character(NA))

vacay <- map_if(vacay, 
       unlist(map(vacay, is.null)),
       function(x) blankdf) %>% 
  #add team name
  map2(tm_name,
       function(x,y) mutate(x,tm_name=y)) %>% 
  #join with data on vacatino stop and start time
  map(.x=., function(x) left_join(x, vacationstops, by=c("X1"="Name"))) %>% 
  map(setNames, c("stopname", "recorded", "rider", "team", "location", "starts", "ends")) %>%
  bind_rows() %>% 
  tbl_df() %>% 
  mutate(time = parse_date_time(ends, "hmsp"),
         day = ifelse(hour(time)>=19 & hour(time) <=23, "Friday", "Saturday"),
         datetime = ifelse(day=="Friday", paste0("8/18/2017, ", ends), paste0("8/19/2017, ", ends)),
         end_datetime = mdy_hms(datetime)
  ) %>% 
  select(-time, -day, -datetime) %>% 
  filter(!is.na(rider)) %>% 
  arrange(team, end_datetime)

#make names for vacation stop
vacayvarnames <- map_chr(hourbreaks, function(x) paste0("vacay_hour_", 
                                                      ifelse(hour(x)>=20, hour(x)-19, hour(x)+5)))

# number of vacation stops  at each hour interval, by team
for (i in 1:length(hourbreaks)) {
  varname <- quo_name(vacayvarnames[i])
  time <- hourbreaks[i]
  vacay <- vacay %>% 
    group_by(team) %>%  
    mutate(!!varname := sum(time>end_datetime)) 
}

team_vacay <- vacay %>%
  select(team, vacay_hour_1:vacay_hour_23) %>% 
  distinct() 

###############################################################
############ VACAY VACAY VACAY VACAY VACAY ####################
###############################################################

#read in vacation stops data
vol <- map(tm_data, 3)


#replace teams with no volunteer shifts with blank tibbles
blankdf <- tibble(X1=as.character(NA),
                  X2=as.character(NA),
                  X3=as.character(NA),
                  X4=as.character(NA))


#add team name, rename vars, combine list of dfs, get full date/time, select only relevants
vol <- vol %>% 
  map_if(unlist(map(vol, is.null)),
         function(x) blankdf) %>%
  map2(tm_name,
     function(x,y) mutate(x,tm_name=y)) %>% 
  map(setNames, c("volshift", "start", "end", "emoji", "team")) %>%
  map(select, team, volshift, end) %>% 
  bind_rows() %>% 
  tbl_df() %>%
  mutate(time = parse_date_time(end, "hmsp"),
         day = ifelse(hour(time)>=19 & hour(time) <=23, "Friday", "Saturday"),
         datetime = ifelse(day=="Friday", paste0("8/18/2017, ", end), paste0("8/19/2017, ", end)),
         end_datetime = mdy_hms(datetime)
  ) %>% 
  select(-time, -day, -datetime, -end) %>% 
  filter(!is.na(volshift))


#make names for vacation stop
volvarnames <- map_chr(hourbreaks, function(x) paste0("vol_hour_", 
                                                        ifelse(hour(x)>=20, hour(x)-19, hour(x)+5)))

# number of vacation stops  at each hour interval, by team
for (i in 1:length(hourbreaks)) {
  varname <- quo_name(volvarnames[i])
  time <- hourbreaks[i]
  vol <- vol %>% 
    group_by(team) %>%  
    mutate(!!varname := sum(time>end_datetime)) 
}

team_vol <- vol %>% 
  select(team, vol_hour_1:vol_hour_23) %>% 
  distinct() 


###############################################################
############ POINTS POINTS POINTS POINTS ######################
###############################################################
team_laps_long <- team_laps %>% 
  gather(laps_hour_1:laps_hour_23, key="hour", value="laps") %>% 
  mutate(hour = as.numeric(str_extract(hour, "\\d+"))) 

team_vacay_long <- team_vacay %>% 
  gather(vacay_hour_1:vacay_hour_23, key="hour", value="vacay_stops") %>% 
  mutate(hour = as.numeric(str_extract(hour, "\\d+"))) 

team_vol_long <- team_vol %>% 
  gather(vol_hour_1:vol_hour_23, key="hour", value="vol_shifts") %>% 
  mutate(hour = as.numeric(str_extract(hour, "\\d+"))) 

team_data <- left_join(team_laps_long, team_vacay_long) %>% 
  left_join(team_vol_long) %>% 
  mutate(vol_shifts = ifelse(is.na(vol_shifts), 0, vol_shifts),
         vacay_stops = ifelse(is.na(vacay_stops), 0, vacay_stops)) %>% 
  mutate(points = laps + vacay_stops * 2 + vol_shifts * 2) %>% 
  arrange(hour, -points, team)

save(team_data, file="./p24_graph_2017/teamdata.rda")


