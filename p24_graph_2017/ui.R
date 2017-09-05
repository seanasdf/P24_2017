teamnames <- sort(unique(team_data$team))
teamcats <- c("All Categories", sort(unique(team_data$category)))

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  theme="style.css",
  
  # Application title
  fluidRow(
    column(12,
      headerPanel("Powderhorn 24 Race Results")
    )
  ),
  

  
  fluidRow(
    column(12,
           plotOutput("p24plot")
    )
  ),
  
  fluidRow(
    column(3,
           
      wellPanel(
           selectInput("team1",
                       "Select a Team", 
                       choices = teamnames,
                       selected = sample(teamnames, 1))
      )
    ),
    column(3,
           
           wellPanel(
             selectInput("team2",
                         "Comparison Team", 
                         choices = c("---None---",teamnames),
                         selected = "---None---")
           )
    ),
    
    column(3,
      wellPanel(
         selectInput("category",
                     "Select Team Category",
                     choices = teamcats)
      )
    ),
    column(3,
           wellPanel(
             selectInput("yvar",
                         "Outcome to Plot",
                         choices = c("Points", "Laps", "Vacation Stops"))
           )
    )
  ),
  fluidRow(
    column(12,
          checkboxInput("allteams", 
                        label = "Compare with Other Teams in Category", 
                        value = TRUE)
    )
  )
))
