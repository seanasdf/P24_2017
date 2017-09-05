teamnames <- sort(unique(team_data$team))
teamcats <- c("All Categories", sort(unique(team_data$category)))

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  includeCSS("style.css"),
  
  # Application title
  fluidRow(
    column(12,
      headerPanel("Powderhorn 24 Race Results")
    )
  ),
  
  fluidRow(
    column(12,
           plotOutput("p24plot",
                      width="800px",
                      height="400px")
    )
  ),
  
  fluidRow(
    column(4,
           
      wellPanel(
           selectInput("team",
                       "Select a Team", 
                       choices = teamnames)
      )
    ),
    
    column(4,
      wellPanel(
         selectInput("category",
                     "Select Team Category",
                     choices = teamcats)
      )
    ),
    column(4,
           wellPanel(
             selectInput("yvar",
                         "Outcome to Plot",
                         choices = c("Points", "Laps", "Vacation Stops"))
           )
    )
  )
))
