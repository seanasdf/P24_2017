

shinyServer(function(input, output) {

  #Make plot responsive to selected race category
  plotdata <- reactive({
    input_cat <- input$category
    input_team1 <- input$team1
    input_team2 <- input$team2
    
    if (input_cat=="All Categories") {
      team_data
    } else {
      plotdata <- team_data %>%
        filter(category == input_cat) %>% 
        rbind(filter(team_data, team==input_team1)) %>% 
        rbind(filter(team_data, team==input_team2))
    }
  })
  
  #Make graph responsive to user-selected Y Variable
  plot <- reactive({
    input_yvar <- input$yvar
    
    if (input_yvar =="Points") {
      ggplot(plotdata(), aes(x=hour, y=points, group=team))
    } else if (input_yvar == "Laps") {
      ggplot(plotdata(), aes(x=hour, y=laps, group=team))
    } else {
      ggplot(plotdata(), aes(x=hour, y=vacay_stops, group=team))
    }
    
  })
  
  library(showtext)
  #add roboto font from google
  font.add.google("Roboto", "roboto")
  showtext.auto()
  
  #create default text
  p24_text <- element_text(family="roboto",
                           size=14,
                           face="plain",
                           color="black",
                           lineheight = 0.4
  )
  
  #create graph theme
  theme_p24 <-  theme(
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=p24_text,
    axis.text.x=p24_text,
    axis.text.y=p24_text,
    legend.text=p24_text,
    plot.caption = p24_text,
    legend.title=element_blank(),
    legend.position="bottom",
    plot.title =p24_text,
    panel.grid.major.x = element_blank(),   
    panel.grid.major.y = element_line(color="#d9d9d9")
  ) +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))
  
  #vector for x axis labels
  xlabels <- c("12 AM",
               "5 AM",
               "10 AM",
               "5 PM")
  
  
  output$p24plot <- renderPlot({
    #capture input y variable and teams
    input_yvar <- input$yvar
    input_team1 <- input$team1
    input_team2 <- input$team2
    
    #grab points totals for each selected team
    team1_laps <- filter(team_data, team==input_team1) %>% 
      pull(points) %>% 
      max()
    
    team2_laps <- filter(team_data, team==input_team2) %>% 
      pull(points) %>% 
      max()
    
    team2_laps <- ifelse(input_team2=="---None---", 0, team2_laps)
    
    
    team1_label <- paste0(input_team1, ": ", team1_laps, " points")
    team2_label <- ifelse(input_team2=="---None---", 
                         "" , 
                         paste0(input_team2, ": ", team2_laps, " points"))
    
    # Plot graph with 1 or 2 lines, depending on user input
    if (input$allteams) {
      plot() + 
        geom_line(data = filter(plotdata(), team!=input_team1), color="#a6cee3") +
        geom_line(data = filter(plotdata(), team==input_team1), color="#e41a1c", size=1.1) +
        geom_line(data = filter(plotdata(), team==input_team2), color="#ff7f00", size=1.1) +  
        theme_p24 +
        scale_x_continuous(breaks = c(5,10,15,20),
                           labels = xlabels) +
        scale_y_continuous(position = "right") +
        ylab(input_yvar) #+
        # annotate("text", 
        #          x = 1, 
        #          y = 50, 
        #          label = ifelse(team1_laps>team2_laps, 
        #                         team1_label, 
        #                         team2_label),
        #          hjust=0,
        #          vjust=0,
        #          color = ifelse(team1_laps>team2_laps, "#e41a1c","#ff7f00")) +
        # annotate("text", 
        #          x = 1, 
        #          y = 40, 
        #          label = ifelse(team1_laps>team2_laps, 
        #                         team2_label, 
        #                         team1_label),
        #          hjust=0,
        #          vjust=0,
        #          color = ifelse(team1_laps>team2_laps, "#ff7f00", "#e41a1c"))
    } else {
      plot() + 
        geom_line(data = filter(plotdata(), team==input_team1), color="#e41a1c", size=1.1) +
        geom_line(data = filter(plotdata(), team==input_team2), color="#ff7f00", size=1.1) +  
        theme_p24 +
        scale_x_continuous(breaks = c(5,10,15,20),
                           labels = xlabels) +
        scale_y_continuous(position = "right") +
        ylab(input_yvar)
    }
  })
})
