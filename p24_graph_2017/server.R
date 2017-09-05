

shinyServer(function(input, output) {

  #Make plot responsive to selected race category
  plotdata <- reactive({
    input_cat <- input$category
    input_team <- input$team
    
    if (input_cat=="All Categories") {
      team_data
    } else {
      plotdata <- team_data %>%
        filter(category == input_cat) %>% 
        rbind(filter(team_data, team==input_team))
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
  
  xlabels <- c("12 AM",
               "5 AM",
               "10 AM",
               "5 PM")
  
  
  output$p24plot <- renderPlot({
    input_yvar <- input$yvar
    input_team <- input$team
    

    
    plot() + 
      geom_line(data = filter(plotdata(), team!=input_team), color="#2AA7AE") +
      geom_line(data = filter(plotdata(), team==input_team), color="#e41a1c", size=1.1) +
      theme_p24 +
      scale_x_continuous(breaks = c(5,10,15,20),
                         labels = xlabels) +
      ylab(input_yvar)
  })
})
