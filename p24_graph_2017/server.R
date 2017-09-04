library(shiny)

shinyServer(function(input, output) {

  plotdata <- reactive({
    if(input$category != "All Categories") {
      selected_cat <- input$category
      selected_team <- input$team
      plotdata <- filter(team_data, category== selected_cat) %>% 
        rbind(filter(team_data, team == selected_team))
    } else {
      plotdata <- team_data
    }
  })
    
  library(ggplot2)
  
  plot <- reactive({
    selected_yvar <- input$yvar
    if (selected_yvar=="Points") {
      plot <- ggplot(plotdata, aes(x=hour, y=points, group=team))
    } 
    else if (selected_yvar=="Laps") {
      plot <- ggplot(plotdata, aes(x=hour, y=laps, group=team))
    } 
    else {
      plot <- ggplot(plotdata, aes(x=hour, y=vacay_stops, group=team))
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
    panel.grid.major.x = element_blank()
  ) +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))
  
  xlabels <- c("12 AM", 
               "5 AM",
               "10 AM", 
               "5 PM")
  renderPlot({
    plot +
      geom_line(data = filter(plotdata, team!=selected_team), color="#2AA7AE") +
      geom_line(data = filter(plotdata, team==selected_team), color="#e41a1c", size=1.1) +
      theme_p24 +
      scale_x_continuous(breaks = c(5,10,15,20),
                         labels = xlabels) + 
      ylab(selected_yvar) 
  })
  

})