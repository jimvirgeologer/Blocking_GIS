library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rsconnect)

ui <- fluidPage(  

  titlePanel("MACO MINE"),
      plotlyOutput("plot2", height = 1080, width = 1920))
server <- function(input, output) {
    
  # ggplotly(face_map_plot,tooltip = "text")
  
    p2 <- 
      plot_ly(data = df_points) %>% 
      add_sf(type = "scatter", color = ~cat, colors =col3)%>% 
      layout(title = "AMCI Face Samples",
             plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff') )
    output$plot2 <- renderPlotly({p2})

}



shinyApp(ui, server)