library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rsconnect)

ui <- fluidPage(  

  titlePanel("SDN4"),
      plotlyOutput("plot2", height = 1080, width = 1920))
server <- function(input, output) {
    
  # ggplotly(face_map_plot,tooltip = "text")
  
    p2 <- ggplotly(face_map_plot) %>% layout(height = 1920, width = 960)
    output$plot2 <- renderPlotly({p2})

}

server <- function(input, output) {
  
  output$plot2 <- renderPlotly({
    face_map_plot
  })

}

shinyApp(ui, server)