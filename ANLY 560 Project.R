library(shiny)
library(tidyverse)
filtered_bookings <- filter(hotel_bookings2, country %in% c("USA", "GBR", "JPN"))
ui <- fluidPage(
  
  titlePanel(h3('Hotel Bookings', align = "center")),
  
  sidebarPanel(
    
    
    
    selectInput(inputId = "var", label = "1. Select the country", 
                choices = c("All" = "All", "USA" = "USA", "GBR" = "GBR", "JPN" = "JPN"),
                selected = "All"), 
    
    
    
  ),
  
  mainPanel(
   
    plotOutput(outputId = "myplot")
    
  )
)
server = function(input, output) {
  
  output$myplot <- renderPlot({
    if(input$var == "All"){
      p1 <- ggplot(filtered_bookings, aes(country, Date)) + geom_count()
    }else
      if(input$var == "USA"){
        p1 <- ggplot(filtered_bookings, aes(country == "USA", Date)) + geom_count()
      }else
        if(input$var == "GBR"){
          p1 <- ggplot(filtered_bookings, aes(country == "GBR", Date)) + geom_count()
        }else
          if(input$var == "JPN"){
            p1 <- ggplot(filtered_bookings, aes(country == "JPN", Date)) + geom_count()
          }
                  
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x= input$var,y="Date",title="Number of Bookings by Country and Date")
    
    p1
    
  })
}


shinyApp(ui = ui, server = server)
