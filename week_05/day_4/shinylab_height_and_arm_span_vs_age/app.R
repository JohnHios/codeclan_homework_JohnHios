library(shiny)
library(tidyverse)
library(CodeClanData)
library(DT)


# Define UI for application
ui <- fluidPage(
  fluidRow(
    
    column(12,
           radioButtons("ageyears_input",
                        "Age",
                        choices = unique(students_big$ageyears),
                        inline = TRUE
           )
    ),

  ),

  
  
  fluidRow(
    column(6,
           plotOutput("height_dist")
    ),
    column(6,
           plotOutput("armspan_dist")
    )
  )
  
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    students_big %>% 
      filter(ageyears == input$ageyears_input)
  })
  
  
  output$height_dist <- renderPlot({
    
    filtered_data() %>% 
      ggplot() +
      aes(x = height) +
      geom_histogram()
    
  })
  
  output$armspan_dist <- renderPlot({
    
    filtered_data() %>% 
      ggplot() +
      aes(x = arm_span) +
      geom_histogram()
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
