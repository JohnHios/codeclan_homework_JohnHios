library(shiny)
library(tidyverse)
library(CodeClanData)




# Define UI for application that creates a scatter plot 
ui <- fluidPage(
  
  # Application title
  titlePanel("Reaction Time vs. Memory Game"),

  
  # Attributes of scatter plot 
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("ageyears_input",
                   "Colour of points",
                   choices = c(Blue = "#3891A6", Yellow = "#FDE74C", Red = "#E3655B")
      ),
      
      sliderInput("alpha_input",
                  "Transparency of points",
                  min = 0,
                  max = 1,
                  value = 0.7
      ),
      
      selectInput("shape_input",
                  "Shape of points",
                  choices = c(Square = 15, Circle = 16, Triangle = 17)
      ),
      
      textInput("text_input", 
                label = "Title of graph", 
                value = "Enter text..."
      ),
     
    ),
    
    # Show a scatter plot
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$scatter_plot <- renderPlot({
    
    students_big %>%
      ggplot(mapping = aes(x = reaction_time, y = score_in_memory_game)) +
      geom_point(color= input$ageyears_input, shape = as.integer(input$shape_input), alpha = input$alpha_input)+
      ggtitle(input$text_input)
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)