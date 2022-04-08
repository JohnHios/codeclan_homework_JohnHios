library(shiny)
library(tidyverse)
library(CodeClanData)


# Define UI for application that draws the plot
ui <- fluidPage(
  
  fluidRow(
    
    column(12,
           radioButtons('plottype_input',
                        'Plot Type',
                        choices = c("Bar", "Horizontal Bar", "Stacked Bar")
            )
    ),
 
  ),
  
  # Show a plot
  mainPanel(
    plotOutput("the_plot")
  )
  
)

# Define server logic required to draw plot
server <- function(input, output) {
  
    output$the_plot <- renderPlot({
    
    
    if  (input$plottype_input == "Bar"){
      ggplot(students_big) +
        geom_bar(mapping = aes(x = handed, fill = gender), position = "dodge")
    }

    else if  (input$plottype_input == "Horizontal Bar"){
      ggplot(students_big) +
        geom_bar(mapping = aes(x = handed, fill = gender), position = "dodge") +
        coord_flip()
    }
    
    else if (input$plottype_input == "Stacked Bar"){
      ggplot(students_big) +
        geom_bar(mapping = aes(x = handed, fill = gender))
    }

  })

}

# Run the application 
shinyApp(ui = ui, server = server)
