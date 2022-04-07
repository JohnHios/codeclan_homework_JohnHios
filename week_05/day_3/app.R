#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)


temp_sco <- CodeClanData::temp_df
all_months <- unique(temp_sco$month)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Montly Maximum Temperature Data"),
    
    # theme = shinytheme("sandstone"),
    theme = shinytheme("united"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          selectInput("month_input",
                      "Which Month?",
                      choices = all_months
          ),
          
          HTML(" <br><br><br><br><br><br><br><br>"),
          HTML(" <br><br><br><br><br><br><br><br>"),
            sliderInput("bin_input",
                        "Number of bins:",
                        min = 5,
                        max = 15,
                        value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tsPlot"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$tsPlot <- renderPlot({

      temp_sco %>%
        filter(month == input$month_input) %>%
        ggplot(mapping = aes(x = year, y = max_temp)) +
        geom_line() +
        geom_point() +
        geom_smooth(method = "lm", show.legend = FALSE)+
        labs(x = "year", y = "max. Temperature (deg C)")

    })
    
    
    
    output$distPlot <- renderPlot({
      
      temp_sco %>%
        filter(month == input$month_input) %>%
        ggplot(mapping = aes(x = max_temp)) +
        geom_histogram(bins = input$bin_input, col = "white") +
        labs(x = "max. Temperature (deg C)", y = "Frequency")
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
