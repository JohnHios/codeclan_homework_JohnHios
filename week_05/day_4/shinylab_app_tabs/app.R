library(shiny)
library(tidyverse)
library(CodeClanData)
library(DT)


# Define UI for application with tabs
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparing Importance of Internet Access vs. Reducing Pollution "),
  
  
  
  fluidRow(
    
            sidebarLayout(
              column(5,
                sidebarPanel(
                  
                  selectInput("gender_input",
                              "Gender",
                              choices = c(Male = "M", Female = "F")
                  ),
                  
                  selectInput("region_input",
                              "Region",
                              choices = unique(students_big$region)
                  ),
                  
                  # Add an action button here
                  actionButton("update", "Generate Plots and Table")
                  
                ),
                
              ),
              
              column(7,
            
   
    
                  tabsetPanel(
                    
                    tabPanel("Plot",
                             
                          
                             mainPanel(
                               fluidRow(
                                 #column(4,
                                        plotOutput("internet_barplot"),
                                # ),
                                 #column(4,
                                        plotOutput("pollution_barplot")
                                # )
                               )
                             )
                            
                    ),
                    
                    tabPanel("Third Tab",
                             "Third Tab Content goes here"
                    )
                  )
                  
              )
          
           )
    
  )
  
 
)


# Define server logic required to draw plots
server <- function(input, output) {
  
  filtered_data <- eventReactive(input$update, {
    students_big %>%
      filter(gender == input$gender_input) %>% 
      filter(region == input$region_input) 
  }) 
  
  
    
  output$internet_barplot <- renderPlot({
    filtered_data() %>% 
      ggplot() +
      geom_histogram(aes(x = importance_internet_access))
  })
  
  output$pollution_barplot <- renderPlot({
    filtered_data() %>%
      ggplot() +
      geom_histogram(aes(x = importance_reducing_pollution))
  })  
    

}

# Run the application 
shinyApp(ui = ui, server = server)