# Load libraries
library(shiny)
library(shinythemes)
library(tidyverse)


game_sales <- CodeClanData::game_sales


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("united"),

    # Application title
    titlePanel("Game Sales Data"),
    
    
    # ADD IN A FLUID ROW WITH WIDGETS HERE
    fluidRow(
      
      #Radio Button inputs for game ratings 
      column(4,
             radioButtons('rating_input',
                          'Rating',
                          choices = unique(game_sales$rating),
                          inline = TRUE)
      ), 
      
      #Drop down list inputs for year of release 
      column(4,
             selectInput("year_input", 
                         "Year of Release", 
                         choices = rev(sort(unique(game_sales$year_of_release))))
      ),
      
      #Add an action button
      column(4,
             actionButton("update", "Update dashboard"),
      )
    ),
    
  HTML("<br><br>"),
    # ADD IN A FLUID ROW WITH PLOTS HERE
    # compare the average scores of critics and users for the top 5 publishers.
    # Bar plots are the most appropriate to present results
    fluidRow(
    
    # Show a plot of the top 5 critic score ratings
     column(6,
              plotOutput("critic_rating_barplot")
       ),
    
    # Show a plot of the top 5 user score ratings
     column(6,
              plotOutput("user_rating_barplot")
       )
    ),

  
  HTML("<br><br>"),
    # Show a table of data
    DT::dataTableOutput("table_output")
    
)

# Define server logic required 
server <- function(input, output) {
  
  
  filtered_data <- eventReactive(input$update, {
    game_sales %>%
      filter(rating == input$rating_input) %>%
      filter(year_of_release == input$year_input)
  }) 
  
  
  output$table_output <- DT::renderDataTable({
    filtered_data()
  })
  
  
  output$critic_rating_barplot <- renderPlot({
    filtered_data() %>% 
      group_by(publisher) %>% 
      summarise(avg_crit_rating = mean(critic_score), avg_user_rating = mean(user_score)) %>%
      slice_max(avg_crit_rating, n = 5) %>% 
      ggplot()+
      aes(x = reorder(publisher, avg_crit_rating), y = avg_crit_rating)+
      geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
      ggtitle("Average Critic Score Ratings", subtitle = "top 5 publishers") +
      xlab("Average Critic Score Ratings") +
      ylab("\nPublisher") +
      ylim(0, 100) +
      coord_flip()
  })
  
  
  output$user_rating_barplot <- renderPlot({
    filtered_data() %>% 
      group_by(publisher) %>% 
      summarise(avg_crit_rating = mean(critic_score), avg_user_rating = mean(user_score)) %>%
      slice_max(avg_user_rating, n = 5) %>% 
      ggplot()+
      aes(x = reorder(publisher, avg_user_rating), y = avg_user_rating)+
      geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
      ggtitle("Average User Score Ratings", subtitle = "top 5 publishers") +
      xlab("Average User Score Ratings") +
      ylab("\nPublisher") +
      ylim(0, 10) +
      coord_flip()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
