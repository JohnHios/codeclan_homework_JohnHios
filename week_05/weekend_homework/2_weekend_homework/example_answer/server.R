library(shiny)

server <- function(input, output) {
    
    filter_data <- reactive({
        game_sales %>%
            filter(genre %in% input$genre) %>%
            filter(publisher %in% input$publisher) %>%
            filter(year_of_release >= input$min_year) %>%
            filter(year_of_release <= input$max_year)
    })
    
    output$scores_scatter <- renderPlot({
        
        make_scores_scatter_plot(filter_data())
        
    })
    
    output$sales_histogram <- renderPlot({
        
        make_sales_histogram(filter_data())
        
    })
    
    output$ratings_bar <- renderPlot({
        
        make_rating_bar_plot(filter_data())
        
    })
    
    output$table <- DT::renderDataTable({
        
        filter_data() %>%
            arrange(desc(sales))
        
    })
}

