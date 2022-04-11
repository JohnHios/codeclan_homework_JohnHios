library(shiny)
library(CodeClanData)

ui <- fluidPage(
    
    titlePanel("World Wide Game Sales"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput(
                "genre",
                label = "Genre",
                choices = unique(game_sales$genre),
                multiple = TRUE,
                selected = unique(game_sales$genre)
            ),
            
            selectInput(
                "publisher",
                label = "Publisher",
                choices = unique(game_sales$publisher),
                multiple  = TRUE,
                unique(game_sales$publisher)
            ),
            
            sliderInput(
                "min_year",
                label = "Start Year",
                min = min(game_sales$year_of_release),
                max = max(game_sales$year_of_release),
                value = min(game_sales$year_of_release),
                sep = ""
            ),
            
            sliderInput(
                "max_year",
                label = "End Year",
                min = min(game_sales$year_of_release),
                max = max(game_sales$year_of_release),
                value = max(game_sales$year_of_release),
                sep = ""
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Plots",
                         
                    plotOutput("scores_scatter"),
                    
                    plotOutput("sales_histogram"),
                    
                    plotOutput("ratings_bar")
                         
                ),
                tabPanel("Data",
                    
                    DT::dataTableOutput("table")
                    
                )
            )
        )
    )
)