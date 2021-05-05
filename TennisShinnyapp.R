library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(ggthemes)

Tennis <- read.csv("KaggleMatches.csv")

ui <- fluidPage(
    titlePanel("Tennis"),
    
    sidebarLayout(mainPanel(plotlyOutput("Tennis"),
                            plotlyOutput("Tennis2")),
        sidebarPanel(
            sliderInput(inputId = "year",
                        label = "Year",
                        min = 1949 ,
                        max = 2021,
                        value = 1957,
                        sep = ""
            ),
            selectInput(inputId = "winner_hand",
                        label = "Winner Hand:",
                        choices = c("L", "R")), 
            selectInput(inputId = "surface",
                        label = "Surface:",
                        choices = c("Grass", "Carpet", "Hard", "Clay")
        )
       
    )
))

server <- function (input, output){
    
    output$Tennis <- renderPlotly({
        plot <- Tennis%>% filter(year == input$year, winner_hand == input$winner_hand,
                                 surface == input$surface) %>% 
            ggplot(mapping = aes(x = tourney_level, fill = factor(best_of))) +
            geom_histogram(stat = "count", position = "dodge") +
            facet_wrap(~year) +
            theme_minimal()+
            labs( title = "Tennis Tournaments Levels",
                  subtitle = "Number of different tennis tournaments levels and best of sets numbers",
                  x = "Tournament Level",
                  fill = "Best Of",
                  caption = "Source: https://www.kaggle.com/taylorbrownlow/atpwta-tennis-data")
        ggplotly(plot)
    })
    
    output$Tennis2 <- renderPlotly({
        plot <- Tennis%>% filter(year == input$year, winner_hand == input$winner_hand) %>% 
            ggplot(mapping = aes(x = winner_ioc, fill = tourney_level)) +
            geom_histogram(stat = "count", position = "dodge") +
            facet_wrap(~year) +
            theme_minimal()+
            coord_flip() +
            labs( title = "Tennis Winners Nationalities",
                  subtitle = "Nationalities of the winner of different tennis tournaments levels",
                  x = "Winner Nationality",
                  fill = "Level",
                  caption = "Source: https://www.kaggle.com/taylorbrownlow/atpwta-tennis-data")
        ggplotly(plot)
    })
    
    
}

shinyApp(ui = ui, server = server)