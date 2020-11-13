library(shiny)
library(PPBDS.data)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(shinythemes)
library(gganimate)

opinion_data <- read_rds("data/opinion_data/opinion_data.rds")
opinion_USA <- read_rds("data/opinion_USA/opinion_USA.rds")

ui <- navbarPage(
    "The Reality of Climate Change",
    
# Title of Shiny App

    theme = shinytheme("slate"),

# Background Theme of the Shiny App using the shinythemes package 

    tabPanel("Background Information",
             titlePanel("Global Warming's Greatest Factor: Carbon Emissions"),
             
             p("Human-activity, specifically the emission of carbon dioxide
               and other greenhouse gases, is the primary factor of our 
               changing climate. It is one of the most pressing issues of our
               generation, yet there still exists those who refuse to 
               acknowledge the severity of climate change, and cling to the 
               belief that human activity has not been one of the greatest
               factors contributing to the decline of our planet's health
               and wellbeing."),
             
             br(),
             
             mainPanel(splitLayout(plotOutput("opinion_plot"),
                                   plotOutput("opinion2_plot")))
 
             )
    
    
)

# End of UI Code

server <- function(input, output) {
    
    output$opinion_plot <- renderPlot({
        
        opinion_data %>%
            ggplot(aes(x = Attitudes, 
                       y = Values)) +
            geom_col() +
            coord_flip() +
            facet_wrap(~ Location)
    
})  
    
    output$opinion2_plot <- renderPlot({
        
        opinion_USA %>%
            ggplot(aes(x = Political_Affiliation, 
                       y = Percent_Believe_In_Climate_Change,
                       fill = Political_Affiliation)) +
            geom_col() +
            coord_flip()
        
})
    
}

# End of Server Code

shinyApp(ui = ui, server = server)

