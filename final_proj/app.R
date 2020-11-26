library(shiny)
library(shinythemes)
library(PPBDS.data)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gganimate)

opinion_data <- read_rds("data/opinion_data/opinion_data.rds")
opinion_USA <- read_rds("data/opinion_USA/opinion_USA.rds")

ui <- navbarPage(
    "The Reality of Climate Change",
    
# Title of Shiny App

    theme = shinytheme("lumen"),

# Background Theme of the Shiny App using the shinythemes package 

    tabPanel("Background Information",
             
             fluidPage(
                 titlePanel("Global Warming's Greatest Factor: Carbon Emissions"),
                 
                 br(),
                
                  mainPanel(splitLayout(plotOutput("opinion_plot"),
                                       plotOutput("opinion2_plot")))
 
             ))
    
    
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

