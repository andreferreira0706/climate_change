library(shiny)
library(shinythemes)
library(PPBDS.data)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gganimate)

opinion_data <- read_rds("data/opinion_data/opinion_data.rds")
opinion_USA <- read_rds("data/opinion_USA/opinion_USA.rds")
continent_data <- read_rds("data/continent_data/continent_data.rds")
country_data <- read_rds("data/country_data/country_data.rds")
co2_long_term <- read_rds("data/temp_co2/co2_long_term.rds")
temp_long_term <- read_rds("data/temp_co2/temp_long_term.rds")
correlation <- read_rds("data/temp_co2/correlation.rds")

ui <- navbarPage(
    "The Reality of Climate Change",
    
# Title of Shiny App

    theme = shinytheme("sandstone"),

# Background Theme of the Shiny App using the shinythemes package 

    tabPanel("Background Information",

             fluidPage(
                 titlePanel("Global Warming's Greatest Factor: Carbon Emissions"),
                 
                 includeMarkdown("text/text1.Rmd"),
                 br(),
                 mainPanel(plotOutput("opinion2_plot")),
                 br(),
                 includeMarkdown("text/text2.Rmd"),
                 br(),
                 br(),
                 br(),
                 mainPanel(cellWidth = 400,
                           plotOutput("opinion_plot")),
                 br(),
                 br(),
                 includeMarkdown("text/text3.Rmd")
             )),

    tabPanel("CO2 and Temperature Trends",
             
             fluidPage(
                 titlePanel("The Relationship that Exists Between Carbon Emissions
                            & Temperature Anomalies"),
                 
                 mainPanel(imageOutput("co2concentration_plot"),
                           type = "html",
                           loader = "loader2"),
                 mainPanel(imageOutput("tempanomaly_plot"),
                           type = "html",
                           loader = "loader2"),
                 mainPanel(plotOutput("correlation_plot"))
                 
             ))
    
)

# End of UI Code

server <- function(input, output) {
    
    output$opinion_plot <- renderPlot({
        
        opinion_data %>%
            filter(Location == "United States") %>%
            ggplot(aes(x = Attitudes, 
                       y = Values,
                       fill = Attitudes)) +
            geom_col() +
            coord_flip() +
            scale_x_discrete(labels = c("A little \n serious", 
                                        "Not serious \n at all",
                                        "Somewhat \n serious",
                                        "Very \n serious"),
                             name = "Attitudes") +
            scale_fill_discrete(name = "Attitudes", 
                                type = c("#bdd7e7", 
                                         "#eff3ff", 
                                         "#6baed6", 
                                         "#2171b5")) +
            theme_bw() +
            theme(legend.position = "none") +
            labs(title = "United States Public Opinion",
                 subtitle = "Public Opinion Regarding the Severity of the Climate
                 Crisis",
                 x = "Values",
                 y = "Attitudes",
                 caption = "Source: https://theconversation.com/caribbean-residents
                 -see-climate-change-as-a-severe-threat-but-most-in-us-dont-heres-
                 why-91049")
    
})  
    
    output$opinion2_plot <- renderPlot({
        
        opinion_USA %>%
            ggplot(aes(x = Political_Affiliation, 
                       y = Percent_Believe_In_Climate_Change,
                       fill = Political_Affiliation)) +
            geom_col() +
            scale_fill_discrete(labels = c("Center", "Conservative", "Liberal"),
                                name = "Political Affiliation",
                                type = c("darkgray", "darkred", "darkblue")) +
            theme_classic() +
            labs(title = "US Public Opinion",
                 subtitle = "Public Opinion in the United States According to One's
                 Political Ideologies",
                 x = "Political Affilation",
                 y = "Percent of Individuals Who Believe in Climate Change",
                 caption = "Source: https://theconversation.com/caribbean-residents
                 -see-climate-change-as-a-severe-threat-but-most-in-us-dont-heres-
                 why-91049")
            
})
    
    output$co2concentration_plot <- renderPlot({
        
        list(
            src = 'data/temp_co2/co2_concentration.gif',
            contentType = 'image/gif'
        )
        
})
    
    output$tempanomaly_plot <- renderPlot({
        
        list(
            src = 'data/temp_co2/temp_anomaly.gif',
            contentType = 'image/gif'
        )
        
    })
   
    output$correlation_plot <- renderPlot({
        
        correlation %>%
            ggplot(aes(x = median_temp_anomaly, 
                       y = co2_concentrations,
                       color = median_temp_anomaly)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE, color = "black", linetype = 2) +
            theme_few() +
            labs(title = "Global Median Temperature Anomalies v. Global CO2 Concentrations",
                 subtitle = "Examining the Correlation Between These 2 Variables Over Time",
                 x = "Median Temperature Anomaly",
                 y = "CO2 Concentration (ppm)",
                 caption = "Source: Source: https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions") +
            theme(legend.title = element_blank())
     
    }) 
    
    
}

# End of Server Code

shinyApp(ui = ui, server = server)

