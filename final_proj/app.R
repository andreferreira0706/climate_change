library(shiny)
library(shinythemes)
library(PPBDS.data)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(gganimate)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gtsummary)

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
                 mainPanel(plotOutput("opinion_plot")),
                 br(),
                 br(),
                 includeMarkdown("text/text3.Rmd"),
                 mainPanel(plotlyOutput("worldshare_plot")),
                 br(),
                 includeMarkdown("text/text4.Rmd"),
                 mainPanel(plotOutput("worldshare_map_plot"),
                           width = 700),
                 includeMarkdown("text/text6.Rmd")
             )),

    tabPanel("CO2 and Temperature Trends",
             
             fluidPage(
                 titlePanel("The Relationship that Exists Between Carbon Emissions
                            & Temperature Anomalies"),
                 
                 mainPanel(splitLayout(
                     cellWidths = 400,
                     cellArgs = list(style = "padding: 25px"),
                     imageOutput("co2concentration_plot"),
                           type = "html",
                           loader = "loader2",
                     imageOutput("tempanomaly_plot"),
                           type = "html",
                           loader = "loader2")),
                 mainPanel(plotOutput("correlation_plot"),
                           width = 700),
                 br(),
                 includeMarkdown("text/text5.Rmd")
                 
                 
             )),

    tabPanel("Geographic Data",
         
         fluidPage(
             titlePanel("Continent & Country Data Exploration"),
             
             fluidPage(
                
                 br(),
                 br(),
                 mainPanel(plotOutput("continent_co2_plot"),
                           width = 700),
                 br(),
                 includeMarkdown("text/text7.Rmd"),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("country",
                                     "Country",
                                     country_data$country,
                                     multiple = TRUE, 
                                     selected = "Afghanistan")),
                     mainPanel(plotOutput("country_co2_plot"),
                               plotOutput("country_pop_plot"),
                               plotOutput("country_gdp_plot")))
                 
         ))
    
),

tabPanel("Model", 
         titlePanel("Model"),
         mainPanel(tableOutput("tbl_regression"))),

tabPanel("About", 
         titlePanel("About"),
         h3("Project Background and Motivations"),
         p("Hello, this is where I talk about my project."),
         h3("About Me"),
         p("My name is Andre Ferreira and I am concentrating in Government
             with a secondary in History. 
             You can reach me at andreferreira@college.harvard.edu."))

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
            scale_fill_discrete(name = "Values", 
                                type = c("#bdd7e7", 
                                         "#eff3ff", 
                                         "#6baed6", 
                                         "#2171b5")) +
            theme_bw() +
            theme(legend.position = "none") +
            labs(title = "United States Public Opinion",
                 subtitle = "Public Opinion Regarding the Severity of the Climate
                 Crisis",
                 x = "Attitudes",
                 y = "Values")
        
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
                 y = "Percent of Individuals Who Believe in Climate Change")
            
})
    
    output$worldshare_plot <- renderPlotly({
        
        country_data <- country_data %>%
            select(country, year, share_global_co2, cumulative_co2, iso_code) %>%
            arrange(desc(cumulative_co2)) %>%
            filter(year == 2018,
                   iso_code != "OWID_WRL") %>%
            slice(1:8) %>%
            ggplot(aes(x = share_global_co2, y = cumulative_co2)) +
            geom_text(aes(label = iso_code), size = 3, angle = 45) +
            labs(title = "Global CO2 Emissions (2018 Data)",
                 subtitle = "Measuring National Emissions in 2018",
                 x = "National CO2 Emissions (Share of Global Total)",
                 y = "Cumulative CO2 Emissions (Million Tonnes per Year)") +
            theme_bw()
        
        plotly_build(country_data)
        
    })
    
    output$worldshare_map_plot <- renderPlot({
        
        ggplot(data = world_map) +
            geom_sf(aes(fill = share_global_co2)) +
            labs(title = "World Plot Depicting Global CO2 Share") +
            scale_fill_continuous(name = "Percentage of CO2 Share")
        
    })
    
    output$co2concentration_plot <- renderImage({
        
        list(
            src = 'data/temp_co2/co2_concentration.gif',
            contentType = 'image/gif',
            height = 400
        )
        
}, deleteFile = FALSE)
    
    output$tempanomaly_plot <- renderImage({
        
        list(
            src = 'data/temp_co2/temp_anomaly.gif',
            contentType = 'image/gif',
            height = 400
        )
        
    }, deleteFile = FALSE)
   
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
    
    output$continent_co2_plot <- renderPlot({
        
            continent_data %>%
            select(country, year, co2) %>%
            ggplot(aes(x = year, y = co2, group = country)) +
            facet_wrap(~ country) +
            geom_line() + 
            theme_bw() +
            labs(title = "Continental Carbon Emissions Over Time",
                 x = "Year",
                 y = "CO2 Concentrations (ppm)")
        
    }) 
    
    output$country_co2_plot <- renderPlot({
        
            country_data %>%
            select(country, 
                   year, 
                   co2) %>%
            filter(country %in% input$country) %>%
            ggplot(aes(x = year, 
                       y = co2, 
                       group = country,
                       color = country)) +
            geom_smooth(method = "lm", 
                        se = FALSE, 
                        color = "black",
                        linetype = 2) +
            geom_point() +
            theme_classic() +
            labs(title = "National CO2 Emissions",
                 x = "Year",
                 y = "CO2 Concentrations (ppm)") +
            scale_color_discrete(name = "Country")
        
    }) 
    
    output$country_pop_plot <- renderPlot({
        
        country_data %>%
            select(country, 
                   year, 
                   population) %>%
            filter(country %in% input$country) %>%
            ggplot(aes(x = year, 
                       y = population, 
                       group = country,
                       color = country)) +
            geom_smooth(method = "lm", 
                        se = FALSE, 
                        color = "black",
                        linetype = 2) +
            geom_point() +
            theme_classic() +
            labs(title = "National Population Growth",
                 x = "Year",
                 y = "Population") +
            scale_color_discrete(name = "Country")
        
    }) 
    
    output$country_gdp_plot <- renderPlot({
        
        country_data %>%
            select(country, 
                   year, 
                   gdp) %>%
            filter(country %in% input$country) %>%
            ggplot(aes(x = year, 
                       y = gdp, 
                       group = country,
                       color = country)) +
            geom_smooth(method = "lm", 
                        se = FALSE, 
                        color = "black",
                        linetype = 2) +
            geom_point() +
            theme_classic() +
            labs(title = "National GDP Growth",
                 x = "Year",
                 y = "GDP") +
            scale_color_discrete(name = "Country")
        
    })
    
    output$tbl_regression <- renderTable({
        
        brazil_table %>%
            modify_table_header(column = estimate,
                                label = "**Beta**") %>%
            as_gt() %>%
            tab_header(title = "Regression of Greenhouse Gas Emissions in Brazil")
        
     
    })
    
}

# End of Server Code

shinyApp(ui = ui, server = server)

