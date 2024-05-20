library(dplyr)
library(tidyr)
library(shiny)
library(vroom)
library(ggplot2)
library(DT)
library(leaflet)
library(leaflet.extras)
library(randomForest)

# Sets working directory
setwd('C:/Users/stick/Documents/GitHub/Health-Transit-Walkability-Final')
# Clear Variables
rm(list = ls())

# State map to convert initials
state_map <- c(
  "AL" = "Alabama",
  "AK" = "Alaska",
  "AZ" = "Arizona",
  "AR" = "Arkansas",
  "CA" = "California",
  "CO" = "Colorado",
  "CT" = "Connecticut",
  "DE" = "Delaware",
  "FL" = "Florida",
  "GA" = "Georgia",
  "HI" = "Hawaii",
  "ID" = "Idaho",
  "IL" = "Illinois",
  "IN" = "Indiana",
  "IA" = "Iowa",
  "KS" = "Kansas",
  "KY" = "Kentucky",
  "LA" = "Louisiana",
  "ME" = "Maine",
  "MD" = "Maryland",
  "MA" = "Massachusetts",
  "MI" = "Michigan",
  "MN" = "Minnesota",
  "MS" = "Mississippi",
  "MO" = "Missouri",
  "MT" = "Montana",
  "NE" = "Nebraska",
  "NV" = "Nevada",
  "NH" = "New Hampshire",
  "NJ" = "New Jersey",
  "NM" = "New Mexico",
  "NY" = "New York",
  "NC" = "North Carolina",
  "ND" = "North Dakota",
  "OH" = "Ohio",
  "OK" = "Oklahoma",
  "OR" = "Oregon",
  "PA" = "Pennsylvania",
  "RI" = "Rhode Island",
  "SC" = "South Carolina",
  "SD" = "South Dakota",
  "TN" = "Tennessee",
  "TX" = "Texas",
  "UT" = "Utah",
  "VT" = "Vermont",
  "VA" = "Virginia",
  "WA" = "Washington",
  "WV" = "West Virginia",
  "WI" = "Wisconsin",
  "WY" = "Wyoming"
)

# file.remove('data/finalizeddata.rds')

if (!file.exists("data/finalizeddata.rds")) {
  df <- vroom::vroom("data/Citizen_Connect_-_County_data__live_.csv")
  cost_of_living <- vroom::vroom('data/us_cost_of_living.csv')
  
  df <- df %>% filter(df$Year == 2019)
  df <- df %>% filter(df$Variable == 'Commute by public transportation')
  df <- df %>% filter(df$Level == 'County')
  #df <- df %>%
  #  mutate(County = paste(County, State, sep = ", "))
  
  #df$County <- sub(",.*", "", df$County)
  # Remove unneeded columns
  df <- df %>%
    select(-Level, -Category, -Location, -Variable, -Date, -GEOID, 
           -`State FIPS`, -Year, -`Denominator Description`, -`Row-ID`)
  
  df <- df %>% rename_at('Value', ~'Transit_Commuters')
  df <- df %>% rename_at('Denominator', ~'Total_Commuters')
  
  # Handle Cost of Living
  # Resolve an issue with a lack of uppercase in cost of living
  cost_of_living <- cost_of_living %>% rename_at('county', ~'County')
  cost_of_living <- cost_of_living %>%
    mutate(County = ifelse(County == 'Acadia Parish','Acadia County',County)) # So it can merge with Citizen County data
  cost_of_living <- cost_of_living %>%
    mutate(County = paste(County, state_map[state], sep = ", "))
  
  cost_of_living$case_id <- NULL
  cost_of_living$state <- NULL
  cost_of_living$isMetro <- NULL
  
  df <- left_join(df, cost_of_living, by = 'County')
  rm(cost_of_living) # Clear memory
  
  saveRDS(df, 'data/finalizeddata.rds')
} else {
  df <- readRDS('data/finalizeddata.rds')
}

col_names <- colnames(df)

County_Transit_Cost_Summary <- df %>%
  filter(family_member_count == '2p2c') %>%
  group_by(County, State, Transit_Commuters, Total_Commuters, total_cost) %>%
  summarise()
State_Transit_Cost <- df %>%
  filter(family_member_count == '2p2c') %>%
  group_by(State) %>%
  summarise(Avg_Total_Cost = mean(total_cost, na.rm = TRUE),
            Sum_Transit_Commuters = sum(Transit_Commuters, na.rm = TRUE),
            Avg_Transportation_Cost = mean(transportation_cost, na.rm = TRUE))
State_Transit_Cost$Sum_Transit_Commuters <- as.numeric(as.character(State_Transit_Cost$ Sum_Transit_Commuters))
County_Transit_Cost_Summary <- County_Transit_Cost_Summary %>%
  mutate(Transit_Percentage = (Transit_Commuters / Total_Commuters) * 100)
# Used for leaflet
df_aggregated <- df %>%
  group_by(County, State, Longitude, Latitude) %>%
  summarise(
    Transit_Commuters = sum(Transit_Commuters),
    family_member_count = paste(family_member_count, ":", total_cost, collapse = "; ")
  ) %>%
  ungroup()

ui <- fluidPage(
  # Application title
  titlePanel("Transit In Living Data"),
  tabsetPanel(
    # Setup the sandbox
    tabPanel("Sandbox", 
             fluidPage(
               titlePanel("The Data Sandbox"), 
               mainPanel(
                 fluidRow(
                   p("The Sandbox of data"),
                   #column(1, selectInput("sandboxPlotShow", "Show Plot", c("No", "Yes"), "No")),
                   column(2,
                          selectInput("X", "Choose X", col_names, col_names[4]),
                          selectInput("Y", "Choose Y", col_names, col_names[4]),
                          selectInput("Splitby", "Split By", col_names, col_names[2])),
                   column(12, plotOutput("sandboxPlot")),
                 ),
                 fluidRow(
                 column(6, DT::dataTableOutput("table_01", width = "100%")),
               ))
             )
    ),
    tabPanel("Important Findings", 
             fluidPage(
               titlePanel("Important FIndings"),
               mainPanel(
                 fluidRow(
                   p("A collection of important findings"),
                   column(12, plotOutput("plot_02")),
                 ),
                 fluidRow(
                   column(12, plotOutput("plot_03")),
                 ),
                 fluidRow(
                   column(12, plotOutput("plot_04")),
                 ),
               )
             )),
    tabPanel("Leaflet", 
             fluidPage(
               titlePanel("Leaflet from Data"),
               mainPanel(
                 fluidRow(
                   column(12, leaflet::leafletOutput("leaflet"))
                 )
               )
             ))
    
  )
)

server <- function(input, output) {
  output$sandboxPlot <- renderPlot({
    ggplot(df, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) +
      geom_point(na.rm = FALSE) 
      #scale_x_continuous(labels = scales::comma) +
      #scale_y_continuous(labels = scales::comma)
  })
  output$table_01 <- DT::renderDataTable(df[, c(input$X, input$Y, input$Splitby)], 
                                         options = list(pageLength = 25))
  
  output$plot_02 <- renderPlot({
    ggplot(County_Transit_Cost_Summary, aes(x = Transit_Percentage, y = total_cost)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Transit Commuters vs. Total Cost of Living",
           x = "Percentage of Transit Commuters",
           y = "Total Cost of Living") +
      theme_minimal()
  })
  
  output$plot_03 <- renderPlot({
    ggplot(State_Transit_Cost, aes(x = Sum_Transit_Commuters, y = State)) +
      geom_bar(stat = "identity", fill = "#0066CC") +
      labs(
        title = "Number of Transit Commuters by State",
        x = "Total Transit Commuters",
        y = "State"
      ) +
      theme_minimal()+
      scale_x_continuous(labels = scales::number_format(scale = 1e-3, big.mark = ","))
  })
  
  output$leaflet <- renderLeaflet({
      leaflet(df_aggregated) %>%
        addTiles() %>%
        addMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~paste("Number of transit Commuters:", Transit_Commuters, "<br>County: ", County, "<br>State: ", State),
          clusterOptions = markerClusterOptions()
        ) 
  })

}

shinyApp(ui, server)

