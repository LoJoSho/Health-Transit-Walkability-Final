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
           -`State FIPS`, -Year, -State, -`Denominator Description`, -`Row-ID`)
  
  df <- df %>% rename_at('Value', ~'Transit_Commuters')
  df <- df %>% rename_at('Denominator', ~'Total_Commuters')
  
  # Handle Cost of Living
  # Resolve an issue with a lack of uppercase in cost of living
  cost_of_living <- cost_of_living %>% rename_at('county', ~'County')
  #cost_of_living <- cost_of_living %>% filter(cost_of_living$family_member_count == '2p2c')
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
                          selectInput("X", "Choose X", col_names, col_names[3]),
                          selectInput("Y", "Choose Y", col_names, col_names[2]),
                          selectInput("Splitby", "Split By", col_names, col_names[7])),
                   column(12, plotOutput("plot_01")),
                 ),
                 fluidRow(
                 column(6, DT::dataTableOutput("table_01", width = "100%")),
               ))
             )
    ),
    
  )
)

server <- function(input, output) {
  output$plot_01 <- renderPlot({
    ggplot(df, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) +
      geom_point(na.rm = FALSE) + 
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$table_01 <- DT::renderDataTable(df[, c(input$X, input$Y, input$Splitby)], 
                                         options = list(pageLength = 25))
}


shinyApp(ui, server)





