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

## This is my version of the RDS because I have changed a little bit in cleaning the data
  df <- vroom::vroom("FinalData/Citizen_Connect_-_County_data__live_.csv")
  cost_of_living <- vroom::vroom('FinalData/us_cost_of_living.csv')
  
  df <- df %>% filter(df$Year == 2019)
  df <- df %>% filter(df$Variable == 'Commute by public transportation')
  
  #Remove unneeded columns
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
  
  df <- left_join(df, cost_of_living, by = 'County', 'State')
  #rm(cost_of_living) # Clear memory
  
  saveRDS(df, 'FinalRDS.rds')


##I'm not good at putting these charts into shinyapp, if anyone can do it, i can then do some analysis for it
col_names <- colnames(df)
#Separate County and State
df <- df %>%
  separate(County, into = c("County"), sep = ",")
#Pivot County with State, Transit_Commuters, Total_Commuters, total_cost
County_Transit_Cost_Summary <- df %>%
  filter(family_member_count == '2p2c') %>%
  group_by(County, State, Transit_Commuters, Total_Commuters, total_cost) %>%
  summarise()

#Pivot separate because state is much smaller than county pivot table
State_Transit_Cost <- df %>%
  filter(family_member_count == '2p2c') %>%
  group_by(State) %>%
  summarise(Avg_Total_Cost = mean(total_cost, na.rm = TRUE),
            Sum_Transit_Commuters = sum(Transit_Commuters, na.rm = TRUE),
            Avg_Transportation_Cost = mean(transportation_cost, na.rm = TRUE))
State_Transit_Cost$Sum_Transit_Commuters <- as.numeric(as.character(State_Transit_Cost$ Sum_Transit_Commuters))

County_Transit_Cost_Summary <- County_Transit_Cost_Summary %>%
  mutate(Transit_Percentage = (Transit_Commuters / Total_Commuters) * 100)

#Line chart of Transit Commuters vs. Total Cost of Living
ggplot(County_Transit_Cost_Summary, aes(x = Transit_Percentage, y = total_cost)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Transit Commuters vs. Total Cost of Living",
       x = "Percentage of Transit Commuters",
       y = "Total Cost of Living") +
  theme_minimal()
#Bar chart of Average Transportation Cost by State
ggplot(State_Transit_Cost, aes(x = Avg_Transportation_Cost, y = State)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Average Transportation Cost by State",
    x = "State",
    y = "Average Transportation Cost"
  ) +
  theme_minimal() 


#Bar chart of Average Total Cost of Living by State
ggplot(State_Transit_Cost, aes(x = Avg_Total_Cost, y = State)) +
  geom_bar(stat = "identity", fill = "#0066CC") +
  labs(
    title = "Average Total Cost of Living by State",
    x = "Average Total Cost of Living",
    y = "State"
  ) +
  theme_minimal()

#Bar chart with Number of Transit Commuters by State
ggplot(State_Transit_Cost, aes(x = Sum_Transit_Commuters, y = State)) +
  geom_bar(stat = "identity", fill = "#0066CC") +
  labs(
    title = "Number of Transit Commuters by State",
    x = "Total Transit Commuters",
    y = "State"
  ) +
  theme_minimal()+
  scale_x_continuous(labels = scales::number_format(scale = 1e-3, big.mark = ","))

# Map Leaflet with County, State, Number of transit commuters, and total cost of living
Map_leaflet <- function(County_Transit_Cost_Summary) {
  leaflet(df) %>%
    addTiles() %>%
    addMarkers(
      lng = ~Longitude,
      lat = ~Latitude,
      popup = ~paste("Number of transit Commuters:", Transit_Commuters, "<br>Total Cost of Living:", total_cost, "<br>County: ", County, "<br>State: ", State),
      clusterOptions = markerClusterOptions()
    ) 
}
Map_leaflet()


