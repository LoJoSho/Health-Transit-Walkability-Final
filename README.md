# Health Transit Walkability 

First, these are the libraries we used to start our project

## Starting the project

First, these are the libraries we used to start our project. They help with cleaning, creating predictions, creating maps, creating plots, and creating a shiny app 

 ```r 
 library(dplyr)
library(tidyr)
library(shiny)
library(vroom)
library(ggplot2)
library(DT)
library(leaflet)
library(leaflet.extras)
library(randomForest) 
```
 Next, we converted states into their abbreviations to help track state information. 

 ```r 
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
```


To get our data into a form where we could  get insights we needed to start filtering, renaming, and removing unnecessary columns.

```r
  df <- df %>% filter(df$Year == 2019)
  df <- df %>% filter(df$Variable == 'Commute by public transportation')
  df <- df %>% filter(df$Level == 'County')

 df <- df %>%
    select(-Level, -Category, -Location, -Variable, -Date, -GEOID, 
           -`State FIPS`, -Year, -`Denominator Description`, -`Row-ID`)
  
  df <- df %>% rename_at('Value', ~'Transit_Commuters')
  df <- df %>% rename_at('Denominator', ~'Total_Commuters')



cost_of_living <- cost_of_living %>% rename_at('county', ~'County')
  cost_of_living <- cost_of_living %>%
    mutate(County = ifelse(County == 'Acadia Parish','Acadia County',County))   cost_of_living <- cost_of_living %>%
    mutate(County = paste(County, state_map[state], sep = ", "))
```

Created summary tables to help us summarize our data and make it easier to display our findings.

```r
col_names <- colnames(df)
df <- df %>%
  separate(County, into = c("County"), sep = ",")
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

df_aggregated <- df %>%
  group_by(County, State, Longitude, Latitude) %>%
  summarise(
    Transit_Commuters = sum(Transit_Commuters),
    family_member_count = paste(family_member_count, ":", total_cost, collapse = "; ")
  ) %>%
  ungroup()
```
Plots to show insights from our data, These visualizations are currently in our shiny app.
```r
server <- function(input, output) {
  output$sandboxPlot <- renderPlot({
    ggplot(df, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) +
      geom_point(na.rm = FALSE) 
    #scale_x_continuous(labels = scales::comma) +
    #scale_y_continuous(labels = scales::comma)
  })
  output$table_01 <- DT::renderDataTable(df[, c(input$X, input$Y, input$Splitby)], 
                                         options = list(pageLength = 25))
  #Line chart of Transit Commuters vs. Total Cost of Living  
  output$plot_01 <- renderPlot({
    ggplot(County_Transit_Cost_Summary, aes(x = Transit_Percentage, y = total_cost)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Transit Commuters vs. Total Cost of Living",
           x = "Percentage of Transit Commuters",
           y = "Total Cost of Living") +
      theme_minimal()
  })
  #Bar chart of Average Transportation Cost by State
  output$plot_02 <- renderPlot({
    ggplot(State_Transit_Cost, aes(x = Avg_Transportation_Cost, y = State)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(
        title = "Average Transportation Cost by State",
        x = "State",
        y = "Average Transportation Cost"
      ) +
      theme_minimal() 
  })
  #Bar chart with Number of Transit Commuters by State
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
  #Bar chart of Average Total Cost of Living by State
  output$plot_04 <- renderPlot({
    ggplot(State_Transit_Cost, aes(x = Avg_Total_Cost, y = State)) +
      geom_bar(stat = "identity", fill = "#0066CC") +
      labs(
        title = "Average Total Cost of Living by State",
        x = "Average Total Cost of Living",
        y = "State"
      ) +
      theme_minimal()
  })

```
Created a leaflet to display County, State, Number of transit commuters, and total cost of living

```r
 output$leaflet <- renderLeaflet({
    leaflet(df_aggregated) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste("Number of transit Commuters:", Transit_Commuters, "<br>County: ", County, "<br>State: ", State),
        clusterOptions = markerClusterOptions()






