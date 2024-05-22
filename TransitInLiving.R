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
#setwd('C:/Users/stick/OneDrive/Documents/GitHub/Health-Transit-Walkability-Final')
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
  
  df$PercentageTransit <- (df$Transit_Commuters / df$Total_Commuters)
  
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

# Prediction Engine:
training_data <- df %>% 
  filter(family_member_count == '2p2c') %>%
  sample_n(1000, replace = FALSE)

model_lat <- randomForest(Latitude ~ median_family_income, data = training_data)
model_lon <- randomForest(Longitude ~ median_family_income, data = training_data)


ui <- fluidPage(
  # Application title
  titlePanel("Transit In Living Data"),
  tabsetPanel(
    tabPanel("Documentation",
             fluidPage(
               titlePanel("Documentation"),
               mainPanel(
                 fluidRow(
                   column(12,
                          h2("Scope of Project")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h3("Group Members"),
                          p("Logan Schulz"),
                          p("Quang Nguyen"),
                          p("Kevin Akins")
                          
                   )
                 ),
                 fluidRow(
                   column(12,
                          h3("Project Title"),
                          h4("Analyzing the Relationship Between Transit Commuters and Cost of Living"))
                 ),
                 fluidRow(
                   column(12,
                          h3("Project Objectives"),
                          p("1. To analyze the correlation between the number of transit commuters and the cost of living in various counties and states."),
                          p("2. To identify patterns and trends in transportation costs and cost of living across different regions."),
                          p("3. To visualize the data to facilitate understanding and decision-making.")
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h3("Deliverables"),
                          p("1. Data Preparation:"),
                          p("- Cleaned and formatted dataset including county, state, and various cost metrics."),
                          p("- Separation of county and state into distinct columns."),
                          p("- Aggregated data to summarize average costs by state and county."),
                          p("2. Analysis and Visualizations:"),
                          p("- Bar charts showing the number of transit commuters, average total cost of living, and transportation costs by state."),
                          p("- Line charts or alternative visualizations if bar charts are not suitable due to data volume."),
                          p("- Summary statistics and insights derived from the data analysis."),
                          p("3. Documentation:"),
                          p("- Detailed report on the methodology used for data cleaning, aggregation, and analysis."),
                          p("- Interpretation of the visualizations and statistical results."),
                          p("- Recommendations based on the findings.")
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h3("Tasks and Activities"),
                          p("1. Data Collection and Cleaning:"),
                          p("- Gather data and filter on transit commuters, cost of living components (transportation), and demographic information."),
                          p("- Clean the data to remove inconsistencies and errors."),
                          p("- Split combined county and state fields into separate columns."),
                          p("2. Data Aggregation:"),
                          p("- Calculate average costs and other relevant metrics by state."),
                          p("- Summarize the total number of transit commuters by state."),
                          p("3. Data Visualization:"),
                          p("- Create visualizations such as bar charts and line charts to represent the data."),
                          p("- Use geospatial tools to visualize data on maps, showing the distribution of transit commuters and cost of living by region."),
                          p("- Ensure visualizations are clear, informative, and properly labeled."),
                          p("4. Analysis and Interpretation:"),
                          p("- Perform statistical analysis to identify correlations and patterns."),
                          p("- Interpret the results to draw meaningful conclusions."),
                          p("5. Reporting:"),
                          p("- Compile findings into a comprehensive report."),
                          p("- Include visualizations, statistical analysis, and recommendations.")
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h3("Exclusions"),
                          p("- Analysis of non-transit commuter data."),
                          p("- Costs not directly related to living expenses (housing, food, healthcare, etc.).")
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h3("Constraints"),
                          p("- Timeframe for project completion: 2 weeks.")
                          
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h3("Assumptions"),
                          p("- All required data will be available and accessible."),
                          p("- Data will be accurate and up-to-date.")
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h3("Risks"),
                          p("- Potential data quality issues."),
                          p("- Misinterpretation of complex data patterns.")
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          h3("Timeline"),
                          p("1. Week 1: Data Collection and Cleaning, Data Visualization and Analysis, and Research"),
                          p("2. Week 2: Reporting and Documentation")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h3("Backlog of ideas"),
                          p("- Filter Data to only include transit use"),
                          p("- Join data together based on counties & state"),
                          p("- Chart with number of transit commuters with states and total cost of living"),
                          p("- Chart with transportation cost with each state"),
                          p("- Leaflet map with state and number of transit commuters"),
                          p("- Add interpretations of what we found in the Shinny App"),
                          p("- Prediction Model"),
                          p("- Find Research Model - Write Summary - Connect it with our data")
                 ))
               ))
             ),
    tabPanel("Research",
             fluidPage(
               titlePanel("Transportation Essay"),
               mainPanel(
                 h3("Our research compared to others"),
                 p("For our project we looked at two different examples of research. Both research papers tried to link the need of public transportation to income and cost of living. These studies took information from various major cities around America. They looked at the cost of living and income at the rich and poor sides of each city. Our first paper Transportation needs of low income population: a policy analysis for the Washington D.C. metropolitan region. In this paper they examined D.C.’s transportation. They mainly focused on families. This relates to our project. We not only had a focus on family, but also the size of the family as well. The findings in this paper not only revealed public transportation use, but also various forms of transportation. This includes walking, biking, and car ride passengers. Something that was not really present in our data was the car ownership amount. In D.C. their research found that average family in low income neighborhoods did not own a car. On the other hand, families in high income areas owned on average almost two cars. This causes many lower income families to use public transportation because they cannot afford their own vehicle. In the second paper, they examined similar topics like us. One of the many points we look into was cost of transportation. In Why do the poor live in cities? The role of public transportation the researchers sought out to find wheter or not cost of transportation  as well as cost of other things played a role in the amount of people who took public transportation. This paper researched many major cities over the United States. Some of these cities are Chicago, Philadelphia, and New York. Their findings showed that cost of transportation did not play a role in whether or not people used public transportation more or less. This finding does not agree with the observations we made in our data. We concluded that lower cost of transportation likely influcneced the decision to take public transportation. I believe that our findings seem to be different because of the location of the studies, as well as the many variables such as distance from jobs they included in their study." ),
                 h4("Citations"),
                 p("Glaeser, Edward L., et al. “Why do the poor live in cities? the role of public transportation.” Journal of Urban Economics, vol. 63, no. 1, Jan. 2008, pp. 1–24, https://doi.org/10.1016/j.jue.2006.12.004.
                    Serulle, Nayel Urena, and Cinzia Cirillo. “Transportation needs of low income population: A policy analysis for the Washington D.C. Metropolitan Region.” Public Transport, vol. 8, no. 1, 3 Feb. 2016, pp. 103–123, https://doi.org/10.1007/s12469-015-0119-2")
               )
             )
    ),
    tabPanel("Why and requirements",
             fluidPage(
               titlePanel("Paragraph on why and requirements"),
               mainPanel(
                 h3("Why followed by requirements "),
                 p("As college students the cost of things is important to us. Many college students do not have cars on campus. Unfortunately, like many places in America, the Quad Cities are not walkable. Therefore, lots of students depend on public transportation to travel throughout the Quad Cities.  This made us think of how many people also depend on transportation in the United States. We know that the cost of living impacts how much money a family can spend. Ideally, a family could afford a vehicle to commute if the cost of living is cheap in a city. We decided to do a deep dive into whether the cost of living and public transportation users have any evidence of correlation. Our project looks into trends in public transportation. Furthermore, we infer as to why certain decisions are being made. Optimistically, our project can help decision-makers make better choices about transportation. "),
                 p("The biggest and most important requirement was to collect and transform data from all 50 states. That entails renaming columns, separating each state, and making new data frames. This includes data from counties and minor cities. Without all states, our data would be incomplete, and concluding using that data would be irresponsible. Another major requirement is cross-referencing the cost of living with the location of the city. A lot of this data overlapped in many ways. Multiple cities belonged to the same county and state all with similar names. Making sure the data overlapped in ways they were supposed to, and stopping them from overlapping in areas they should not was vital to success. Calculation proved to be important as well. Taking the cost of living from different categories added an extra layer of depth to our work. The different sizes of families impacted the cost of living as well.  This required us to take into account the different sizes of families. To take into the true impact of the cost of living. We looked into the exact location of places as well. This entails including the longitude as well as the latitude for each town. Lastly, we need to look at trends in our data. Looking at the results helped us with the next part of our project. Making an accurate prediction model includes recognizing patterns in our data. Without pattern recognition building a model would be inaccurate and useless as a result.")
                 
               )
               
             )
             
    ),
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
                          selectInput("Y", "Choose Y", col_names, col_names[18]),
                          selectInput("Splitby", "Split By", col_names, col_names[2])),
                   column(12, plotOutput("sandboxPlot")),
                 ),
                 fluidRow(
                   column(6, DT::dataTableOutput("table_01", width = "100%")),
                 ))
             )
    ),
    tabPanel("Important Findings 1", 
             fluidPage(
               titlePanel("Important Findings"),
               mainPanel(
                 fluidRow(
                   p("Transit Commuters vs. Total Cost of Living"),
                   column(12, plotOutput("plot_01")),
                   p("- The percentage of transit commuters is quite low, mostly below 5% across the US."),
                   p("- People with a cost of living below $100,000 were more likely to take transit transportation.")
                 ),
                 fluidRow(
                   p("Average Transportation Cost by State"),
                   column(12, plotOutput("plot_02")),
                   p("- California, Utah, Colorado, and Nevada are states with the highest average transportation cost."),
                   p("- Louisiana, Mississippi, and New York had low average transportation cost.")
                 ),
                 
               )
             )),
    tabPanel("Important findings 2",
             fluidPage(
               titlePanel("Important Findings 1"),
               mainPanel(
                 fluidRow(
                   p("Number of Transit Commuters by State"),
                   column(12, plotOutput("plot_03")),
                   p("- New York has the highest number of transit commuters with over 2 million transit commuters. Moreover, California and Illinois also take second and third place for most transit commuters since they all have biggest cities in the US."),
                   p("- Louisiana and Alaska are the two states with the lowest transit commuters."),
                   p("- According to the previous graph, some states, such as New York or New Jersey, prefer public transit since they have low average transportation costs and a high number of transit commuters."),
                   p("- From the above statement, we can consider increasing the number of transit transportation in states that have high transportation costs and few transit commuters. However, we still need to consider the cost of living in those states.")
                 ),
                 fluidRow(
                   p("Average Total Cost of Living by State"),
                   column(12, plotOutput("plot_04")),
                   p("- The states with highest total cost of living are Massachusetts and Hawaii."),
                   p("- The states with the lowest total cost of living are Mississippi, Arkansas, South Carolina."),
                   p("- Some states such as New York, Illinois, and California have maximized the use of transit transportation with low transportation costs, a low total cost of livings and high transit commuters."),
                   p("- From these data, Arkansas, Wyoming or Vermont state should develop transit transport to lower transportation costs and total cost of living.")
                 )
               )
             )),
    tabPanel("Data Leaflet", 
             fluidPage(
               titlePanel("Leaflet from Data"),
               mainPanel(
                 fluidRow(
                   column(12, leaflet::leafletOutput("leaflet"))
                 )
               )
             )),
    # Preidction Engine
    tabPanel("Prediction Engine", 
             fluidPage(
               titlePanel("Prediction"), 
               p("Predict, if you have a certain income, where you should live"),
               sidebarLayout(
                 sidebarPanel(
                   # Input panel for time
                   numericInput("Income", "Enter your income:", value = 14000, min = 0),
                 ),
                 mainPanel(
                   textOutput("result_lat"),
                   textOutput("result_lon"),
                   leafletOutput("predict_leaflet"),
                 )
               )
             )
    )
  )
)

server <- function(input, output) {
  calculate_position <- reactive({
    # Predict latitude and longitude
    lat <- predict(model_lat, newdata = data.frame(median_family_income = input$Income))
    lon <- predict(model_lon, newdata = data.frame(median_family_income = input$Income))
    
    # Return position and address
    return(list(lat = lat, lon = lon))
  })
  
  # Prediction Engine Stuff
  output$result_lat <- renderText({
    pos <- calculate_position()
    result_text <- paste("Latitude: ", pos$lat)
  })
  output$result_lon <- renderText({
    pos <- calculate_position()
    result_text <- paste("Longitude: ", pos$lon)
  })
  
  output$predict_leaflet <- renderLeaflet({
    # Predict latitude and longitude
    position <- calculate_position()
    
    # Create leaflet map
    leaflet(position) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~paste("Lat:", lat, "<br>Lon:", lon),
        clusterOptions = markerClusterOptions
      )
  })
  
  
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
  #Map Leaflet with County, State, Number of transit commuters, and total cost of living
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
