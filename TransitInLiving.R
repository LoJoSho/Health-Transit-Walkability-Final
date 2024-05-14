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

if (!file.exists("data/finalizeddata.rds")) {
  df <- vroom::vroom("data/Citizen_Connect_-_County_data__live_.csv")
  cost_of_living <- vroom::vroom('data/us_cost_of_living.csv')
  
  df <- df %>% filter(df$Year == 2019)
  df_transit_use <- df %>% filter(df$Variable == 'Commute by public transportation')
  
  saveRDS(df, 'data/finalizeddata.rds')
} else {
  df <- readRDS('data/finalizeddata.rds')
}





