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
  
  df <- df %>% filter(df$Variable == '')
  
  saveRDS(df, 'data/finalizeddata.rds')
} else {
  df <- readRDS('data/finalizeddata.rds')
}





