library(tmap)
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(viridis)
library(readxl)
library(tidyverse)
library(lubridate)
library(readr)
library(googlesheets4)
library(data.table)
library(shiny)
library(colorspace)
library(dplyr)
library(terra) 
library(RColorBrewer)
library(leaflet)
library(raster)
library(gstat)
library(grid)
library(gridExtra)
library(shinydashboard) 
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders) 
library(waiter) 
 

santa_rita_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/13UBBNXL4JfQbFNUmtU0sdcR-s1p4nayElb7o1Emh6oQ/edit?usp=sharing")

process_santa_rita_data <- function(data) {
  data %>%
    mutate(`Transect Name` = as.factor(`Transect Name`)) %>%
    dplyr::select(-c(`UTM start X`, `UTM start Y`, `UTM end X`, `UTM end Y`)) %>%
    filter(!is.na(`UTM Center X`)) %>%
    mutate(across(starts_with("% Use"), ~ replace_na(., 0)))
}

santa_rita_data <- process_santa_rita_data(santa_rita_data_raw)


santa_rita_1 <- read_sf("shp/SRER_BASEMAP.shp") %>%
  mutate(PastureNam = as.factor(PastureNam)) %>%
  dplyr::select(-c("Shape_Leng", "Shape_Le_1", "Shape_Le_2", "Shape_Le_3", "Area", "Shape_Area", "PastureMai")) %>%
  rename(PastureShape = geometry)

santa_rita_data_plot <- st_as_sf(santa_rita_data, coords = c("UTM Center X", "UTM Center Y"), crs = "+proj=utm +zone=12 +datum=WGS84")

if (!identical(st_crs(santa_rita_1)$input, st_crs(santa_rita_data_plot)$input)) {
  santa_rita_data_plot <- st_transform(santa_rita_data_plot, st_crs(santa_rita_1))
}

SRER_Pastures <- st_join(santa_rita_1, santa_rita_data_plot) %>%
  filter(!is.na(Pasture)) %>%
  group_by(Pasture, PastureShape) %>%
  summarize()


get_transect_names_for_pasture <- function(data, pasture_name) {
  
  if (is.null(data) || nrow(data) == 0) {
    stop("Data is null or empty.")
  }
  
  if (!"Pasture" %in% names(data)) {
    stop("Pasture column does not exist in the data.")
  }
  
  if (is.null(pasture_name) || pasture_name == "") {
    stop("Pasture name is null or empty.")
  }
  
  if (!pasture_name %in% data$Pasture) {
    stop(paste("Pasture name", pasture_name, "does not exist in the data."))
  }
  return(unique(data[data$Pasture == pasture_name, ]$`Transect Name`))
}



get_percent_use_for_year <- function(data, pasture_name, year) {
  transect_names <- get_transect_names_for_pasture(data, pasture_name)
  column_name <- paste0("% Use ", year)
  data[data$`Transect Name` %in% transect_names, ][[column_name]]
}

get_average_use_for_three_years <- function(data, pasture_name, years_to_average) {
  transect_names <- get_transect_names_for_pasture(data, pasture_name)
  data_subset <- data[data$`Transect Name` %in% transect_names, ]
  rowMeans(data_subset[paste0("% Use ", years_to_average)], na.rm = TRUE)
}

#Transform the CRS of Santa Rita data to match Santa Rita 1
santa_rita_data_plot <- st_transform(santa_rita_data_plot, st_crs(santa_rita_1))

# print(head(st_crs(santa_rita_1))$input)==print(head(st_crs(santa_rita_data_plot))$input)
# Join Santa Rita data to Santa Rita 1
result <- st_join(santa_rita_data_plot,santa_rita_1)
result <- result[!is.na(result$Pasture),]
SRER_Pastures <- st_join(santa_rita_1,santa_rita_data_plot)
SRER_Pastures <- SRER_Pastures[!is.na(SRER_Pastures$Pasture),]

SRER_Pastures <- SRER_Pastures %>% group_by(Pasture,PastureShape) %>% summarize()
test_srer <- result  %>%
  pivot_longer(cols = starts_with("% Use"),
               names_to = "Year",
               values_to = "% Use") %>%
  group_by(Pasture, geometry, Year) %>%
  summarize(Average = mean(`% Use`, na.rm = TRUE), .groups="drop")


use_columns <- grep("^% Use", names(santa_rita_data), value = TRUE)
year_choices <- unique(substr(use_columns, 7, 10))



santa_rita_2 = santa_rita_data_raw
# Define the variables of interest from the Santa Rita dataset
santa_rita_GIS_2 <- c("Pasture", "Transect", "Transect Name", 
                      "UTM start X", "UTM start Y", "UTM end X", "UTM end Y")



# Remove any rows with missing data
santa_rita_data_clean_2 <- na.omit(santa_rita_2)

# Convert the cleaned data to a simple features object with UTM coordinates
santa_rita_data_plot_2 <- st_as_sf(santa_rita_data_clean_2, 
                                   coords = c("UTM start X", "UTM start Y"), 
                                   crs = "+proj=utm +zone=12 +datum=WGS84")
santa_rita_3 <- read_sf("shp/SRER_BASEMAP.shp")
# Change Transect_Name to a factor variable
santa_rita_2$"Transect Name" <- as.factor(santa_rita_2$"Transect Name")



# Change character to factor
santa_rita_3$PastureNam <- as.factor(santa_rita_3$PastureNam)

# Check the column names of Santa Rita data
col_names_2 <- names(santa_rita_2)

santa_rita_data_plot_2 <- st_transform(santa_rita_data_plot_2, st_crs(santa_rita_3))

# Join Santa Rita data to Santa Rita 1
result_2 <- st_join(santa_rita_3, santa_rita_data_plot_2)

# Ensure "% Use 2020" column is numeric
santa_rita_2$`% Use 2020` <- as.numeric(santa_rita_2$`% Use 2020`)

# Create a sequence of years
years_2 <- 2010:2023

# Loop through each year to convert the respective columns to numeric
for (year in years_2) {
  column_name <- paste0("% Use ", year)
  santa_rita_2[[column_name]] <- as.numeric(santa_rita_2[[column_name]])
}

# Joining the santa_rita_1 and santa_rita dataframes, reshaping the data, and calculating the average use by year
test_srer_2 <- santa_rita_3 %>%
  left_join(santa_rita_2,by = c("PastureNam"="Pasture")) %>%
  pivot_longer(cols = starts_with("% Use"),
               names_to = "Year",
               values_to = "% Use") %>%
  group_by(PastureNam, geometry, Year) %>%
  summarize(Average = mean(`% Use`, na.rm = TRUE), .groups="drop")

color_labels_2 <- c('0-5', '5-20', '20-40', '40-60', '60-100')

# Define the color palette
color_palette_2 <- c("white", "#7484D2", "#D1D1D1", "#F5CF86","#E60000")


test_srer_2$Year <- as.numeric(gsub("^% Use ", "", test_srer_2$Year))
test_srer_2$Average <- ifelse(is.na(test_srer_2$Average),0,test_srer_2$Average)


# test_srer=test_srer%>%filter(!is.nan(Average))

# Define the color labels
color_labels_3 <- c('0-5', '5-20', '20-40', '40-60', '60-100')

# Define the color palette
color_palette_3 <- c("white", "#7484D2", "#D1D1D1", "#F5CF86","#E60000")

test_srer_2$Transect_Name <- test_srer_2$PastureNam

full_year_data <- santa_rita_3 %>%
  left_join(santa_rita_2,by = c("PastureNam"="Pasture")) %>%
  pivot_longer(cols = starts_with("% Use"),
               names_to = "Year",
               values_to = "% Use") 

test_srer_2$Year <- as.numeric(gsub("^% Use ", "", test_srer_2$Year))









get_transect_names_for_pasture <- function(data, pasture_name) {
  
  df <- unique(data[data$Pasture %in% pasture_name, "Transect Name"][[1]])
  
  return(df)
}



get_percent_use_for_year <- function(data, pasture_name, year) {
  transect_names <- get_transect_names_for_pasture(data, pasture_name)
  column_name <- paste0("% Use ", year)
  values <- data[data$`Transect Name` %in% transect_names, ][[column_name]]
  return(values)
}

