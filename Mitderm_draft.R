##Set Up

library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(kableExtra)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots
library(mapview)
library(tigris)
library(leaflet)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

##Housing Data

Boulder_boundary <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/City_of_Boulder_City_Limits.kml") %>%
  st_transform('EPSG:26913')%>%
  select(-Name, -Description)
Boulder_boundary <- st_union(Boulder_boundary)

housing <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/studentData.geojson", crs = 'ESRI:102254')%>%
  st_transform('EPSG:26913')

housing <- housing %>%
  select(-Stories, -UnitCount)

housing <- housing [!is.na(housing$price),]
housing <- housing [!is.na(housing$geometry),]

##Crime Data
  
crime <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Boulder_Police_Department_(BPD)_Offenses.geojson") %>%
  st_transform('EPSG:26913')

year <- c("2019","2020","2021")

crime <- filter(crime, Report_Year %in% year)

crime <- crime%>%
  filter(IBRType != "All Other Offenses")%>%
  select(-Special_District)%>%
  na.omit() %>% 
  distinct()

##Crime Buffer

housing$crimes.Buffer =
  st_buffer(housing, 200) %>% 
  aggregate(mutate(crime, counter = 1),., sum) %>% 
  pull(counter)

##Crime Nearest Neighbor Feature

st_c <- st_coordinates

housing <-
  housing %>% 
  mutate(crime_nn1 = nn_function(st_c(housing), st_c(crime), 1),
         crime_nn2 = nn_function(st_c(housing), st_c(crime), 2), 
         crime_nn3 = nn_function(st_c(housing), st_c(crime), 3), 
         crime_nn4 = nn_function(st_c(housing), st_c(crime), 4), 
         crime_nn5 = nn_function(st_c(housing), st_c(crime), 5))

##Public Facilities

park <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Properties_Managed_by_Parks_and_Recreation_Points.geojson")%>%
  st_transform('EPSG:26913')%>%
  select(NAME, PARKTYPE, SUBCOMMUNITY, geometry)

playground <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Playground_Sites_Points.geojson")%>%
  st_transform('EPSG:26913')%>%
  select(NAME, MAPLABEL, geometry)%>%
  na.omit()

housing <- housing%>%
  mutate(playground=nn_function(st_c(housing),st_c(playground),1))%>%
  mutate(park=nn_function(st_c(housing),st_c(park),1))

st_cast##use this for converting polygons to points


##School District

school <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/school.csv")%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

school <- st_intersection(school, Boulder_boundary)

housing$school_buffer =
  st_buffer(housing, 8000) %>% 
  aggregate(mutate(school, counter = 1),., sum) %>% 
  pull(counter)

housing <- housing%>%
  mutate(school=nn_function(st_c(housing),st_c(school),1))

#Bus Stop
bus_stop <-read.csv("bus_stop.csv")%>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

bus_stop <- st_intersection(bus_stop, Boulder_boundary) 


