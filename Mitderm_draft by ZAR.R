##Set Up

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

setwd("C:/Users/zheng/Desktop/MUSA508_Midterm")
Boulder_boundary <- st_read("City_of_Boulder_City_Limits.kml") %>%
  st_transform('EPSG:26913')%>%
  select(-Name, -Description)
Boulder_boundary <- st_union(Boulder_boundary)

parcel_account <- read.csv("Account_Parcels.csv")
building <- read.csv("Buildings.csv")
sale <- read.csv("Sales.csv")
parcel_gis <- st_read("Parcels.geojson")%>%
  select(-OBJECTID, -PARCEL_NUM)%>%
  st_transform('EPSG:26913')

building <- filter(building, bldgClassDscr=="SINGLE FAM RES IMPROVEMENTS")
house <- building %>%
  left_join(.,sale, by=c("strap"="strap"))%>%
  left_join(.,parcel_account, by=c("strap"="strap"))%>%
  left_join(.,parcel_gis, by=c("Parcelno"="PARCEL_NO"))%>%
  st_sf()%>%
  select(-deedNum, -status_cd.y)

house <- st_intersection(house, Boulder_boundary)                               

house_15_19 <- house
house_15_19$Tdate<-as.Date(house_15_19$Tdate,format='%m/%d/%Y')
house_15_19 <- house_15_19%>%
  filter(Tdate >= "2015-01-01" & Tdate <= "2020-01-01")

##Crime Data

crime <- st_read("Boulder_Police_Department_(BPD)_Offenses.geojson") %>%
  st_transform('EPSG:26913')

crime <- filter(crime, Report_Year=="2019")

crime <- crime%>%
  filter(IBRType != "All Other Offenses") %>% 
  filter(IBRType != "Society")%>%
  na.omit() %>% 
  distinct()

##Crime Buffer

house_15_19$crimes.Buffer =
  st_buffer(house_15_19, 660) %>% 
  aggregate(mutate(crime, counter = 1),., sum) %>% 
  pull(counter)
crime <- mutate(crime,counter=1)
buffer<-st_buffer(house_15_19,660)
aggregate(crime,buffer,FUN=sum)

##Crime Nearest Neighbor Feature

st_c <- st_coordinates

house_15_19 <-
  house_15_19 %>% 
  mutate(crime_nn1 = nn_function(st_c(house_15_19), st_c(crime), 1),
         crime_nn2 = nn_function(st_c(house_15_19), st_c(crime), 2), 
         crime_nn3 = nn_function(st_c(house_15_19), st_c(crime), 3), 
         crime_nn4 = nn_function(st_c(house_15_19), st_c(crime), 4), 
         crime_nn5 = nn_function(st_c(house_15_19), st_c(crime), 5))

##Public Facilities

park <- st_read("Properties_Managed_by_Parks_and_Recreation_Polygons.geojson")%>%
  st_transform('EPSG:26913')%>%
  select(NAME, PARKTYPE, SUBCOMMUNITY, geometry)

##School District

school <- read.csv("school.csv")%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

school <- st_intersection(school, Boulder_boundary) 

#bus stop
bus_stop <-read.csv("bus_stop.csv")%>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

bus_stop <- st_intersection(bus_stop, Boulder_boundary) 
