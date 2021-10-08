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

housing.city <- st_intersection(housing, Boulder_boundary)

replace.na ##CLEANING THE NAs

##Internal Characteristics
housing <- housing %>%
  mutate(age = 2021- EffectiveYear)%>%
  filter(price <= 8000000)%>%
  filter(nbrBedRoom < 10, carStorageSF < 3000)


st_drop_geometry(housing)%>%
  dplyr::select(price, TotalFinishedSF, age) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

st_drop_geometry(housing)%>%
  dplyr::select(price, bsmtSF, carStorageSF, nbrBedRoom, nbrRoomsNobath, nbrFullBaths) %>%
  filter(nbrBedRoom < 10, carStorageSF < 3000)%>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

st_drop_geometry(housing) %>% 
  dplyr::select(price,
                designCodeDscr,
                qualityCodeDscr,
                ConstCodeDscr) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

st_drop_geometry(housing) %>% 
  dplyr::select(price,
                HeatingDscr,
                ExtWallDscrPrim,
                IntWallDscr,
                Roof_CoverDscr) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#IntWall not good

##Feature Engineering: Internal Characteristics
housing <- 
  housing %>%
  mutate(nbrBedRoom.cat = case_when(
    nbrBedRoom >= 0 & nbrBedRoom < 4  ~ "Up to 3 Bedrooms",
    nbrBedRoom >= 4 & nbrBedRoom < 5  ~ "4 Bedrooms",
    nbrBedRoom > 4                    ~ "5+ Bedrooms"))

##Crime Data
  
crime <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Boulder_Police_Department_(BPD)_Offenses.geojson") %>%
  st_transform('EPSG:26913')

year <- c("2019","2020","2021")

crime <- filter(crime, Report_Year %in% year)

crime <- crime%>%
  filter(IBRType != "All Other Offenses")%>%
  select(-Special_District)%>%
  na.omit()

crime.sf <- crime%>%
  dplyr::select(geometry) %>%
  st_as_sf(crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')%>%
  distinct()

##Crime Buffer

housing$crimes.Buffer =
  st_buffer(housing, 200) %>% 
  aggregate(mutate(crime.sf, counter = 1),., sum) %>% 
  pull(counter)

##Crime Nearest Neighbor Feature

st_c <- st_coordinates

housing <-
  housing %>% 
  mutate(crime_nn1 = nn_function(st_c(housing), st_c(crime.sf), 1),
         crime_nn2 = nn_function(st_c(housing), st_c(crime.sf), 2), 
         crime_nn3 = nn_function(st_c(housing), st_c(crime.sf), 3), 
         crime_nn4 = nn_function(st_c(housing), st_c(crime.sf), 4), 
         crime_nn5 = nn_function(st_c(housing), st_c(crime.sf), 5))

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

school.sf <- school%>%
  dplyr::select(geometry) %>%
  st_as_sf(crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

housing$school_buffer =
  st_buffer(housing, 8000) %>% ##WHAT IS THE RIGHT PARAMETER
  aggregate(mutate(school.sf, counter = 1),., sum) %>% 
  pull(counter)

housing <- housing%>%
  mutate(school=nn_function(st_c(housing),st_c(school.sf),1))

#Bus Stop
bus_stop <-read.csv("bus_stop.csv")%>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

bus_stop <- st_intersection(bus_stop, Boulder_boundary) 

