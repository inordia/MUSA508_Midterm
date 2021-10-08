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


housing <- st_read("studentData.geojson", crs = 'ESRI:102254')%>%
  st_transform('EPSG:26913')

housing <- housing %>%
  select(-Stories, -UnitCount)

housing <- housing [!is.na(housing$price),]
housing <- housing [!is.na(housing$geometry),]

##Crime Data

crime <- st_read("Boulder_Police_Department_(BPD)_Offenses.geojson") %>%
  st_transform('EPSG:26913')

year <- c("2019","2020","2021")

crime <- filter(crime, Report_Year %in% year)

crime.sf <- crime%>%
  filter(IBRType != "All Other Offenses")%>%
  na.omit() %>% 
  dplyr::select(geometry) %>%
  st_as_sf(crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')%>%
  distinct()

##Crime Buffer

housing$crimes.Buffer =
  st_buffer(housing, 200) %>% 
  aggregate(mutate(crime.sf, counter = 1),., sum) %>% 
  pull(counter)

crime <- mutate(crime.sf,counter=1)
buffer<-st_buffer(housing, 200)
aggregate(crime,buffer,FUN=sum,na.rm=TRUE)
##Crime Nearest Neighbor Feature
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

st_c <- st_coordinates

## Plot assault density
ggplot() + geom_sf(data = Boulder_boundary, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(crime.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Crime, Blouder") +
  mapTheme()

##Public Facilities

park <- st_read("Properties_Managed_by_Parks_and_Recreation_Points.geojson")%>%
  st_transform('EPSG:26913')%>%
  select(NAME, PARKTYPE, SUBCOMMUNITY, geometry)

playground <- st_read("Playground_Sites_Points.geojson")%>%
  st_transform('EPSG:26913')%>%
  select(NAME, MAPLABEL, geometry)%>%
  na.omit()

housing <- housing%>%
  mutate(playground=nn_function(st_c(housing),st_c(playground),1))%>%
  mutate(playground=nn_function(st_c(housing),st_c(park),1))


##School District

school <- read.csv("school.csv")%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

school <- st_intersection(school, Boulder_boundary)   

#Bus Stop
bus_stop <-read.csv("bus_stop.csv")%>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

bus_stop <- st_intersection(bus_stop, Boulder_boundary) 

#median income by census tracts

