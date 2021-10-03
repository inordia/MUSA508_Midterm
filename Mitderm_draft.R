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

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

##Housing Data

Boulder_boundary <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/City_of_Boulder_City_Limits.kml") %>%
  st_transform('EPSG:26916')%>%
  select(-Name, -Description)
Boulder_boundary <- st_union(Boulder_boundary)

parcel_account <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Account_Parcels.csv")
building <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Buildings.csv")
sale <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Sales.csv")
parcel_gis <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Parcels.geojson")%>%
  select(-OBJECTID, -PARCEL_NUM)%>%
  st_transform('EPSG:26916')

building <- filter(building, bldgClassDscr=="SINGLE FAM RES IMPROVEMENTS")
house <- building %>%
  left_join(.,sale, by=c("strap"="strap"))%>%
  left_join(.,parcel_account, by=c("strap"="strap"))%>%
  left_join(.,parcel_gis, by=c("Parcelno"="PARCEL_NO"))%>%
  st_sf()%>%
  select(-deedNum, -status_cd.y)%>%
  st_intersection(house,Boulder_boundary)

##Crime Data
  
crime <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Boulder_Police_Department_(BPD)_Offenses.geojson") %>%
  st_transform('EPSG:26916')%>%
  filter(crime, Report_Year=="2019")

##Public Facilities

park <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Properties_Managed_by_Parks_and_Recreation_Polygons.geojson")%>%
  st_transform('EPSG:26916')%>%
  select(NAME, PARKTYPE, SUBCOMMUNITY, geometry)



