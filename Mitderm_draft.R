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

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

##Housing Data

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
  select(-deedNum, -status_cd.y)

